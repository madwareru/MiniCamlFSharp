module mini_caml_fsharp.KOptimizationTests

open Microsoft.FSharp.Core
open NUnit.Framework
open mini_caml_fsharp.SExpr
open mini_caml_fsharp.Id
open mini_caml_fsharp.Type
open mini_caml_fsharp.Parsing
open mini_caml_fsharp.Typing
open mini_caml_fsharp.KNorm
open mini_caml_fsharp.KNormalisation
open mini_caml_fsharp.AlphaConv
open mini_caml_fsharp.BetaReduction
open mini_caml_fsharp.Assoc
open mini_caml_fsharp.Inlining
open mini_caml_fsharp.ConstFolding
open mini_caml_fsharp.Elimination
open mini_caml_fsharp.KNormInterpreter

type private test_case = { s_expr: string; expected_k_form: KNorm.t }

let private tests: test_case list = [
    {
        s_expr = "(let x = 7 in x)"
        expected_k_form = KNorm.Int 7
    }
    {
        s_expr = "(let x = 7.0 in x)"
        expected_k_form = KNorm.Float 7.0
    }
    {
        // В данном случае уничтожается промежуточное объявление кортежа,
        // но дальше уже не соптимизировать
        s_expr = "(let x = (, 7.0 7) in x)"
        expected_k_form =
            KNorm.Let (("Td1.4", Type.FloatType), KNorm.Float 7.0,
            KNorm.Let (("Ti2.5", Type.IntType), KNorm.Int 7,
            KNorm.Tuple [ "Td1.4"; "Ti2.5" ]))
    }
    {
        s_expr = "(+ 12 30)"
        expected_k_form = KNorm.Int 42
    }
    {
        s_expr = "(let x = (let y = (let z = 1 in (+ z 1)) in (+ y y)) in (- x))"
        expected_k_form = KNorm.Int -4
    }
    {
        s_expr = "(let x = 5 in (let y = 10 in (if (<= x y) then 42 else 13)))"
        expected_k_form = KNorm.Int 42
    }
    {
        s_expr = "(let x = 5 in (let y = 10 in (if (= x y) then 42 else 13)))"
        expected_k_form = KNorm.Int 13
    }
    {
        s_expr = "(let (, x y) = (, 5 5) in (if (= x y) then 42 else 13))"
        expected_k_form = KNorm.Int 42
    }
    {
        s_expr = "(let (, x y) = (, 5 5) in (if (<> x y) then 42 else 13))"
        expected_k_form = KNorm.Int 13
    }
    {
        s_expr = @"
            (let-rec (add3 x y z) = (+ x y z) in (add3 1 2 3))
        "
        expected_k_form = KNorm.Int 6
    }
    {
        s_expr = @"
            (let-rec (add3 x y z) = (+ x y z) in
            (add3 (add3 1 2 3) (add3 1 2 3) (add3 1 2 3)))
        "
        expected_k_form = KNorm.Int 18
    }
]

[<Test>]
let testKOptimizations () =
    let limit = 100
    let rec iter n e =
        printfn $"iteration %d{n}."
        match n with
        | 0 -> e
        | _ ->
            let e' = e |> BetaReduction.f
            let e' = e' |> Assoc.f
            let e' = e' |> Inlining.f 45
            let e' = e' |> ConstFolding.f
            let e' = e' |> Elimination.f
            if e = e' then e' else iter (n - 1) e'

    for case in tests do
        Id.reset ()
        let k_form =
            case.s_expr
            |> SExpr.parse
            |> Parsing.f
            |> Typing.f Typing.ProgramShouldNotReturnFunction
            |> KNormalisation.f
            |> AlphaConv.f

        let converted = (limit, k_form) ||> iter
        Assert.AreNotEqual(k_form, converted)
        Assert.AreEqual(case.expected_k_form, converted)
        let res = k_form |> KNormInterpreter.f
        let res' = converted |> KNormInterpreter.f
        Assert.AreEqual(res, res')
