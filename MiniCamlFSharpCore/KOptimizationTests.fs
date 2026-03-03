module mini_caml_fsharp_core.KOptimizationTests

open Microsoft.FSharp.Core
open NUnit.Framework
open mini_caml_fsharp_core.SExpr
open mini_caml_fsharp_core.Id
open mini_caml_fsharp_core.Type
open mini_caml_fsharp_core.Parsing
open mini_caml_fsharp_core.Typing
open mini_caml_fsharp_core.KNorm
open mini_caml_fsharp_core.KNormalisation
open mini_caml_fsharp_core.AlphaConv
open mini_caml_fsharp_core.BetaReduction
open mini_caml_fsharp_core.Assoc
open mini_caml_fsharp_core.Inlining
open mini_caml_fsharp_core.ConstFolding
open mini_caml_fsharp_core.Elimination
open mini_caml_fsharp_core.CommonSubElim
open mini_caml_fsharp_core.KNormInterpreter

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
    {
        // В цепочках сложений константы сворачиваются корректно
        s_expr = "(let x = (read_int ()) in (+ 5 6 x))"
        expected_k_form =
            KNorm.Let(("Tu1.6", Type.UnitType), KNorm.Unit,
            KNorm.Let(("x.5", Type.IntType), KNorm.ExtFunApply ("read_int", ["Tu1.6"]),
            KNorm.Let (("Ti4.7", Type.IntType), KNorm.Int 11,
            KNorm.Add ("Ti4.7", "x.5"))))
    }
    {
        // В цепочках сложений константы сворачиваются корректно
        s_expr = "(let x = (read_int ()) in (+ 5 x 6))"
        expected_k_form =
            KNorm.Let(("Tu1.6", Type.UnitType), KNorm.Unit,
            KNorm.Let(("x.5", Type.IntType), KNorm.ExtFunApply ("read_int", ["Tu1.6"]),
            KNorm.Let (("Ti10", Type.IntType), KNorm.Int 11,
            KNorm.Add ("Ti10", "x.5"))))
    }
    {
        // В цепочках сложений константы сворачиваются корректно
        s_expr = "(let x = (read_int ()) in (+ x 5 6))"
        expected_k_form =
            KNorm.Let(("Tu1.6", Type.UnitType), KNorm.Unit,
            KNorm.Let(("x.5", Type.IntType), KNorm.ExtFunApply ("read_int", ["Tu1.6"]),
            KNorm.Let (("Ti10", Type.IntType), KNorm.Int 11,
            KNorm.Add ("Ti10", "x.5"))))
    }
    {
        // В цепочках сложений константы сворачиваются корректно,
        // литералы () сворачиваются до единственной переменной
        s_expr = "(let x = (read_int ()) in (let y = (read_int ()) in (+ x y 5 6)))"
        expected_k_form =
            KNorm.Let(("Tu1.8", Type.UnitType), KNorm.Unit,
            KNorm.Let(("x.7", Type.IntType), KNorm.ExtFunApply ("read_int", ["Tu1.8"]),
            KNorm.Let(("y.9", Type.IntType), KNorm.ExtFunApply ("read_int", ["Tu1.8"]),
            KNorm.Let(("Ti3.12", Type.IntType), KNorm.Add ("x.7", "y.9"),
            KNorm.Let (("Ti15", Type.IntType), KNorm.Int 11,
            KNorm.Add ("Ti15", "Ti3.12"))))))
    }
    {
        // Коммутативное повторяющееся подвыражение
        s_expr = "(let x = (read_int ()) in (+ (+ x 5) (+ 5 x)))"
        expected_k_form =
            KNorm.Let(("Tu1.7", Type.UnitType), KNorm.Unit,
            KNorm.Let(("x.6", Type.IntType), KNorm.ExtFunApply ("read_int", ["Tu1.7"]),
            KNorm.Let(("Ti2.9", Type.IntType), KNorm.Int 5,
            KNorm.Let (("Ti3.8", Type.IntType), KNorm.Add ("Ti2.9", "x.6"),
            KNorm.Add ("Ti3.8", "Ti3.8")))))
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
            // После удаления мёртвого кода пробуем удалить
            // повторяющиеся подвыражения, после чего повторяем
            // удаление мёртвого кода ещё раз
            let e' = e' |> CommonSubElim.f
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
