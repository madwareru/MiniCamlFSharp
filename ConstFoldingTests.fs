module mini_caml_fsharp.ConstFoldingTests

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
open mini_caml_fsharp.KNormInterpreter

type private test_case = { s_expr: string; expected_k_form: KNorm.t }

let private tests: test_case list = [
    {
        s_expr = "(let x = 7 in x)"
        expected_k_form =
            KNorm.Let (("x.1", Type.IntType), KNorm.Int 7,
            KNorm.Int 7)
    }
    {
        s_expr = "(let x = 7.0 in x)"
        expected_k_form =
            KNorm.Let (("x.1", Type.FloatType), KNorm.Float 7.0,
            KNorm.Float 7.0)
    }
    {
        s_expr = "(let x = (, 7.0 7) in x)"
        expected_k_form =
            KNorm.Let (("Td1.4", Type.FloatType), KNorm.Float 7.0,
            KNorm.Let (("Ti2.5", Type.IntType), KNorm.Int 7,
            KNorm.Let (("x.3", Type.TupleType [ Type.FloatType; Type.IntType]),
            KNorm.Tuple [ "Td1.4"; "Ti2.5" ],
            KNorm.Tuple [ "Td1.4"; "Ti2.5" ])))
    }
    {
        s_expr = "(+ 12 30)"
        expected_k_form =
            KNorm.Let (("Ti1.3", Type.IntType), KNorm.Int 12,
            KNorm.Let (("Ti2.4", Type.IntType), KNorm.Int 30,
            KNorm.Int 42))
    }
    {
        s_expr = "(- 30 12)"
        expected_k_form =
            KNorm.Let (("Ti1.3", Type.IntType), KNorm.Int 30,
            KNorm.Let (("Ti2.4", Type.IntType), KNorm.Int 12,
            KNorm.Int 18))
    }
    {
        s_expr = "(+. 12.0 30.0)"
        expected_k_form =
            KNorm.Let (("Td1.3", Type.FloatType), KNorm.Float 12.0,
            KNorm.Let (("Td2.4", Type.FloatType), KNorm.Float 30.0,
            KNorm.Float 42.0))
    }
    {
        s_expr = "(-. 30.0 12.0)"
        expected_k_form =
            KNorm.Let (("Td1.3", Type.FloatType), KNorm.Float 30.0,
            KNorm.Let (("Td2.4", Type.FloatType), KNorm.Float 12.0,
            KNorm.Float 18.0))
    }
    {
        s_expr = "(*. 12.0 30.0)"
        expected_k_form =
            KNorm.Let (("Td1.3", Type.FloatType), KNorm.Float 12.0,
            KNorm.Let (("Td2.4", Type.FloatType), KNorm.Float 30.0,
            KNorm.Float 360.0))
    }
    {
        s_expr = "(/. 30.0 5.0)"
        expected_k_form =
            KNorm.Let (("Td1.3", Type.FloatType), KNorm.Float 30.0,
            KNorm.Let (("Td2.4", Type.FloatType), KNorm.Float 5.0,
            KNorm.Float 6.0))
    }
    {
        s_expr = "(let x = (let y = (let z = 1 in (+ z 1)) in (+ y y)) in (- x))"
        expected_k_form =
            KNorm.Let (("z.4", Type.IntType), KNorm.Int 1,
            KNorm.Let (("Ti1.5", Type.IntType), KNorm.Int 1,
            KNorm.Let (("y.3", Type.IntType), KNorm.Int 2,
            KNorm.Let (("x.2", Type.IntType), KNorm.Int 4,
            KNorm.Int -4))))
    }
    {
        s_expr = "(let x = 5 in (let y = 10 in (if (<= x y) then 42 else 13)))"
        expected_k_form =
            KNorm.Let (("x.1", Type.IntType), KNorm.Int 5,
            KNorm.Let (("y.2", Type.IntType), KNorm.Int 10,
            KNorm.Int 42))
    }
    {
        s_expr = "(let x = 5 in (let y = 10 in (if (<= y x) then 42 else 13)))"
        expected_k_form =
            KNorm.Let (("x.1", Type.IntType), KNorm.Int 5,
            KNorm.Let (("y.2", Type.IntType), KNorm.Int 10,
            KNorm.Int 13))
    }
    {
        s_expr = "(let x = 5 in (let y = 10 in (if (<> x y) then 42 else 13)))"
        expected_k_form =
            KNorm.Let (("x.1", Type.IntType), KNorm.Int 5,
            KNorm.Let (("y.2", Type.IntType), KNorm.Int 10,
            KNorm.Int 42))
    }
    {
        s_expr = "(let x = 5 in (let y = 10 in (if (= x y) then 42 else 13)))"
        expected_k_form =
            KNorm.Let (("x.1", Type.IntType), KNorm.Int 5,
            KNorm.Let (("y.2", Type.IntType), KNorm.Int 10,
            KNorm.Int 13))
    }
    {
        s_expr = "(let x = 5.0 in (let y = 10.0 in (if (<= x y) then 42 else 13)))"
        expected_k_form =
            KNorm.Let (("x.1", Type.FloatType), KNorm.Float 5.0,
            KNorm.Let (("y.2", Type.FloatType), KNorm.Float 10.0,
            KNorm.Int 42))
    }
    {
        s_expr = "(let x = 5.0 in (let y = 10.0 in (if (<= y x) then 42 else 13)))"
        expected_k_form =
            KNorm.Let (("x.1", Type.FloatType), KNorm.Float 5,
            KNorm.Let (("y.2", Type.FloatType), KNorm.Float 10,
            KNorm.Int 13))
    }
    {
        s_expr = "(let x = 5.0 in (let y = 10.0 in (if (<> x y) then 42 else 13)))"
        expected_k_form =
            KNorm.Let (("x.1", Type.FloatType), KNorm.Float 5.0,
            KNorm.Let (("y.2", Type.FloatType), KNorm.Float 10.0,
            KNorm.Int 42))
    }
    {
        s_expr = "(let x = 5.0 in (let y = 10.0 in (if (= x y) then 42 else 13)))"
        expected_k_form =
            KNorm.Let (("x.1", Type.FloatType), KNorm.Float 5.0,
            KNorm.Let (("y.2", Type.FloatType), KNorm.Float 10.0,
            KNorm.Int 13))
    }
    {
        s_expr = "(let (, x y) = (, 5 5) in (if (= x y) then 42 else 13))"
        expected_k_form =
            KNorm.Let(("Ti1.5", Type.IntType), KNorm.Int 5L,
            KNorm.Let(("Ti2.6", Type.IntType), KNorm.Int 5L,
            KNorm.Let(("Tt3.4", Type.TupleType [Type.IntType; Type.IntType]),
                KNorm.Tuple ["Ti1.5"; "Ti2.6"],
                // LetTuple развёрнут в пару из Let
                KNorm.Let(("y.8", Type.IntType), KNorm.Var "Ti2.6",
                KNorm.Let(("x.7", Type.IntType), KNorm.Var "Ti1.5",
                KNorm.BranchEq ("x.7", "y.8", KNorm.Int 42L, KNorm.Int 13L))))))
    }
]

[<Test>]
let testConstFold () =
    for case in tests do
        Id.reset ()
        let k_form =
            case.s_expr
            |> SExpr.parse
            |> Parsing.f
            |> Typing.f
            |> KNormalisation.f

        let converted =
            k_form
            |> AlphaConv.f
            |> BetaReduction.f
            |> Assoc.f
            |> Inlining.f 45
            |> ConstFolding.f
        Assert.AreNotEqual(k_form, converted)
        Assert.AreEqual(case.expected_k_form, converted)
        let res = k_form |> KNormInterpreter.f
        let res' = converted |> KNormInterpreter.f
        Assert.AreEqual(res, res')
