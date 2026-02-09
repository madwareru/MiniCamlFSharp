module mini_caml_fsharp.BetaReductionTests

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

type private test_case = { s_expr: string; expected_k_form: KNorm.t }

let private beta_reduce_tests: test_case list = [
    {
        s_expr = "(let x = 5 in x)"
        expected_k_form = KNorm.Let(
            ("x.1", Type.IntType),
            KNorm.Int 5,
            KNorm.Var "x.1"
        )
    }
    {
        s_expr = "(let x = 5 in (- x))"
        expected_k_form = KNorm.Let(
            ("x.1", Type.IntType),
            KNorm.Int 5,
            KNorm.Neg "x.1"
        )
    }
    {
        s_expr = "(let x = #t in (let y = x in y))"
        expected_k_form = KNorm.Let(
            ("x.1", Type.IntType),
            KNorm.Int 1,
            KNorm.Var "x.1"
       )
    }
    {
        s_expr = "(let x = #t in (let y = x in (not y)))"
        expected_k_form = KNorm.Let(("x.2", Type.IntType),
            KNorm.Int 1,
            KNorm.Let(("Ti1.4", Type.IntType), KNorm.Int 0,
                KNorm.BranchEq("x.2", "Ti1.4", KNorm.Int 1, KNorm.Int 0)
            )
       )
    }
    {
        s_expr = "(let x = 5 in (let y = x in (let z = y in (+ z x))))"
        expected_k_form = KNorm.Let(
            ("x.1", Type.IntType),
            KNorm.Int 5,
            KNorm.Add("x.1", "x.1")
        )
    }
    {
        s_expr = "(let x = 5 in (let y = x in (let z = y in (- z x))))"
        expected_k_form = KNorm.Let(
            ("x.1", Type.IntType),
            KNorm.Int 5,
            KNorm.Sub("x.1", "x.1")
        )
    }
    {
        s_expr = "(let x = 5.0 in (let y = x in (let z = y in (+. z x))))"
        expected_k_form = KNorm.Let(
            ("x.1", Type.FloatType),
            KNorm.Float 5.0,
            KNorm.FAdd("x.1", "x.1")
        )
    }
    {
        s_expr = "(let x = 5. in (let y = x in (let z = y in (-. z x))))"
        expected_k_form = KNorm.Let(
            ("x.1", Type.FloatType),
            KNorm.Float 5.0,
            KNorm.FSub("x.1", "x.1")
        )
    }
    {
        s_expr = "(let x = 5.0 in (let y = x in (let z = y in (*. z x))))"
        expected_k_form = KNorm.Let(
            ("x.1", Type.FloatType),
            KNorm.Float 5.0,
            KNorm.FMul("x.1", "x.1")
        )
    }
    {
        s_expr = "(let x = 5. in (let y = x in (let z = y in (/. z x))))"
        expected_k_form = KNorm.Let(
            ("x.1", Type.FloatType),
            KNorm.Float 5.0,
            KNorm.FDiv("x.1", "x.1")
        )
    }
]

[<Test>]
let testBetaReductionConversion () =
    for case in beta_reduce_tests do
        Id.reset ()
        let k_form =
            case.s_expr
            |> SExpr.parse
            |> Parsing.f
            |> Typing.f Typing.ProgramShouldNotReturnFunction
            |> KNormalisation.f
            |> AlphaConv.f
            |> BetaReduction.f
        Assert.AreEqual(case.expected_k_form, k_form)
