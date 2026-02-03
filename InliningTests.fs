module mini_caml_fsharp.InliningTests

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
open mini_caml_fsharp.KNormInterpreter

type private test_case = { s_expr: string; expected_k_form: KNorm.t }

let private inlining_tests: test_case list = [
    {
        s_expr = "(let-rec (add1 x) = (+ 1 x) in (add1 10))"
        // В результате подстановки получится
        // (let-rec (add1.3 x.4) = (let Ti2.5 = 1 in (+ Ti2.5 x.4)) in
        // (let Ti1.6 = 10 in
        // (let Ti2.5.7 = 1 in
        // (+ Ti2.5.7 Ti1.6)))),
        // при этом add1.3 по факту становится неиспользуемым
        expected_k_form = KNorm.LetRec(
            {
                name = "add1.3", Type.FunType([Type.IntType], Type.IntType)
                args = [("x.4", Type.IntType)]
                body = KNorm.Let(("Ti2.5", Type.IntType), KNorm.Int 1, KNorm.Add("Ti2.5", "x.4"))
            }, KNorm.Let(("Ti1.6", Type.IntType), KNorm.Int 10,
                KNorm.Let(("Ti2.5.7", Type.IntType), KNorm.Int 1, KNorm.Add("Ti2.5.7", "Ti1.6")))
        )
    }
]

[<Test>]
let testInlining() =
    for case in inlining_tests do
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
        Assert.AreNotEqual(k_form, converted)
        Assert.AreEqual(case.expected_k_form, converted)
        let result = k_form |> KNormInterpreter.f
        let result_after_conv = converted |> KNormInterpreter.f
        Assert.AreEqual(result, result_after_conv)
