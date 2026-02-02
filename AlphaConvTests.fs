module mini_caml_fsharp.AlphaConvTests

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

type private test_case = { s_expr: string; expected_k_form: KNorm.t }

let private a_conv_tests: test_case list = [
    {
        s_expr = "(let x = 5 in x)"
        expected_k_form = KNorm.Let(
            ("x.1", Type.IntType),
            KNorm.Int 5,
            KNorm.Var "x.1"
        )
    }
    {
       s_expr = "(let x = #t in x)"
       expected_k_form = KNorm.Let(
           ("x.1", Type.IntType),
           KNorm.Int 1,
           KNorm.Var "x.1"
       )
    }
    {
        s_expr = "(let x = 123.456 in x)"
        expected_k_form = KNorm.Let(
            ("x.1", Type.FloatType),
            KNorm.Float 123.456,
            KNorm.Var "x.1"
        )
    }
    {
        s_expr = "(let x : (, i _) = (, 5 #t) in x)"
        expected_k_form = KNorm.Let(("x.3", Type.TupleType [Type.IntType; Type.IntType]),
            KNorm.Let(("Ti1.4", Type.IntType),
                KNorm.Int 5,
                KNorm.Let(("Ti2.5", Type.IntType), KNorm.Int 1, KNorm.Tuple(["Ti1.4"; "Ti2.5"]))
            ),
            KNorm.Var "x.3"
        )
    }
    {
        s_expr = "(let (, x y z) = (, 10 #t 42) in (if y then x else z))"
        expected_k_form = KNorm.Let(
            ("Tt4.6", Type.TupleType [Type.IntType; Type.IntType; Type.IntType]),
            KNorm.Let(("Ti1.7", Type.IntType), KNorm.Int 10,
                KNorm.Let(("Ti2.8", Type.IntType), KNorm.Int 1,
                    KNorm.Let(("Ti3.9", Type.IntType), KNorm.Int 42,
                        KNorm.Tuple ["Ti1.7"; "Ti2.8"; "Ti3.9"]))),
            KNorm.LetTuple(["x.10", Type.IntType; "y.11", Type.IntType; "z.12", Type.IntType], "Tt4.6",
                KNorm.Let(("Ti5.13", Type.IntType), KNorm.Int 0,
                    KNorm.BranchEq("y.11", "Ti5.13", KNorm.Var "z.12", KNorm.Var "x.10" ))))
    }
    {
        s_expr = "(let-rec (add x y) = (+ x y) in (add 12 30))"
        expected_k_form = KNorm.LetRec(
            {
                name = "add.3", Type.FunType([Type.IntType; Type.IntType], Type.IntType)
                args = ["x.4", Type.IntType; "y.5", Type.IntType]
                body = KNorm.Add("x.4", "y.5")
            },
            KNorm.Let(("Ti1.6", Type.IntType), KNorm.Int 12,
                KNorm.Let(("Ti2.7", Type.IntType), KNorm.Int 30,
                    KNorm.Apply("add.3", ["Ti1.6"; "Ti2.7"])))
        )
    }
]

[<Test>]
let testAlphaConversion () =
    for case in a_conv_tests do
        Id.reset ()
        let k_form =
            case.s_expr
            |> SExpr.parse
            |> Parsing.f
            |> Typing.f
            |> KNormalisation.f
            |> AlphaConv.f
        Assert.AreEqual(case.expected_k_form, k_form)