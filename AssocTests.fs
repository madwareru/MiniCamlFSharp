module mini_caml_fsharp.AssocTests

open Microsoft.FSharp.Core
open NUnit.Framework
open mini_caml_fsharp.SExpr
open mini_caml_fsharp.Id
open mini_caml_fsharp.Type
open mini_caml_fsharp.Parsing
open mini_caml_fsharp.Typing
open mini_caml_fsharp.KNorm
open mini_caml_fsharp.KNormalisation
open mini_caml_fsharp.Assoc

type private test_case = { s_expr: string; expected_k_form: KNorm.t }

let private assoc_tests: test_case list = [
    {
        s_expr = "(let x = (let y = (let z = 1 in (+ z 1)) in (+ y y)) in (- x))"
        expected_k_form =
            // На выходе плоская последовательность
            // z <- 1
            // Ti <- 1
            // y <- (+ z Ti)
            // x <- (+ y y)
            // (- x)
            KNorm.Let(("z", Type.IntType), KNorm.Int 1,
            KNorm.Let(("Ti1", Type.IntType), KNorm.Int 1,
            KNorm.Let(("y", Type.IntType), KNorm.Add("z", "Ti1"),
            KNorm.Let(("x", Type.IntType), KNorm.Add("y", "y"),
            KNorm.Neg "x"))))
    }
    {
        s_expr = "(let x = (let (, y z) = (, 10 32) in (+ y z)) in x)"
        expected_k_form =
            // На выходе плоская последовательность
            // Ti1 <- 10
            // Ti2 <- 32
            // Tt3 <- (, Ti1 Ti2)
            // (, y z) <- Tt3
            // x <- (+ y z)
            // x
            KNorm.Let(("Ti1", Type.IntType), KNorm.Int 10,
            KNorm.Let(("Ti2", Type.IntType), KNorm.Int 32,
            KNorm.Let(("Tt3", Type.TupleType [Type.IntType; Type.IntType]), KNorm.Tuple ["Ti1"; "Ti2"],
            KNorm.LetTuple(["y", Type.IntType; "z", Type.IntType], "Tt3",
            KNorm.Let(("x", Type.IntType), KNorm.Add("y", "z"),
            KNorm.Var "x")))))
    }
]

[<Test>]
let testAssocTransformation () =
    for case in assoc_tests do
        Id.reset ()
        let k_form =
            case.s_expr
            |> SExpr.parse
            |> Parsing.f
            |> Typing.f
            |> KNormalisation.f
            |> Assoc.f
        Assert.AreEqual(case.expected_k_form, k_form)
