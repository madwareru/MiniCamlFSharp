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
    {
        s_expr = "(let add' = (let-rec (add x y) = (+ x y) in add) in (add' 5 8))"
        expected_k_form = KNorm.LetRec(
            {
                name = ("add", Type.FunType([Type.IntType; Type.IntType], Type.IntType))
                args = ["x", Type.IntType; "y", Type.IntType]
                body = KNorm.Add("x", "y")
            },
            KNorm.Let(("add'", Type.FunType([Type.IntType; Type.IntType], Type.IntType)), KNorm.Var("add"),
            KNorm.Let(("Ti1", Type.IntType), KNorm.Int 5,
            KNorm.Let(("Ti2", Type.IntType), KNorm.Int 8,
            KNorm.Apply("add'", ["Ti1"; "Ti2"]))))
        )
    }
    {
        s_expr = @"
            (let a = 3 in
            (let b = 5 in
            (if (<= a b)
                then (let c = (let d = (let e = 1 in (+ e 1)) in (+ d d)) in (- c))
                else (let f = (let (, g h) = (, 10 32) in (+ g h)) in f))))
        "
        expected_k_form =
            KNorm.Let(("a", Type.IntType), KNorm.Int 3,
            KNorm.Let(("b", Type.IntType), KNorm.Int 5,
            KNorm.BranchLE("a", "b",
                KNorm.Let(("e", Type.IntType), KNorm.Int 1,
                KNorm.Let(("Ti1", Type.IntType), KNorm.Int 1,
                KNorm.Let(("d", Type.IntType), KNorm.Add("e", "Ti1"),
                KNorm.Let(("c", Type.IntType), KNorm.Add("d", "d"),
                KNorm.Neg "c")))),
                KNorm.Let(("Ti2", Type.IntType), KNorm.Int 10,
                KNorm.Let(("Ti3", Type.IntType), KNorm.Int 32,
                KNorm.Let(("Tt4", Type.TupleType [Type.IntType; Type.IntType]), KNorm.Tuple ["Ti2"; "Ti3"],
                KNorm.LetTuple(["g", Type.IntType; "h", Type.IntType], "Tt4",
                KNorm.Let(("f", Type.IntType), KNorm.Add("g", "h"),
                KNorm.Var "f"))))))))
    }
    {
        s_expr = @"
            (let a = 3 in
            (let b = 5 in
            (if (= a b)
                then (let c = (let d = (let e = 1 in (+ e 1)) in (+ d d)) in (- c))
                else (let f = (let (, g h) = (, 10 32) in (+ g h)) in f))))
        "
        expected_k_form =
            KNorm.Let(("a", Type.IntType), KNorm.Int 3,
            KNorm.Let(("b", Type.IntType), KNorm.Int 5,
            KNorm.BranchEq("a", "b",
                KNorm.Let(("e", Type.IntType), KNorm.Int 1,
                KNorm.Let(("Ti1", Type.IntType), KNorm.Int 1,
                KNorm.Let(("d", Type.IntType), KNorm.Add("e", "Ti1"),
                KNorm.Let(("c", Type.IntType), KNorm.Add("d", "d"),
                KNorm.Neg "c")))),
                KNorm.Let(("Ti2", Type.IntType), KNorm.Int 10,
                KNorm.Let(("Ti3", Type.IntType), KNorm.Int 32,
                KNorm.Let(("Tt4", Type.TupleType [Type.IntType; Type.IntType]), KNorm.Tuple ["Ti2"; "Ti3"],
                KNorm.LetTuple(["g", Type.IntType; "h", Type.IntType], "Tt4",
                KNorm.Let(("f", Type.IntType), KNorm.Add("g", "h"),
                KNorm.Var "f"))))))))
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
            |> Typing.f Typing.ProgramShouldNotReturnFunction
            |> KNormalisation.f
            |> Assoc.f
        Assert.AreEqual(case.expected_k_form, k_form)
