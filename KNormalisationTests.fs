module mini_caml_fsharp.KNormalisationTests

open Microsoft.FSharp.Core
open NUnit.Framework
open mini_caml_fsharp.SExpr
open mini_caml_fsharp.Id
open mini_caml_fsharp.Type
open mini_caml_fsharp.Syntax
open mini_caml_fsharp.Parsing
open mini_caml_fsharp.Typing
open mini_caml_fsharp.KNorm
open mini_caml_fsharp.KNormalisation

type private test_case = { s_expr: string; expected_k_form: KNorm.t }

let private typing_tests: test_case list = [
    {
        s_expr = "(let x = 5 in x)"
        expected_k_form = KNorm.Let(
            ("x", Type.IntType),
            KNorm.Int 5,
            KNorm.Var "x"
        )
    }
    {
       s_expr = "(let x = #t in x)"
       expected_k_form = KNorm.Let(
           ("x", Type.IntType),
           KNorm.Int 1,
           KNorm.Var "x"
       )
    }
    {
        s_expr = "(let x = 123.456 in x)"
        expected_k_form = KNorm.Let(
            ("x", Type.FloatType),
            KNorm.Float 123.456,
            KNorm.Var "x"
        )
    }
    {
        s_expr = "(let x : (, i _) = (, 5 #t) in x)"
        expected_k_form = KNorm.Let(("x", Type.TupleType [Type.IntType; Type.IntType]),
            KNorm.Let(("Ti1", Type.IntType),
                KNorm.Int 5,
                KNorm.Let(("Ti2", Type.IntType), KNorm.Int 1, KNorm.Tuple(["Ti1"; "Ti2"]))
            ),
            KNorm.Var "x"
        )
    }
    {
        s_expr = "(let (, x y z) = (, 10 #t 42) in (if y then x else z))"
        expected_k_form = KNorm.Let(
            ("Tt4", Type.TupleType [Type.IntType; Type.IntType; Type.IntType]),
            KNorm.Let(("Ti1", Type.IntType), KNorm.Int 10,
                KNorm.Let(("Ti2", Type.IntType), KNorm.Int 1,
                    KNorm.Let(("Ti3", Type.IntType), KNorm.Int 42,
                        KNorm.Tuple ["Ti1"; "Ti2"; "Ti3"]))),
            KNorm.LetTuple(["x", Type.IntType; "y", Type.IntType; "z", Type.IntType], "Tt4",
                KNorm.Let(("Ti5", Type.IntType), KNorm.Int 0,
                    KNorm.BranchEq("y", "Ti5", KNorm.Var "z", KNorm.Var "x" ))))
    }
    // {
    //     s_expr = "(let-rec (add x y) = (+ x y) in (add 12 30))"
        // expected_syntax = Syntax.LetRecNode(
        //     {
        //         name = ("add", Type.FunType([Type.IntType; Type.IntType], Type.IntType))
        //         args = [("x", Type.IntType); ("y", Type.IntType)]
        //         body = Syntax.AddNode(Syntax.VarNode "x", Syntax.VarNode "y")
        //     }, Syntax.ApplyNode(Syntax.VarNode "add", [Syntax.IntNode 12; Syntax.IntNode 30])
        // )
    // }
    // {
    //     s_expr = "(let-rec (sub x y) : (_ _) -> _ = (- x y) in (sub 12 30))"
        // expected_syntax = Syntax.LetRecNode(
        //     {
        //         name = ("sub", Type.FunType([Type.IntType; Type.IntType], Type.IntType))
        //         args = [("x", Type.IntType); ("y", Type.IntType)]
        //         body = Syntax.SubNode(Syntax.VarNode "x", Syntax.VarNode "y")
        //     }, Syntax.ApplyNode(Syntax.VarNode "sub", [Syntax.IntNode 12; Syntax.IntNode 30])
        // )
    // }
    // {
    //     s_expr = @"
    //         (let-rec (fib x) =
    //             (if (<= x 1)
    //                 then 1
    //                 else (+ (fib (- x 1)) (fib (- x 2)))) in (fib 10))"
        // expected_syntax = Syntax.LetRecNode(
        //     let foo : Syntax.fun_def = {
        //         name = ("fib", Type.FunType([Type.IntType], Type.IntType))
        //         args = [("x", Type.IntType)]
        //         body = Syntax.IfNode(
        //             Syntax.LENode(Syntax.VarNode "x", Syntax.IntNode 1),
        //             Syntax.IntNode 1,
        //             Syntax.AddNode(
        //                 Syntax.ApplyNode(Syntax.VarNode "fib", [Syntax.SubNode(Syntax.VarNode "x", Syntax.IntNode 1)]),
        //                 Syntax.ApplyNode(Syntax.VarNode "fib", [Syntax.SubNode(Syntax.VarNode "x", Syntax.IntNode 2)])
        //             )
        //         )
        //     } in
        //     foo, Syntax.ApplyNode(Syntax.VarNode "fib", [Syntax.IntNode 10])
        // )
    // }
]


[<Test>]
let testKNormalisation () =
    for case in typing_tests do
        Id.counter <- 0
        let k_form =
            case.s_expr
            |> SExpr.parse
            |> Parsing.f
            |> Typing.f
            |> KNormalisation.f
        Assert.AreEqual(case.expected_k_form, k_form)
