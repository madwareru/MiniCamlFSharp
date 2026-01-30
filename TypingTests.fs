module mini_caml_fsharp.TypingTests

open System
open Microsoft.FSharp.Core
open NUnit.Framework
open mini_caml_fsharp.SExpr
open mini_caml_fsharp.Type
open mini_caml_fsharp.Syntax
open mini_caml_fsharp.Parsing
open mini_caml_fsharp.Typing

type private test_case = { s_expr: string; expected_syntax: Syntax.t }

let private typing_tests: test_case list = [
    {
        s_expr = "(let x = 5 in x)"
        expected_syntax = Syntax.LetNode(
            ("x", Type.IntType),
            Syntax.IntNode 5,
            Syntax.VarNode "x"
        )
    }
    {
        s_expr = "(let x = #t in x)"
        expected_syntax = Syntax.LetNode(
            ("x", Type.BoolType),
            Syntax.BoolNode true,
            Syntax.VarNode "x"
        )
    }
    {
        s_expr = "(let x = 123.456 in x)"
        expected_syntax = Syntax.LetNode(
            ("x", Type.FloatType),
            Syntax.FloatNode 123.456,
            Syntax.VarNode "x"
        )
    }
    {
        s_expr = "(let x = (, 1 #t) in x)"
        expected_syntax = Syntax.LetNode(
            ("x", Type.TupleType [Type.IntType; Type.BoolType]),
            Syntax.TupleNode([ Syntax.IntNode 1; Syntax.BoolNode true ]),
            Syntax.VarNode "x"
        )
    }
    {
        s_expr = "(let (, x y z) = (, 1 #t 42) in (if y then x else z))"
        expected_syntax = Syntax.LetTuple(
            [
                ("x", Type.IntType)
                ("y", Type.BoolType)
                ("z", Type.IntType)
            ],
            Syntax.TupleNode([ Syntax.IntNode 1; Syntax.BoolNode true; Syntax.IntNode 42 ]),
            Syntax.IfNode(
                Syntax.VarNode "y",
                Syntax.VarNode "x",
                Syntax.VarNode "z"
            )
        )
    }
    // {
    //     s_expr = @"
    //         (let-rec (fib x) =
    //             (if (<= x 1)
    //                 then 1
    //                 else (+ (fib (- x 1)) (fib (- x 2)))) in (fib 10))"
    //     expected_syntax = Syntax.LetRecNode(
    //         let foo : Syntax.fun_def = {
    //             name = ("fib", Type.FunType([Type.IntType], Type.IntType))
    //             args = [("x", Type.IntType)]
    //             body = Syntax.IfNode(
    //                 Syntax.LENode(Syntax.VarNode "x", Syntax.IntNode 1),
    //                 Syntax.IntNode 1,
    //                 Syntax.AddNode(
    //                     Syntax.ApplyNode(Syntax.VarNode "fib", [Syntax.SubNode(Syntax.VarNode "x", Syntax.IntNode 1)]),
    //                     Syntax.ApplyNode(Syntax.VarNode "fib", [Syntax.SubNode(Syntax.VarNode "x", Syntax.IntNode 2)])
    //                 )
    //             )
    //         } in
    //         foo, Syntax.ApplyNode(Syntax.VarNode "fib", [Syntax.IntNode 10])
    //     )
    // }
]


[<Test>]
let testParsingSExprToSyntax () =
    for case in typing_tests do
        let parsed_syntax =
            case.s_expr
            |> SExpr.parse
            |> Parsing.f
            |> Typing.f
        Assert.AreEqual(case.expected_syntax, parsed_syntax)