module mini_caml_fsharp.CmmConvTests

open Microsoft.FSharp.Core
open NUnit.Framework
open mini_caml_fsharp.SExpr
open mini_caml_fsharp.Id
open mini_caml_fsharp.Parsing
open mini_caml_fsharp.Typing
open mini_caml_fsharp.KNormalisation
open mini_caml_fsharp.AlphaConv
open mini_caml_fsharp.ClosureRepresentationConv
open mini_caml_fsharp.Cmm
open mini_caml_fsharp.CmmConv

open Cmm
open mini_caml_fsharp.Type.Type

type private test_case = { s_expr: string; expected_program: program_t }

let private a_conv_tests: test_case list = [
    {
        s_expr = "(let x = 5 in x)"
        expected_program =
        {
            top_level_functions = []
            entry = Seq(Assignment(("x.1", IntType), Atom <| Int 5),
                    Return <| Var "x.1")
        }
    }
    {
        s_expr = "(let x : (, i _) = (, 5 #t) in x)"
        expected_program =
        {
            top_level_functions = []
            entry = Seq(Assignment(("Ti1.4", IntType), Atom <| Int 5),
                    Seq(Assignment(("Ti2.5", IntType), Atom <| Int 1),
                    Seq(Assignment(("Ti11", IntType), Atom <| Int 2),
                    Seq(Assignment(("Tt6", TupleType [IntType; IntType]),
                                   ApplyDirect(Id.L "min_caml_alloc_vector", ["Ti11"])),
                    Seq(Assignment(("Ti9", IntType), Atom <| Int 0),
                    Seq(Assignment(("Tu10", UnitType), MemoryPut ("Tt6", "Ti9", "Ti1.4")),
                    Seq(Assignment(("Ti7", IntType), Atom <| Int 1),
                    Seq(Assignment(("Tu8", UnitType), MemoryPut ("Tt6", "Ti7", "Ti2.5")),
                    Seq(Assignment(("x.3", TupleType [IntType; IntType]), Var "Tt6"),
                    Return <| Var "x.3")))))))))
        }
    }
    {
        s_expr = "(let (, x y) = (, 5 #t) in x)"
        expected_program =
        {
            top_level_functions = []
            entry = Seq(Assignment(("Ti1.5", IntType), Atom <| Int 5),
                    Seq(Assignment(("Ti2.6", IntType), Atom <| Int 1),
                    Seq(Assignment(("Ti14", IntType), Atom <| Int 2),
                    Seq(Assignment(("Tt9", TupleType [IntType; IntType]),
                                   ApplyDirect(Id.L "min_caml_alloc_vector", ["Ti14"])),
                    Seq(Assignment(("Ti12", IntType), Atom <| Int 0),
                    Seq(Assignment(("Tu13", UnitType), MemoryPut ("Tt9", "Ti12", "Ti1.5")),
                    Seq(Assignment(("Ti10", IntType), Atom <| Int 1),
                    Seq(Assignment(("Tu11", UnitType), MemoryPut ("Tt9", "Ti10", "Ti2.6")),
                    Seq(Assignment(("Tt3.4", TupleType [IntType; IntType]), Var "Tt9"),
                    Seq(Assignment(("Ti16", IntType), Atom <| Int 0),
                    Seq(Assignment(("x.7", IntType), MemoryGet ("Tt3.4", "Ti16")),
                    Seq(Assignment(("Ti15", IntType), Atom <| Int 1),
                    Seq(Assignment(("y.8", IntType), MemoryGet ("Tt3.4", "Ti15")),
                    Return <| Var "x.7")))))))))))))
        }
    }
]

[<Test>]
let testClosureToCmmConversion () =
    for case in a_conv_tests do
        Id.reset ()
        let program =
            case.s_expr
            |> SExpr.parse
            |> Parsing.f
            |> Typing.f
            |> KNormalisation.f
            |> AlphaConv.f
            |> ClosureRepresentationConv.f
            |> CmmConv.f
        Assert.AreEqual(case.expected_program, program)