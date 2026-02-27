module mini_caml_fsharp_core.ClosureRepresentationConvTests

open Microsoft.FSharp.Core
open NUnit.Framework
open mini_caml_fsharp_core.SExpr
open mini_caml_fsharp_core.Id
open mini_caml_fsharp_core.Parsing
open mini_caml_fsharp_core.Typing
open mini_caml_fsharp_core.KNormalisation
open mini_caml_fsharp_core.AlphaConv
open mini_caml_fsharp_core.ClosureRepresentation
open mini_caml_fsharp_core.ClosureRepresentationConv

open ClosureRepresentation
open mini_caml_fsharp_core.Type.Type

type private test_case = { s_expr: string; expected_program: program }

let private a_conv_tests: test_case list = [
    {
        s_expr = "(let x = 5 in x)"
        expected_program =
        {
            top_level_functions = []
            main =
                Let(("x.1", IntType), Int 5,
                Var "x.1")
        }
    }
    {
       s_expr = "(let x = #t in x)"
       expected_program =
       {
           top_level_functions = []
           main =
                Let(("x.1", IntType), Int 1,
                Var "x.1")
       }
    }
    {
       s_expr = "(let x = 123.456 in x)"
       expected_program =
       {
           top_level_functions = []
           main =
                Let(("x.1", FloatType), Float 123.456,
                Var "x.1")
       }
    }
    {
        s_expr = "(let x : (, i _) = (, 5 #t) in x)"
        expected_program =
        {
            top_level_functions = []
            main =
                Let(("x.3", TupleType [IntType; IntType]),
                    Let(("Ti1.4", IntType), Int 5,
                    Let(("Ti2.5", IntType), Int 1,
                    Tuple(["Ti1.4"; "Ti2.5"]))
                ),
                Var "x.3"
            )
        }
    }
    {
        s_expr = "(let (, x y z) = (, 10 #t 42) in (if y then x else z))"
        expected_program =
        {
            top_level_functions = []
            main =
                Let(("Tt4.6", TupleType [IntType; IntType; IntType]),
                    Let(("Ti1.7", IntType), Int 10,
                    Let(("Ti2.8", IntType), Int 1,
                    Let(("Ti3.9", IntType), Int 42,
                    Tuple ["Ti1.7"; "Ti2.8"; "Ti3.9"]))
                ),
                LetTuple(["x.10", IntType; "y.11", IntType; "z.12", IntType], "Tt4.6",
                Let(("Ti5.13", IntType), Int 0,
                BranchEq("y.11", "Ti5.13", Var "z.12", Var "x.10" ))))
        }
    }
    {
        s_expr = "(let-rec (add x y) = (+ x y) in (add 12 30))"
        expected_program =
        {
            top_level_functions = [
                {
                    name = "add.3" |> Id.L, FunType([IntType; IntType], IntType)
                    args = ["x.4", IntType; "y.5", IntType]
                    free_vars = []
                    is_closure = false
                    body = Add("x.4", "y.5")
                }
            ]
            main =
                Let(("Ti1.6", IntType), Int 12,
                Let(("Ti2.7", IntType), Int 30,
                ApplyDirect("add.3" |> Id.L, ["Ti1.6"; "Ti2.7"]))
            )
        }
    }
    {
        s_expr = "(let-rec (add x y) = (+ x y) in ())"
        expected_program =
        {
            top_level_functions = [
                {
                    name = "add.1" |> Id.L, FunType([IntType; IntType], IntType)
                    args = ["x.2", IntType; "y.3", IntType]
                    free_vars = []
                    is_closure = false
                    body = Add("x.2", "y.3")
                }
            ]
            main = Unit
        }
    }
    {
        s_expr = "(let z = 1990 in (let-rec (add x y) = (+ x y z) in (add 12 30)))"
        expected_program =
        {
            top_level_functions = [
                {
                    name = "add.5" |> Id.L, FunType([IntType; IntType], IntType)
                    args = ["x.6", IntType; "y.7", IntType]
                    free_vars = ["z.4", IntType]
                    is_closure = true
                    body =
                        Let (("Ti3.8", IntType), Add ("x.6", "y.7"),
                        Add ("Ti3.8", "z.4"))
                }
            ]
            main =
                Let(("z.4", IntType), Int 1990,
                LetClosure(("add.5", FunType ([IntType; IntType], IntType)), Id.L "add.5",
                Let(("Ti1.9", IntType), Int 12,
                Let(("Ti2.10", IntType), Int 30,
                ApplyClosure ("add.5", ["Ti1.9"; "Ti2.10"])))))
        }
    }
    {
        s_expr = @"
            (let-rec (fact x) =
                (if (<= x 1.0)
                    then 1.0
                    else (*. x (fact (-. x 1.0 )))) in (fact 6.0))
        "
        expected_program =
        {
            top_level_functions = [
                {
                    name = ("fact.6" |> Id.L, FunType ([FloatType], FloatType))
                    args = [("x.7", FloatType)]
                    free_vars = []
                    is_closure = false
                    body =
                        Let(("Td2.8", FloatType), Float 1.0,
                        BranchLE("x.7", "Td2.8",
                            Float 1.0,
                            Let(("Td5.9", FloatType),
                                Let(("Td4.10", FloatType),
                                    Let(("Td3.11", FloatType), Float 1.0,
                                    FSub ("x.7", "Td3.11")
                                ),
                                ApplyDirect (Id.L "fact.6", ["Td4.10"])
                            ), FMul ("x.7", "Td5.9"))))
                }
            ]
            main =
                Let(("Td1.12", FloatType), Float 6.0,
                ApplyDirect("fact.6" |> Id.L, ["Td1.12"]))
        }
    }
    {
        s_expr = @"
            (let arr : ([] i) = (new[] 0 2) in
                (;
                (set[] arr 0 <- 10)
                (set[] arr 1 <- 20)
                (+
                    (get[] arr 0)
                    (get[] arr 1))))
        "
        expected_program =
        {
            top_level_functions = []
            main =
                Let(("arr.11", ArrayType IntType),
                    Let(("Ti1.12", IntType), Int 2,
                    Let(("Ti2.13", IntType), Int 0,
                    ApplyDirect (Id.L "min_caml_create_array", ["Ti1.12"; "Ti2.13"]))
                ),
                Let(("_.14", UnitType),
                    Let(("Ti3.15", IntType), Int 0,
                    Let(("Ti4.16", IntType), Int 10,
                    Put ("arr.11", "Ti3.15", "Ti4.16"))
                ),
                Let(("_.17", UnitType), 
                    Let(("Ti5.18", IntType), Int 1,
                    Let(("Ti6.19", IntType), Int 20,
                    Put ("arr.11", "Ti5.18", "Ti6.19"))
                ),
                Let(("Ti8.20", IntType), 
                    Let (("Ti7.21", IntType), Int 0,
                    Get ("arr.11", "Ti7.21")
                ),
                Let(("Ti10.22", IntType), 
                    Let (("Ti9.23", IntType), Int 1,
                    Get ("arr.11", "Ti9.23")
                ),
                Add ("Ti8.20", "Ti10.22"))))))
        }
    }
]

[<Test>]
let testKNormToClosureConversion () =
    for case in a_conv_tests do
        Id.reset ()
        let program =
            case.s_expr
            |> SExpr.parse
            |> Parsing.f
            |> Typing.f Typing.ProgramShouldNotReturnFunction
            |> KNormalisation.f
            |> AlphaConv.f
            |> ClosureRepresentationConv.f
        Assert.AreEqual(case.expected_program, program)
