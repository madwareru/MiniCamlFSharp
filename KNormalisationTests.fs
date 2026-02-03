module mini_caml_fsharp.KNormalisationTests

open Microsoft.FSharp.Core
open NUnit.Framework
open mini_caml_fsharp.SExpr
open mini_caml_fsharp.Id
open mini_caml_fsharp.Type
open mini_caml_fsharp.Parsing
open mini_caml_fsharp.Typing
open mini_caml_fsharp.KNorm
open mini_caml_fsharp.KNormalisation

type private test_case = { s_expr: string; expected_k_form: KNorm.t }

let private k_norm_tests: test_case list = [
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
        // код аналогичен предыдущему тесту, но изначально K-нормализован
        s_expr = "(let x = (let Ti1 = 5 in (let Ti2 = 1 in (, Ti1 Ti2))) in x)"
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
    {
        // код аналогичен предыдущему тесту, но изначально K-нормализован
        s_expr = @"
            (let Tt4 = (let Ti1 = 10 in (let Ti2 = 1 in (let Ti3 = 42 in (, Ti1 Ti2 Ti3)))) in
                (let (, x y z) = Tt4 in (let Ti5 = 0 in (if (= y Ti5) then z else x))))
        "
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
    {
        s_expr = "(let-rec (add x y) = (+ x y) in (add 12 30))"
        expected_k_form = KNorm.LetRec(
            {
                name = "add", Type.FunType([Type.IntType; Type.IntType], Type.IntType)
                args = ["x", Type.IntType; "y", Type.IntType]
                body = KNorm.Add("x", "y")
            },
            KNorm.Let(("Ti1", Type.IntType), KNorm.Int 12,
                KNorm.Let(("Ti2", Type.IntType), KNorm.Int 30,
                    KNorm.Apply("add", ["Ti1"; "Ti2"])))
        )
    }
    {
        // код аналогичен предыдущему тесту, но изначально K-нормализован
        s_expr = "(let-rec (add x y) = (+ x y) in (let Ti1 = 12 in (let Ti2 = 30 in (add Ti1 Ti2))))"
        expected_k_form = KNorm.LetRec(
            {
                name = "add", Type.FunType([Type.IntType; Type.IntType], Type.IntType)
                args = ["x", Type.IntType; "y", Type.IntType]
                body = KNorm.Add("x", "y")
            },
            KNorm.Let(("Ti1", Type.IntType), KNorm.Int 12,
                KNorm.Let(("Ti2", Type.IntType), KNorm.Int 30,
                    KNorm.Apply("add", ["Ti1"; "Ti2"])))
        )
    }
    {
        s_expr = @"
            (let-rec (fib x) =
                (if (<= x 1)
                    then 1
                    else (+ (fib (- x 1)) (fib (- x 2)))) in (fib 10))
        "
        expected_k_form = KNorm.LetRec(
            {
                name = "fib", Type.FunType([Type.IntType], Type.IntType)
                args = [("x", Type.IntType)]
                body = KNorm.Let(("Ti2", Type.IntType), KNorm.Int 1,
                    KNorm.BranchLE("x", "Ti2",
                        KNorm.Int 1,
                        KNorm.Let(("Ti5", Type.IntType),
                            KNorm.Let(("Ti4", Type.IntType),
                                KNorm.Let(("Ti3", Type.IntType), KNorm.Int 1, KNorm.Sub("x", "Ti3")),
                                KNorm.Apply("fib", ["Ti4"])
                            ),
                            KNorm.Let(("Ti8", Type.IntType),
                                KNorm.Let(("Ti7", Type.IntType),
                                    KNorm.Let(("Ti6", Type.IntType), KNorm.Int 2, KNorm.Sub("x", "Ti6")),
                                    KNorm.Apply("fib", ["Ti7"])
                                ),
                                KNorm.Add("Ti5", "Ti8")
                            )
                        )
                    )
                )
            }, KNorm.Let(("Ti1", Type.IntType), KNorm.Int 10, KNorm.Apply("fib", ["Ti1"]))
        )
    }
    {
        // код аналогичен предыдущему тесту, но изначально K-нормализован
        s_expr = @"
            (let-rec (fib x) =
              (let Ti2 = 1 in
                (if (<= x Ti2)
                  then 1
                  else
                    (let Ti5 = (let Ti4 = (let Ti3 = 1 in (- x Ti3)) in (fib Ti4)) in
                      (let Ti8 = (let Ti7 = (let Ti6 = 2 in (- x Ti6)) in (fib Ti7)) in
                        (+ Ti5 Ti8)))))
              in (let Ti1 = 10 in (fib Ti1)))
        "
        expected_k_form = KNorm.LetRec(
            {
                name = "fib", Type.FunType([Type.IntType], Type.IntType)
                args = [("x", Type.IntType)]
                body = KNorm.Let(("Ti2", Type.IntType), KNorm.Int 1,
                    KNorm.BranchLE("x", "Ti2",
                        KNorm.Int 1,
                        KNorm.Let(("Ti5", Type.IntType),
                            KNorm.Let(("Ti4", Type.IntType),
                                KNorm.Let(("Ti3", Type.IntType), KNorm.Int 1, KNorm.Sub("x", "Ti3")),
                                KNorm.Apply("fib", ["Ti4"])
                            ),
                            KNorm.Let(("Ti8", Type.IntType),
                                KNorm.Let(("Ti7", Type.IntType),
                                    KNorm.Let(("Ti6", Type.IntType), KNorm.Int 2, KNorm.Sub("x", "Ti6")),
                                    KNorm.Apply("fib", ["Ti7"])
                                ),
                                KNorm.Add("Ti5", "Ti8")
                            )
                        )
                    )
                )
            }, KNorm.Let(("Ti1", Type.IntType), KNorm.Int 10, KNorm.Apply("fib", ["Ti1"]))
        )
    }
    {
        s_expr = @"
            (let arr : ([] i) = (new[] 0 2) in
                (;
                (set[] arr 0 <- 10)
                (set[] arr 1 <- 20)
                (+
                    (get[] arr 0)
                    (get[] arr 1))))"
        expected_k_form = KNorm.Let(("arr", Type.ArrayType Type.IntType),
            KNorm.Let(("Ti3", Type.IntType), KNorm.Int 2,
                KNorm.Let(("Ti4", Type.IntType), KNorm.Int 0, KNorm.ExtFunApply("create_array", ["Ti3"; "Ti4"]))),
            KNorm.Let(("Tu1", Type.UnitType),
                KNorm.Let(("Ti5", Type.IntType), KNorm.Int 0,
                KNorm.Let(("Ti6", Type.IntType), KNorm.Int 10, KNorm.Put("arr", "Ti5", "Ti6"))),
                KNorm.Let(("Tu2", Type.UnitType),
                    KNorm.Let(("Ti7", Type.IntType), KNorm.Int 1,
                    KNorm.Let(("Ti8", Type.IntType), KNorm.Int 20, KNorm.Put("arr", "Ti7", "Ti8"))),
                    KNorm.Let(("Ti10", Type.IntType),
                        KNorm.Let(("Ti9", Type.IntType), KNorm.Int 0, KNorm.Get("arr", "Ti9")),
                        KNorm.Let(("Ti12", Type.IntType),
                            KNorm.Let(("Ti11", Type.IntType), KNorm.Int 1, KNorm.Get("arr", "Ti11")),
                            KNorm.Add("Ti10", "Ti12")
                        )
                    )
                )
            )
        )
    }
]

[<Test>]
let testKNormalisation () =
    for case in k_norm_tests do
        Id.reset ()
        let k_form =
            case.s_expr
            |> SExpr.parse
            |> Parsing.f
            |> Typing.f
            |> KNormalisation.f
        Assert.AreEqual(case.expected_k_form, k_form)
