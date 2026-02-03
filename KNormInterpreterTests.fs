module mini_caml_fsharp.KNormInterpreterTests

open Microsoft.FSharp.Core
open NUnit.Framework
open mini_caml_fsharp.SExpr
open mini_caml_fsharp.Id
open mini_caml_fsharp.Parsing
open mini_caml_fsharp.Typing
open mini_caml_fsharp.KNormalisation
open mini_caml_fsharp.KNormInterpreter

type private test_case = { s_expr: string; expected_res: KNormInterpreter.value_t }

let private k_norm_tests: test_case list = [
    {
        s_expr = "(let x = 5 in x)"
        expected_res = KNormInterpreter.Int 5
    }
    {
       s_expr = "(let x = #t in x)"
       expected_res = KNormInterpreter.Int 1
    }
    {
        s_expr = "(let x = 123.456 in x)"
        expected_res = KNormInterpreter.Float 123.456
    }
    {
        s_expr = "(let x : (, i _) = (, 5 #t) in x)"
        expected_res = KNormInterpreter.Tuple [ KNormInterpreter.Int 5; KNormInterpreter.Int 1 ]
    }
    {
        s_expr = "(let (, x y z) = (, 10 #t 42) in (if y then x else z))"
        expected_res = KNormInterpreter.Int 10
    }
    {
        s_expr = "(let (, x y z) = (, 10 #f 42) in (if y then x else z))"
        expected_res = KNormInterpreter.Int 42
    }
    {
        s_expr = "(let-rec (add x y) = (+ x y) in (add 12 30))"
        expected_res = KNormInterpreter.Int 42
    }
    {
        s_expr = @"
            (let-rec (fib x) =
                (if (<= x 1)
                    then 1
                    else (+ (fib (- x 1)) (fib (- x 2)))) in (fib 6))
        "
        expected_res = KNormInterpreter.Int 13
    }
    {
        s_expr = @"
            (let arr : ([] i) = (new[] 0 2) in
                (;
                ([set] arr 0 10)
                ([set] arr 1 20)
                (+
                    ([get] arr 0)
                    ([get] arr 1))))
        "
        expected_res = KNormInterpreter.Int 30
    }
]

[<Test>]
let testKNormalisationInterpretation () =
    for case in k_norm_tests do
        Id.reset ()
        let k_form =
            case.s_expr
            |> SExpr.parse
            |> Parsing.f
            |> Typing.f
            |> KNormalisation.f
        let res = k_form |> KNormInterpreter.f
        Assert.AreEqual(case.expected_res, res)
