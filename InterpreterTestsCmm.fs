module mini_caml_fsharp.InterpreterTestsCmm

open Microsoft.FSharp.Core
open NUnit.Framework
open mini_caml_fsharp
open mini_caml_fsharp.SExpr
open mini_caml_fsharp.Id
open mini_caml_fsharp.Parsing
open mini_caml_fsharp.Typing
open mini_caml_fsharp.KNormalisation
open mini_caml_fsharp.AlphaConv
open mini_caml_fsharp.BetaReduction
open mini_caml_fsharp.Assoc
open mini_caml_fsharp.Inlining
open mini_caml_fsharp.ConstFolding
open mini_caml_fsharp.Elimination
open mini_caml_fsharp.ClosureRepresentationConv
open mini_caml_fsharp.CmmConv
open mini_caml_fsharp.CmmInterpreter

type private test_case = { s_expr: string; expected_res: CmmInterpreter.value_t }

let private interpretation_tests: test_case list = [
    // Тесты полностью идентичны таковым для KNorm
    {
        s_expr = "()"
        expected_res = CmmInterpreter.Unit
    }
    {
        s_expr = "13"
        expected_res = CmmInterpreter.Int 13
    }
    {
        s_expr = "(let x = (<= 5 10) in x)"
        expected_res = CmmInterpreter.Int 1
    }
    {
        s_expr = "(let x = (= 5 10) in x)"
        expected_res = CmmInterpreter.Int 0
    }
    {
        s_expr = "(let x = 5 in x)"
        expected_res = CmmInterpreter.Int 5
    }
    {
        s_expr = "(let x = 5 in (- x))"
        expected_res = CmmInterpreter.Int -5
    }
    {
       s_expr = "(let x = #t in x)"
       expected_res = CmmInterpreter.Int 1
    }
    {
       s_expr = "(let x = #t in (not x))"
       expected_res = CmmInterpreter.Int 0
    }
    {
        s_expr = "(let x = 123.456 in x)"
        expected_res = CmmInterpreter.Float 123.456
    }
    {
        s_expr = "(let x = 123.456 in (-. x))"
        expected_res = CmmInterpreter.Float -123.456
    }
    {
        s_expr = "(let x : (, i _) = (, 5 #t) in x)"
        expected_res = CmmInterpreter.Memory [| CmmInterpreter.Int 5; CmmInterpreter.Int 1 |]
    }
    {
        s_expr = "(let (, x y z) = (, 10 #t 42) in (if y then x else z))"
        expected_res = CmmInterpreter.Int 10
    }
    {
        s_expr = "(let (, x y z) = (, 10 #f 42) in (if y then x else z))"
        expected_res = CmmInterpreter.Int 42
    }
    {
        s_expr = "(let (, x y z) = (, 10 #f 42) in (if (not y) then x else z))"
        expected_res = CmmInterpreter.Int 10
    }
    {
        s_expr = "(let-rec (add x y) = (+ x y) in (add 12 30))"
        expected_res = CmmInterpreter.Int 42
    }
    {
        s_expr = "(let-rec (sub3 x y z) = (- x y z) in (sub3 12 3 4))"
        expected_res = CmmInterpreter.Int 5
    }
    {
        s_expr = "(let-rec (add x y) = (+. x y) in (add 12.5 12.5))"
        expected_res = CmmInterpreter.Float 25.0
    }
    {
        s_expr = "(*. 1.0 2.0 3.0 4.0 5.0)"
        expected_res = CmmInterpreter.Float 120.0
    }
    {
        s_expr = "(/. 33.0 3.0 0.5)"
        expected_res = CmmInterpreter.Float 22.0
    }
    {
        s_expr = @"
            (let-rec (fib x) =
                (if (<= x 1)
                    then 1
                    else (+ (fib (- x 1)) (fib (- x 2)))) in (fib 6))
        "
        expected_res = CmmInterpreter.Int 13
    }
    {
        s_expr = @"
            (let-rec (fib x) : (i) -> i =
              (let-rec (fib-tail x-2 x-1 x) : (i i i) -> i =
                (if (<= x 1)
                  then x-1
                  else
                    (let (, x-2 x-1 x) = (, x-1 (+ x-2 x-1) (- x 1))
                    in (fib-tail x-2 x-1 x)))
                in (fib-tail 1 1 x))
              in (fib 6))
        "
        expected_res = CmmInterpreter.Int 13
    }
    {
        s_expr = @"
            (let-rec (fact x) =
                (if (<= x 1.0)
                    then 1.0
                    else (*. x (fact (-. x 1.0 )))) in (fact 6.0))
        "
        expected_res = CmmInterpreter.Float 720.0
    }
    {
        s_expr = @"
            (let-rec (foo t) : ( (, i i i) ) -> i =
                (let (, x y z) = t in (+ x y z)) in
                (foo (, 3 4 5)))
        "
        expected_res = CmmInterpreter.Int 12
    }
    {
        s_expr = @"
            (let-rec (fact x) =
                (if (<= x 1.0)
                    then 1.0
                    else (*. x (fact (-. x 1.0 )))) in
                (let f = fact in (f 6.0) ))
        "
        expected_res = CmmInterpreter.Float 720.0
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
        expected_res = CmmInterpreter.Int 30
    }
    {
        s_expr = @"
            (let arr : ([] f) = (new[] 0.0 2) in
                (;
                (set[] arr 0 <- 10.0)
                (set[] arr 1 <- 20.0)
                (+.
                    (get[] arr 0)
                    (get[] arr 1))))
        "
        expected_res = CmmInterpreter.Float 30.0
    }
    {
        s_expr = @"
            (let arr = (new[] (, 5 ()) 2) in
                (;
                (set[] arr 0 <- (, 15 ()))
                (+
                    (let (, x z) = (get[] arr 0) in x)
                    (let (, y w) = (get[] arr 1) in y))))
        "
        expected_res = CmmInterpreter.Int 20
    }
    {
        // Императивный факториал
        s_expr = @"
            (let acc : ([] f) = (new[] 1.0 1) in
            (let-rec (fact-step x) =
                (if (;тут-был-вася (<= x 1.0))
                    then ()
                    else
                        (;Внимание!!!!-мутабельность!!!!-аыаыаыа
                            (let v = (get[] acc 0) in
                            (let v' = (*. x v) in
                            (;
                                (set[] acc 0 <- v')
                                (fact-step (-. x 1.0))))))) in
            (;
                (fact-step 6.0)
                (get[] acc 0))))
        "
        expected_res = CmmInterpreter.Float 720.0
    }
]

[<Test>]
let testCmmInterpretation () =
    let limit = 100
    let rec iter n e =
        printfn $"iteration %d{n}."
        match n with
        | 0 -> e
        | _ ->
            let e' = e |> BetaReduction.f
            let e' = e' |> Assoc.f
            let e' = e' |> Inlining.f 45
            let e' = e' |> ConstFolding.f
            let e' = e' |> Elimination.f
            if e = e' then e' else iter (n - 1) e'

    for case in interpretation_tests do
        Id.reset ()
        let k_form =
            case.s_expr
            |> SExpr.parse
            |> Parsing.f
            |> Typing.f
            |> KNormalisation.f
            |> AlphaConv.f

        let converted =
            (limit, k_form)
            ||> iter
            |> ClosureRepresentationConv.f
            |> CmmConv.f
        let res = converted |> CmmInterpreter.f
        Assert.AreEqual(case.expected_res, res)
