module mini_caml_fsharp.GenCTests


open Microsoft.FSharp.Core
open NUnit.Framework
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
open mini_caml_fsharp.CmmDeclosuredConv
open mini_caml_fsharp.GenC

type private test_case = {
    s_expr: string
}

let private interpretation_tests: test_case list = [
    // {
    //     s_expr = @"
    //         (let-rec (fact x) =
    //             (if (<= x 1.0)
    //                 then 1.0
    //                 else (*. x (fact (-. x 1.0 )))) in
    //             (let f = fact in (print_int (int_of_float (f 6.0)))))
    //     "
    // }
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
                (print_int (int_of_float (get[] acc 0))))))
        "
    }
]

[<Test>]
let testGenC () =
    let limit = 100
    let rec iter n e =
        printfn $"iteration %d{n}."
        match n with
        | 0 -> e
        | _ ->
            let e' = e |> BetaReduction.f
            let e' = e' |> Assoc.f
            let e' = e' |> Inlining.f 16
            let e' = e' |> ConstFolding.f
            let e' = e' |> Elimination.f
            if e = e' then e' else iter (n - 1) e'

    for case in interpretation_tests do
        Id.reset ()
        let k_form =
            case.s_expr
            |> SExpr.parse
            |> Parsing.f
            |> Typing.f Typing.ProgramShouldNotReturnFunction
            |> KNormalisation.f
            |> AlphaConv.f
        
        let converted = (limit, k_form) ||> iter
        let res_text =
            converted
            |> ClosureRepresentationConv.f
            |> CmmConv.f
            |> CmmDeclosuredConv.f
            |> GenC.f
        
        printfn $"s_expr: \n%s{case.s_expr} \ngenerated C code: \n%s{res_text}"