module mini_caml_fsharp.SExpr

open Microsoft.FSharp.Core
open NUnit.Framework
open mini_caml_fsharp.ParserCombinators

module SExpr =
    type SExpr =
        | SExprInt of int64
        | SExprBool of bool
        | SExprFloat of double
        | SExprId of char list
        | SExprList of SExpr list

    type Token =
        | LParenToken
        | RParenToken
        | IdToken of char list
        | IntToken of int
        | BoolToken of bool
        | FloatToken of double

    let lex_step: LexerStep<Token> =
        let rec lex_init =
            function
            | [] -> None, []
            | '(' :: rest -> Some LParenToken, rest
            | ')' :: rest -> Some RParenToken, rest
            | c :: rest when System.Char.IsWhiteSpace(c) -> lex_init rest
            | c :: rest when System.Char.IsDigit(c) -> lex_int_or_float ((int c) - (int '0')) rest
            | c :: rest -> lex_id (fun xs -> c :: xs) rest

        and lex_int_or_float n s =
            match s with
            | c :: rest when System.Char.IsDigit(c) -> lex_int_or_float (n * 10 + (int c) - (int '0')) rest
            | '.' :: rest -> lex_float (double n) 0.1 rest
            | _ -> Some(IntToken n), s

        and lex_float n next_fp_part s =
            match s with
            | c :: rest when System.Char.IsDigit(c) ->
                let added_part = (int c) - (int '0')
                let added_part = (double added_part) * next_fp_part
                lex_float (n + added_part) (next_fp_part * 0.1) rest
            | _ -> Some(FloatToken n), s

        and lex_id cont s =
            match s with
            | c :: rest when not (System.Char.IsWhiteSpace(c) || c.Equals('(') || c.Equals(')')) ->
                lex_id (cont << fun xs -> c :: xs) rest
            | _ ->
                match cont [] with
                | [ '#'; 't' ] -> Some(BoolToken true), s
                | [ '#'; 'f' ] -> Some(BoolToken false), s
                | ident -> Some(IdToken ident), s

        makeParser lex_init

    type SExprGrammar() =
        member this.parseSExpr = this.parseAtom <|> this.parseList

        member this.parseSExprRef = parserRef <| fun () -> this.parseSExpr

        member this.parseAtom =
            this.parseId <|> this.parseInt <|> this.parseFloat <|> this.parseBool

        member this.parseId =
            makeParser
            <| function
                | IdToken id :: rest -> SExprId id, rest
                | x -> raise (ParseException $"Expected identifier but got %A{x}")

        member this.parseInt =
            makeParser
            <| function
                | IntToken n :: rest -> SExprInt n, rest
                | x -> raise (ParseException $"Expected integer but got %A{x}")

        member this.parseFloat =
            makeParser
            <| function
                | FloatToken n :: rest -> SExprFloat n, rest
                | x -> raise (ParseException $"Expected float but got %A{x}")

        member this.parseBool =
            makeParser
            <| function
                | BoolToken b :: rest -> SExprBool b, rest
                | x -> raise (ParseException $"Expected boolean but got %A{x}")

        member this.parseLParen =
            makeParser
            <| function
                | LParenToken :: rest -> (), rest
                | x -> raise (ParseException $"Expected LParen but got %A{x}")

        member this.parseRParen =
            makeParser
            <| function
                | RParenToken :: rest -> (), rest
                | x -> raise (ParseException $"Expected RParen but got %A{x}")

        member this.parseList =
            this.parseLParen -=>+ this.parseSExprRef.some +=>- this.parseRParen
            |=> SExprList

        member this.parse s =
            let lexer = lex_step |> toSeqLexer
            let tokenStream = lexer (Seq.toList s) |> Seq.toList

            match this.parseSExpr.parse tokenStream with
            | x, [] -> x
            | _, garbage -> raise (ParseException $"found some garbage at the end: %A{garbage}")

    let parse source = source |> SExprGrammar().parse

[<Test>]
let testSExprParser () =
    let tests: (string * SExpr.SExpr) list =
        [ "123", SExpr.SExprInt 123

          "hello?", SExpr.SExprId(Seq.toList "hello?")

          "(1 2.51 #f)", SExpr.SExprList [ SExpr.SExprInt 1; SExpr.SExprFloat 2.51; SExpr.SExprBool false ]

          "((1 2 3) (1 2 3) (1 2 3))",
          SExpr.SExprList
              [ SExpr.SExprList [ SExpr.SExprInt 1; SExpr.SExprInt 2; SExpr.SExprInt 3 ]
                SExpr.SExprList [ SExpr.SExprInt 1; SExpr.SExprInt 2; SExpr.SExprInt 3 ]
                SExpr.SExprList [ SExpr.SExprInt 1; SExpr.SExprInt 2; SExpr.SExprInt 3 ] ]

          "(a/test 2 1 + 3)",
          SExpr.SExprList
              [ SExpr.SExprId(Seq.toList "a/test")
                SExpr.SExprInt 2
                SExpr.SExprInt 1
                SExpr.SExprId(Seq.toList "+")
                SExpr.SExprInt 3 ] ]

    for source, expected in tests do
        Assert.AreEqual(expected, SExpr.parse source)
