module mini_caml_fsharp_core.SExpr

open Microsoft.FSharp.Core
open mini_caml_fsharp_core.ParserCombinators

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
    
    /// Альтернативный парсер для использования в blazor.
    /// Парсер основанный на комбинаторах показывает плохую
    /// производетельность на Web Assembly, вероятно, по причине
    /// насыщенного использования обработки исключений
    let parse_alternative s =
        let rec parse' s =
            match lex_step.parse s with
            | Some LParenToken, rest -> parse_list id rest
            | Some (IdToken id), rest -> SExprId id, rest
            | Some (IntToken i), rest -> SExprInt i, rest
            | Some (FloatToken f), rest -> SExprFloat f, rest
            | Some (BoolToken b), rest -> SExprBool b, rest
            | Some RParenToken, _ -> failwith "ill formed data! Expected Atom or LParen, but got RParen"
            | None, _ -> failwith "ill formed data! Expected Atom or LParen, but got nothing"
        and parse_list cont s =
            match lex_step.parse s with
            | Some RParenToken, rest -> SExprList (cont []), rest
            | _ ->
                let x, rest = parse' s
                parse_list (cont << (fun xs -> x::xs)) rest
        match parse' (Seq.toList s) with
        | s_expr, [] -> s_expr
        | _, garbage -> failwithf $"ill formed data! detected garbage at the end: %A{garbage}"