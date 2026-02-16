module mini_caml_fsharp.ParserCombinators

exception ParseException of string

type Parser<'TToken, 'TValue> =
    { parse: 'TToken list -> 'TValue * 'TToken list }

type LexerStep<'TToken> = Parser<char, 'TToken option>

let toSeqLexer (l: LexerStep<_>) =
    Some
    >> Seq.unfold (
        Option.bind (fun cur ->
            match l.parse cur with
            | Some token, [] -> Some(token, None)
            | Some token, rest -> Some(token, Some rest)
            | _ -> None)
    )

let makeParser parse = { parse = parse }

/// Map
let (|=>) a mapping =
    makeParser
    <| fun input ->
        let result, remainder = a.parse input
        mapping result, remainder

/// Bind
let (>>=) a mapping =
    makeParser
    <| fun input ->
        let first, remainder = a.parse input
        (mapping first).parse remainder

let map mapping a = a |=> mapping
let bind mapping a = a >>= mapping

/// Or combinator of two parsers. If first parser fails it will try to parse with the second
let (<|>) a b =
    makeParser
    <| fun input ->
        try
            let res, remainder = a.parse input
            res, remainder
        with
        | :? ParseException -> b.parse input
        | x -> raise x

/// Applicative Join of two parsers
let (+=>+) a b = a >>= fun t0 -> b |=> fun t1 -> t0, t1

/// Applicative Join of two parsers, the result on the left is ignored
let (-=>+) a b = a >>= fun _ -> b

/// Applicative Join of two parsers, the result on the right is ignored
let (+=>-) a b = a >>= fun t0 -> b |=> fun _ -> t0

/// Applicative Join of two parsers, both results are ignored
let (-=>-) a b = a >>= fun _ -> b |=> fun _ -> ()

type ParserBuilder() =
    member this.Bind(p0, m) = p0 >>= m
    member this.Combine(p0, p1) = p0 -=>+ p1
    member this.ReturnFrom(p) = p
    member this.Return(r) = makeParser <| fun input -> r, input

let parser = ParserBuilder()

/// Used in places where there is a need to do a recursive parse
let parserRef (a: Unit -> Parser<'TToken, 'T0>) =
    makeParser <| fun input -> a().parse input

type Parser<'TToken, 'TValue> with
    member this.optional = (this |=> Some) <|> (makeParser <| fun input -> None, input)

    member this.some =
        makeParser
        <| fun input ->
            let mutable cont = id
            let mutable rem = input
            let mutable complete = false

            while not complete do
                try
                    let x, rest = this.parse rem
                    cont <- cont << fun xs -> x :: xs
                    rem <- rest
                with
                | :? ParseException -> complete <- true
                | x -> raise x

            cont [], rem

    member this.many = this +=>+ this.some |=> List.Cons

    member this.sepBy separator =
        this +=>+ (separator -=>+ this).many |=> List.Cons

    member this.sepBy1 separator =
        this +=>+ (separator -=>+ this).some |=> List.Cons

    member this.sepBy0 separator =
        (this.sepBy1 separator).optional |=> Option.defaultValue []
