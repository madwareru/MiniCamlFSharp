module mini_caml_fsharp.Parsing

open Microsoft.FSharp.Core
open NUnit.Framework
open mini_caml_fsharp.SExpr
open mini_caml_fsharp.Id
open mini_caml_fsharp.Type
open mini_caml_fsharp.Syntax

module Parsing =
    let make_id id_chars =
        Id.t (new string [| for c in id_chars -> c |])

    let add_typ id_chars =
        let id = make_id id_chars
        id, Type.gen_empty ()

    let rec parse_bindings =
        function
        | [ SExpr.SExprId binding ] -> [ add_typ binding ]
        | SExpr.SExprId binding :: rest -> (add_typ binding) :: (parse_bindings rest)
        | _ -> failwith "Failed to parse bindings in let tuple expression"

    let rec f =
        function
        | SExpr.SExprId id -> Syntax.VarNode(make_id id)
        | SExpr.SExprList [] -> Syntax.UnitNode
        | SExpr.SExprBool b -> Syntax.BoolNode b
        | SExpr.SExprInt i -> Syntax.IntNode i
        | SExpr.SExprFloat f -> Syntax.FloatNode f
        | SExpr.SExprList [ SExpr.SExprId [ 'n'; 'o'; 't' ]; SExpr.SExprBool b ] -> Syntax.BoolNode <| not b
        | SExpr.SExprList [ SExpr.SExprId [ 'n'; 'o'; 't' ]; e ] -> Syntax.NotNode <| f e
        | SExpr.SExprList [ SExpr.SExprId [ '-' ]; SExpr.SExprInt i ] -> Syntax.IntNode -i
        | SExpr.SExprList [ SExpr.SExprId [ '-' ]; e ] -> Syntax.NegNode <| f e
        | SExpr.SExprList [ SExpr.SExprId [ '-'; '.' ]; SExpr.SExprFloat f ] -> Syntax.FloatNode -f
        | SExpr.SExprList [ SExpr.SExprId [ '-'; '.' ]; e ] -> Syntax.FNegNode <| f e
        | SExpr.SExprList [ SExpr.SExprId [ '-' ]; lhs; rhs ] ->
            let lhs = f lhs
            let rhs = f rhs
            Syntax.SubNode(lhs, rhs)
        | SExpr.SExprList [ SExpr.SExprId [ '+' ]; lhs; rhs ] ->
            let lhs = f lhs
            let rhs = f rhs
            Syntax.AddNode(lhs, rhs)
        | SExpr.SExprList [ SExpr.SExprId [ '-'; '.' ]; lhs; rhs ] ->
            let lhs = f lhs
            let rhs = f rhs
            Syntax.FSubNode(lhs, rhs)
        | SExpr.SExprList [ SExpr.SExprId [ '+'; '.' ]; lhs; rhs ] ->
            let lhs = f lhs
            let rhs = f rhs
            Syntax.FAddNode(lhs, rhs)
        | SExpr.SExprList [ SExpr.SExprId [ '*'; '.' ]; lhs; rhs ] ->
            let lhs = f lhs
            let rhs = f rhs
            Syntax.FMulNode(lhs, rhs)
        | SExpr.SExprList [ SExpr.SExprId [ '/'; '.' ]; lhs; rhs ] ->
            let lhs = f lhs
            let rhs = f rhs
            Syntax.FDivNode(lhs, rhs)
        | SExpr.SExprList [ SExpr.SExprId [ '=' ]; lhs; rhs ] ->
            let lhs = f lhs
            let rhs = f rhs
            Syntax.EqNode(lhs, rhs)
        | SExpr.SExprList [ SExpr.SExprId [ '<'; '>' ]; lhs; rhs ] ->
            let lhs = f lhs
            let rhs = f rhs
            Syntax.NotNode(Syntax.EqNode(lhs, rhs))
        | SExpr.SExprList [ SExpr.SExprId [ '<' ]; lhs; rhs ] ->
            let lhs = f lhs
            let rhs = f rhs
            Syntax.NotNode(Syntax.LENode(rhs, lhs))
        | SExpr.SExprList [ SExpr.SExprId [ '>' ]; lhs; rhs ] ->
            let lhs = f lhs
            let rhs = f rhs
            Syntax.NotNode(Syntax.LENode(lhs, rhs))
        | SExpr.SExprList [ SExpr.SExprId [ '<'; '=' ]; lhs; rhs ] ->
            let lhs = f lhs
            let rhs = f rhs
            Syntax.LENode(lhs, rhs)
        | SExpr.SExprList [ SExpr.SExprId [ '>'; '=' ]; lhs; rhs ] ->
            let lhs = f lhs
            let rhs = f rhs
            Syntax.LENode(rhs, lhs)
        | SExpr.SExprList(SExpr.SExprId [ ',' ] :: args) ->
            let args = args |> List.map f
            Syntax.TupleNode args
        | SExpr.SExprList [ SExpr.SExprId [ '['; ']' ]; e; l ] ->
            let e = f e
            let l = f l
            Syntax.ArrayNode(e, l)
        | SExpr.SExprList [ SExpr.SExprId [ '['; 'g'; 'e'; 't'; ']' ]; a; i ] ->
            let a = f a
            let i = f i
            Syntax.GetNode(a, i)
        | SExpr.SExprList [ SExpr.SExprId [ '['; 'g'; 'e'; 't'; ']' ]; a; i; e ] ->
            let a = f a
            let i = f i
            let e = f e
            Syntax.PutNode(a, i, e)
        | SExpr.SExprList [ SExpr.SExprId [ 'i'; 'f' ]
                            cond
                            SExpr.SExprId [ 't'; 'h'; 'e'; 'n' ]
                            then_e
                            SExpr.SExprId [ 't'; 'h'; 'e'; 'n' ]
                            else_e ] ->
            let cond = f cond
            let then_e = f then_e
            let else_e = f else_e
            Syntax.IfNode(cond, then_e, else_e)
        | SExpr.SExprList [ SExpr.SExprId [ 'l'; 'e'; 't' ]
                            SExpr.SExprId binding
                            SExpr.SExprId [ '=' ]
                            e
                            SExpr.SExprId [ 'i'; 'n' ]
                            cont ] ->
            let binding = add_typ binding
            let e = f e
            let cont = f cont
            Syntax.LetNode(binding, e, cont)
        | SExpr.SExprList [ SExpr.SExprId [ 'l'; 'e'; 't' ]
                            SExpr.SExprList [ SExpr.SExprId [ ',' ] ]
                            SExpr.SExprId [ '=' ]
                            _
                            SExpr.SExprId [ 'i'; 'n' ]
                            _ ] -> failwith "construction of a tuple with 0 elements are not supported"
        | SExpr.SExprList [ SExpr.SExprId [ 'l'; 'e'; 't' ]
                            SExpr.SExprList(SExpr.SExprId [ ',' ] :: bindings)
                            SExpr.SExprId [ '=' ]
                            e
                            SExpr.SExprId [ 'i'; 'n' ]
                            cont ] ->
            let bindings = parse_bindings bindings
            let e = f e
            let cont = f cont
            Syntax.LetTuple(bindings, e, cont)
        | SExpr.SExprList [ SExpr.SExprId [ 'l'; 'e'; 't'; '-'; 'r'; 'e'; 'c' ]
                            SExpr.SExprList [ SExpr.SExprId _ ]
                            SExpr.SExprId [ '=' ]
                            _
                            SExpr.SExprId [ 'i'; 'n' ]
                            _ ] -> failwith "let-rec with 0 args are not supported"
        | SExpr.SExprList [ SExpr.SExprId [ 'l'; 'e'; 't'; '-'; 'r'; 'e'; 'c' ]
                            SExpr.SExprList(SExpr.SExprId name :: args)
                            SExpr.SExprId [ '=' ]
                            body
                            SExpr.SExprId [ 'i'; 'n' ]
                            cont ] ->
            let fun_def: Syntax.fun_def =
                { name = add_typ name
                  args = parse_bindings args
                  body = f body }

            let cont = f cont
            Syntax.LetRecNode(fun_def, cont)
        | SExpr.SExprList(SExpr.SExprId [ ';' ] :: es) ->
            let rec unwind =
                function
                | [ e ] -> f e
                | e :: cont ->
                    let e = f e
                    // we are saying here that the type of an expr should be Unit
                    // if it wants to behave like a statement.
                    let id = Id.gen_tmp Type.UnitType
                    let cont = unwind cont
                    Syntax.LetNode((id, Type.UnitType), e, cont)
                | _ -> failwith "sequence of 0 statements are not supported"

            unwind es
        | SExpr.SExprList [ SExpr.SExprId [ '_' ]; e ] ->
            // (_ $e) becomes (let _ = $e in ())
            let e = f e
            let id = Id.t "_"
            Syntax.LetNode((id, Type.gen_empty ()), e, Syntax.UnitNode)
        | SExpr.SExprList [ _ ] -> failwith "apply with 0 args are not supported"
        | SExpr.SExprList(foo :: args) ->
            let foo = f foo
            let args = args |> List.map f
            Syntax.ApplyNode(foo, args)

[<Test>]
let testParsingSExprToSyntax () =
    let tests: (string * Syntax.t) list =
        [ "123", Syntax.IntNode 123

          "(+ 2 2)", Syntax.AddNode(Syntax.IntNode 2, Syntax.IntNode 2)

          "(- 2 2)", Syntax.SubNode(Syntax.IntNode 2, Syntax.IntNode 2)

          "(- 2)", Syntax.IntNode -2

          "(not #t)", Syntax.BoolNode false

          "(let x = 2 in (+ x 2))",
          Syntax.LetNode(
              ("x", Type.gen_empty ()),
              Syntax.IntNode 2,
              Syntax.AddNode(Syntax.VarNode("x"), Syntax.IntNode 2)
          ) ]

    for source, expected in tests do
        let parsed_s_expr = SExpr.parse source
        let parsed_syntax = Parsing.f parsed_s_expr
        Assert.AreEqual(expected, parsed_syntax)
