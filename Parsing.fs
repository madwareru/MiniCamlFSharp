module mini_caml_fsharp.Parsing

open mini_caml_fsharp.SExpr
open mini_caml_fsharp.Id
open mini_caml_fsharp.Type
open mini_caml_fsharp.Syntax

module Parsing =
    let make_id id_chars =
        Id.t (new string [| for c in id_chars -> c |])


    let add_typ id_chars =
        let id = make_id id_chars
        id, Type.gentyp ()

    let rec f =
        function
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
        | _ -> failwith "TODO! handle other cases"
