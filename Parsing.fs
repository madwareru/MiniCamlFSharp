module mini_caml_fsharp.Parsing

open mini_caml_fsharp.SExpr
open mini_caml_fsharp.Id
open mini_caml_fsharp.Type
open mini_caml_fsharp.Syntax

module Parsing =
    let private make_id id_chars = new string [| for c in id_chars -> c |]

    let private add_typ id_chars =
        let id = make_id id_chars
        id, Type.gen_empty ()

    let rec private parse_bindings =
        function
        | [ SExpr.SExprId binding ] -> [ add_typ binding ]
        | SExpr.SExprId binding :: rest -> add_typ binding :: parse_bindings rest
        | _ -> failwith "Failed to parse bindings in let tuple expression"

    let rec private parse_t =
        function
        | SExpr.SExprId [ 'i' ] -> Type.IntType
        | SExpr.SExprId [ 'b' ] -> Type.BoolType
        | SExpr.SExprId [ 'f' ] -> Type.FloatType
        | SExpr.SExprId [ 'u' ] -> Type.UnitType
        | SExpr.SExprId [ '_' ] -> Type.gen_empty ()
        | SExpr.SExprList [ SExpr.SExprId [ '['; ']' ]; t ] -> Type.ArrayType <| parse_t t
        | SExpr.SExprList(SExpr.SExprId [ ',' ] :: ts) -> Type.TupleType(ts |> List.map parse_t)
        | SExpr.SExprList [ SExpr.SExprId [ 'f'; 'n' ]; SExpr.SExprList arg_types; SExpr.SExprId [ '-'; '>' ]; ret_type ] ->
            let arg_types = arg_types |> List.map parse_t
            let ret_type = ret_type |> parse_t
            Type.FunType(arg_types, ret_type)
        | _ -> failwith "failed to parse a type annotation"

    let private mk_reducer ctor f acc next = ctor (acc, f next)

    let private fail_form text xs =
        failwith
        <| Printf.sprintf $"'%s{text}' form with arity %d{xs |> List.length} is not supported"

    let rec private p_vararg form ctor exs =
        match exs with
        | x :: xs when not xs.IsEmpty -> xs |> List.fold (mk_reducer ctor p_expr) (p_expr x)
        | _ -> fail_form form exs

    and private p_bin form ctor exs =
        match exs with
        | [ lhs; rhs ] -> ctor (p_expr lhs, p_expr rhs)
        | _ -> fail_form form exs

    and private p_expr s_expr =
        match s_expr with
        | SExpr.SExprId id -> Syntax.VarNode(make_id id)
        | SExpr.SExprList [] -> Syntax.UnitNode
        | SExpr.SExprBool b -> Syntax.BoolNode b
        | SExpr.SExprInt i -> Syntax.IntNode i
        | SExpr.SExprFloat f -> Syntax.FloatNode f
        | SExpr.SExprList(SExpr.SExprId [ 'c'; 'l'; 'o'; 'n'; 'e'; '!' ] :: exs) ->
            match exs with
            | [ e ] -> Syntax.CloneNode <| p_expr e
            | _ -> fail_form "clone!" exs
        | SExpr.SExprList(SExpr.SExprId [ 'n'; 'o'; 't' ] :: exs) ->
            match exs with
            | [ SExpr.SExprBool b ] -> Syntax.BoolNode <| not b
            | [ e ] -> Syntax.NotNode <| p_expr e
            | _ -> fail_form "not" exs
        | SExpr.SExprList(SExpr.SExprId [ '-' ] :: exs) ->
            match exs with
            | [ SExpr.SExprInt i ] -> Syntax.IntNode -i
            | [ e ] -> Syntax.NegNode <| p_expr e
            | x :: xs when not xs.IsEmpty -> xs |> List.fold (mk_reducer Syntax.SubNode p_expr) (p_expr x)
            | _ -> fail_form "-" exs
        | SExpr.SExprList(SExpr.SExprId [ '-'; '.' ] :: exs) ->
            match exs with
            | [ SExpr.SExprFloat f ] -> Syntax.FloatNode -f
            | [ e ] -> Syntax.FNegNode <| p_expr e
            | x :: xs when not xs.IsEmpty -> xs |> List.fold (mk_reducer Syntax.FSubNode p_expr) (p_expr x)
            | _ -> fail_form "-." exs
        | SExpr.SExprList(SExpr.SExprId [ '+' ] :: exs) -> exs |> p_vararg "+" Syntax.AddNode
        | SExpr.SExprList(SExpr.SExprId [ '+'; '.' ] :: exs) -> exs |> p_vararg "+." Syntax.FAddNode
        | SExpr.SExprList(SExpr.SExprId [ '*'; '.' ] :: exs) -> exs |> p_vararg "*." Syntax.FMulNode
        | SExpr.SExprList(SExpr.SExprId [ '/'; '.' ] :: exs) -> exs |> p_vararg "/." Syntax.FDivNode
        | SExpr.SExprList(SExpr.SExprId [ 'o'; 'r'; '-'; 'e'; 'l'; 's'; 'e' ] :: exs) ->
            exs
            |> p_vararg "or-else" (fun (l, r) -> Syntax.IfNode(l, Syntax.BoolNode true, r))
        | SExpr.SExprList(SExpr.SExprId [ 'a'; 'n'; 'd'; '-'; 't'; 'h'; 'e'; 'n' ] :: exs) ->
            exs
            |> p_vararg "and-then" (fun (l, r) -> Syntax.IfNode(Syntax.NotNode(l), Syntax.BoolNode false, r))
        | SExpr.SExprList(SExpr.SExprId [ '=' ] :: exs) -> exs |> p_bin "=" Syntax.EqNode
        | SExpr.SExprList(SExpr.SExprId [ '<'; '>' ] :: exs) ->
            exs |> p_bin "<>" (fun (lhs, rhs) -> Syntax.NotNode(Syntax.EqNode(lhs, rhs)))
        | SExpr.SExprList(SExpr.SExprId [ '<' ] :: exs) ->
            exs |> p_bin "<" (fun (lhs, rhs) -> Syntax.NotNode(Syntax.LENode(rhs, lhs)))
        | SExpr.SExprList(SExpr.SExprId [ '>' ] :: exs) ->
            exs |> p_bin ">" (fun (lhs, rhs) -> Syntax.NotNode(Syntax.LENode(lhs, rhs)))
        | SExpr.SExprList(SExpr.SExprId [ '<'; '=' ] :: exs) -> exs |> p_bin "<=" Syntax.LENode
        | SExpr.SExprList(SExpr.SExprId [ '>'; '=' ] :: exs) ->
            exs |> p_bin ">=" (fun (lhs, rhs) -> Syntax.LENode(rhs, lhs))
        | SExpr.SExprList(SExpr.SExprId [ 'n'; 'e'; 'w'; '['; ']' ] :: exs) -> exs |> p_bin "new[]" Syntax.ArrayNode
        | SExpr.SExprList(SExpr.SExprId [ 'g'; 'e'; 't'; '['; ']' ] :: exs) -> exs |> p_bin "get[]" Syntax.GetNode
        | SExpr.SExprList(SExpr.SExprId [ 's'; 'e'; 't'; '['; ']' ] :: exs) ->
            match exs with
            | [ a; i; SExpr.SExprId [ '<'; '-' ]; e ] -> Syntax.PutNode(p_expr a, p_expr i, p_expr e)
            | _ -> fail_form "set[]" exs
        | SExpr.SExprList(SExpr.SExprId [ ',' ] :: args) ->
            match args with
            | [] -> fail_form "," args
            | _ -> Syntax.TupleNode(args |> List.map p_expr)
        | SExpr.SExprList [ SExpr.SExprId [ 'i'; 'f' ]
                            cond
                            SExpr.SExprId [ 't'; 'h'; 'e'; 'n' ]
                            then_e
                            SExpr.SExprId [ 'e'; 'l'; 's'; 'e' ]
                            else_e ] -> Syntax.IfNode(p_expr cond, p_expr then_e, p_expr else_e)
        | SExpr.SExprList(SExpr.SExprId [ 'i'; 'f' ] :: _) -> failwith "incorrect 'if' form found"
        | SExpr.SExprList(SExpr.SExprId [ 'l'; 'e'; 't' ] :: exs) ->
            match exs with
            | [ SExpr.SExprId binding; SExpr.SExprId [ '=' ]; e; SExpr.SExprId [ 'i'; 'n' ]; cont ] ->
                let binding = add_typ binding
                let e = p_expr e
                let cont = p_expr cont
                Syntax.LetNode(binding, e, cont)
            | [ SExpr.SExprId binding
                SExpr.SExprId [ ':' ]
                annot
                SExpr.SExprId [ '=' ]
                e
                SExpr.SExprId [ 'i'; 'n' ]
                cont ] ->
                let binding = (make_id binding, parse_t annot)
                let e = p_expr e
                let cont = p_expr cont
                Syntax.LetNode(binding, e, cont)
            | [ SExpr.SExprList [ SExpr.SExprId [ ',' ] ]; SExpr.SExprId [ '=' ]; _; SExpr.SExprId [ 'i'; 'n' ]; _ ] ->
                failwith "deconstruction of a tuple with 0 elements are not supported"
            | [ SExpr.SExprList(SExpr.SExprId [ ',' ] :: bindings)
                SExpr.SExprId [ '=' ]
                e
                SExpr.SExprId [ 'i'; 'n' ]
                cont ] ->
                let bindings = parse_bindings bindings
                let e = p_expr e
                let cont = p_expr cont
                Syntax.LetTuple(bindings, e, cont)
            | [ SExpr.SExprList(SExpr.SExprId [ ',' ] :: bindings)
                SExpr.SExprId [ ':' ]
                SExpr.SExprList(SExpr.SExprId [ ',' ] :: binding_ts)
                SExpr.SExprId [ '=' ]
                e
                SExpr.SExprId [ 'i'; 'n' ]
                cont ] when bindings.Length = binding_ts.Length ->
                let bindings = parse_bindings bindings
                let binding_ts = binding_ts |> List.map parse_t
                let bindings = (bindings, binding_ts) ||> List.map2 (fun a t -> (fst a, t))
                let e = p_expr e
                let cont = p_expr cont
                Syntax.LetTuple(bindings, e, cont)
            | _ -> failwith "incorrect 'let' form found"
        | SExpr.SExprList(SExpr.SExprId [ 'l'; 'e'; 't'; '-'; 'r'; 'e'; 'c' ] :: exs) ->
            match exs with
            | [ SExpr.SExprList(SExpr.SExprId name :: args)
                SExpr.SExprId [ '=' ]
                body
                SExpr.SExprId [ 'i'; 'n' ]
                cont ] ->
                match args with
                | [] -> failwith "let-rec with 0 args are not supported"
                | _ ->
                    let bindings = parse_bindings args
                    let binding_ts = bindings |> List.map snd

                    Syntax.LetRecNode(
                        { name = (make_id name, Type.FunType(binding_ts, Type.gen_empty ()))
                          args = bindings
                          body = p_expr body },
                        p_expr cont
                    )
            | [ SExpr.SExprList(SExpr.SExprId name :: args)
                SExpr.SExprId [ ':' ]
                SExpr.SExprList arg_types
                SExpr.SExprId [ '-'; '>' ]
                ret_type
                SExpr.SExprId [ '=' ]
                body
                SExpr.SExprId [ 'i'; 'n' ]
                cont ] when args.Length = arg_types.Length ->
                match args with
                | [] -> failwith "let-rec with 0 args are not supported"
                | _ ->
                    let args = parse_bindings args
                    let arg_types = arg_types |> List.map parse_t
                    let args = (args, arg_types) ||> List.map2 (fun a t -> (fst a, t))
                    let ret_type = ret_type |> parse_t
                    let name_id = make_id name
                    let f_type = Type.FunType(arg_types, ret_type)

                    Syntax.LetRecNode(
                        { name = (name_id, f_type)
                          args = args
                          body = p_expr body },
                        p_expr cont
                    )
            | _ -> failwith "incorrect 'let-rec' form found"
        | SExpr.SExprList(SExpr.SExprId(';' :: _) :: es) ->
            let rec unwind =
                function
                | [ e ] -> p_expr e
                | e :: cont ->
                    let e = p_expr e
                    let id = Id.gen_tmp Type.UnitType
                    let cont = unwind cont
                    Syntax.LetNode((id, Type.UnitType), e, cont)
                | _ -> failwith "sequence of 0 statements are not supported"

            unwind es
        | SExpr.SExprList [ _ ] -> failwith "apply with 0 args are not supported"
        | SExpr.SExprList(foo :: args) ->
            let foo = p_expr foo
            let args = args |> List.map p_expr
            Syntax.ApplyNode(foo, args)

    let f s_expr = p_expr s_expr
