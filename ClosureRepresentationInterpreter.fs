module mini_caml_fsharp.ClosureRepresentationInterpreter

open mini_caml_fsharp.InterpreterShared
open mini_caml_fsharp.ClosureRepresentation
open mini_caml_fsharp.M
open mini_caml_fsharp.Id

module ClosureRepresentationInterpreter =
    type TopLevelM = (ClosureRepresentation.t InterpreterShared.func_t * string list) M
    type value_t = ClosureRepresentation.t InterpreterShared.value_t

    let rec interpret_expr (top_level_env: TopLevelM) env e =
        let lookup_var var_name =
            env |> InterpreterShared.lookup_var var_name

        let lookup_i var_name =
            env |> InterpreterShared.lookup_i var_name

        let lookup_f var_name =
            env |> InterpreterShared.lookup_f var_name

        let lookup_arr var_name =
            env |> InterpreterShared.lookup_arr var_name

        let lookup_tuple var_name =
            env |> InterpreterShared.lookup_tuple var_name

        let lookup_closure var_name =
            env |> InterpreterShared.lookup_fn var_name

        match e with
        // Простые литералы:
        | ClosureRepresentation.Unit -> value_t.Unit
        | ClosureRepresentation.Int i -> value_t.Int i
        | ClosureRepresentation.Float f -> value_t.Float f
        | ClosureRepresentation.Var var_name -> lookup_var var_name
        | ClosureRepresentation.Tuple ops -> value_t.Tuple(ops |> List.map lookup_var)

        // Операции над целыми:
        | ClosureRepresentation.Neg op -> value_t.Int -(lookup_i op)
        | ClosureRepresentation.Add(lhs, rhs) -> value_t.Int(lookup_i lhs + lookup_i rhs)
        | ClosureRepresentation.Sub(lhs, rhs) -> value_t.Int(lookup_i lhs - lookup_i rhs)

        // Операции над числами с плавающей запятой:
        | ClosureRepresentation.FNeg op -> value_t.Float -(lookup_f op)
        | ClosureRepresentation.FAdd(lhs, rhs) -> value_t.Float(lookup_f lhs + lookup_f rhs)
        | ClosureRepresentation.FSub(lhs, rhs) -> value_t.Float(lookup_f lhs - lookup_f rhs)
        | ClosureRepresentation.FMul(lhs, rhs) -> value_t.Float(lookup_f lhs * lookup_f rhs)
        | ClosureRepresentation.FDiv(lhs, rhs) -> value_t.Float(lookup_f lhs / lookup_f rhs)

        // Операции над массивами:
        | ClosureRepresentation.Get(arr_name, ix) ->
            let arr = lookup_arr arr_name
            let ix = lookup_i ix
            arr[int ix]
        | ClosureRepresentation.Put(arr_name, ix, v) ->
            let arr = lookup_arr arr_name
            let ix = lookup_i ix
            arr[int ix] <- lookup_var v
            value_t.Unit

        // Ветвления:
        | ClosureRepresentation.BranchEq(lhs, rhs, then_e, else_e) ->
            let cmp_op = InterpreterShared.EQ

            match (lookup_var lhs, lookup_var rhs) ||> InterpreterShared.cmp_values cmp_op with
            | true -> then_e |> interpret_expr top_level_env env
            | false -> else_e |> interpret_expr top_level_env env
        | ClosureRepresentation.BranchLE(lhs, rhs, then_e, else_e) ->
            let cmp_op = InterpreterShared.LE

            match (lookup_var lhs, lookup_var rhs) ||> InterpreterShared.cmp_values cmp_op with
            | true -> then_e |> interpret_expr top_level_env env
            | false -> else_e |> interpret_expr top_level_env env

        // Связывания имён:
        | ClosureRepresentation.Let((name, _), body, cont) ->
            let res = body |> interpret_expr top_level_env env
            let env' = env.Add name res
            cont |> interpret_expr top_level_env env'
        | ClosureRepresentation.LetTuple(bs, var_name, cont) ->
            let values = lookup_tuple var_name
            let bound_names = bs |> List.map fst
            let mutable env' = env

            for b_name, v in (List.zip bound_names values) do
                env' <- env'.Add b_name v

            cont |> interpret_expr top_level_env env'

        | ClosureRepresentation.LetClosure((name, _), Id.L label, cont) ->
            match top_level_env.TryFind label with
            | None -> failwithf $"toplevel function with label %s{label}  not found"
            | Some(fn, free_vars) ->
                let mutable fn_env' = fn.env

                for free_var_name in free_vars do
                    let v = lookup_var free_var_name
                    fn_env' <- fn_env'.Add free_var_name v

                let env' = env.Add name (value_t.Func { fn with env = fn_env' })
                cont |> interpret_expr top_level_env env'

        | ClosureRepresentation.ApplyClosure(func_name, args) ->
            let fn = lookup_closure func_name
            let mutable env' = fn.env.Add fn.recursive_name (value_t.Func fn)

            for name, v in (List.zip fn.arg_names args) do
                let v' = lookup_var v
                env' <- env'.Add name v'

            fn.body |> interpret_expr top_level_env env'

        | ClosureRepresentation.ApplyDirect(Id.L label, args) ->
            match top_level_env.TryFind label with
            | Some(fn, _) ->
                let mutable env' = fn.env

                for name, v in (List.zip fn.arg_names args) do
                    let v' = lookup_var v
                    env' <- env'.Add name v'

                fn.body |> interpret_expr top_level_env env'
            | None ->
                match label, args with
                | "min_caml_create_float_array", [ count; v ] ->
                    let count = lookup_i count
                    let v = lookup_f v
                    let arr = Array.create (int count) (value_t.Float v)
                    value_t.Array arr
                | "min_caml_create_array", [ count; v ] ->
                    let count = lookup_i count
                    let v = lookup_var v
                    let arr = Array.create (int count) v
                    value_t.Array arr
                | "min_caml_clone", [v] ->
                    let v = lookup_var v
                    InterpreterShared.clone v
                | _ -> failwithf $"toplevel function with label %s{label}  not found"

    let f (p: ClosureRepresentation.program) =
        let env = M.Empty()
        let mutable top_level_env = M.Empty()

        for fn_definition in p.top_level_functions do
            let Id.L name, _ = fn_definition.name
            let free_vars = fn_definition.free_vars |> List.map fst

            let fn: _ InterpreterShared.func_t =
                { recursive_name = name
                  arg_names = fn_definition.args |> List.map fst
                  env = M.Empty()
                  body = fn_definition.body }

            top_level_env <- top_level_env.Add name (fn, free_vars)

        p.main |> interpret_expr top_level_env env
