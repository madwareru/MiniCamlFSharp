module mini_caml_fsharp.CmmDeclosuredInterpreter

open mini_caml_fsharp.Id
open mini_caml_fsharp.M
open mini_caml_fsharp.InterpreterShared
open mini_caml_fsharp.CmmDeclosured

// Данный интерпретатор практически дословно повторяет общие с Cmm части
module CmmDeclosuredInterpreter =
    type TopLevelM = CmmDeclosured.fn_t M

    type value_t =
        | Unit
        | Int of int64
        | Float of double
        | FunctionPtr of Id.l
        | Memory of value_t array

    let rec private cmp_values cmp l r =
        match l, r with
        | value_t.Unit, value_t.Unit -> true
        | value_t.Int lhs, value_t.Int rhs ->
            match cmp with
            | InterpreterShared.EQ -> lhs = rhs
            | InterpreterShared.LE -> lhs <= rhs
        | value_t.Float lhs, value_t.Float rhs ->
            match cmp with
            | InterpreterShared.EQ -> lhs = rhs
            | InterpreterShared.LE -> lhs <= rhs
        | value_t.Memory l_vs, value_t.Memory r_vs ->
            let failed = (l_vs, r_vs) ||> Array.exists2 (fun l r -> not (cmp_values cmp l r))
            not failed
        // функции сравниваются только на равенство
        | value_t.FunctionPtr f_l, value_t.FunctionPtr f_r when cmp = InterpreterShared.EQ -> f_l = f_r
        | _ -> failwith "can't compare incompatible types"
        
    let private lookup_var var_name (env: value_t M) =
        match env.TryFind var_name with
        | Some v -> v
        | _ -> failwithf $"name %s{var_name} not found in an environment!"

    let private lookup_i var_name (env: value_t M) =
        match env.TryFind var_name with
        | Some(value_t.Int i) -> i
        | _ -> failwithf $"name %s{var_name} with type i not found in an environment!"

    let private lookup_f var_name (env: value_t M) =
        match env.TryFind var_name with
        | Some(value_t.Float f) -> f
        | _ -> failwithf $"name %s{var_name} with type f not found in an environment!"

    let private lookup_mem var_name (env: value_t M) =
        match env.TryFind var_name with
        | Some(value_t.Memory mem) -> mem
        | _ -> failwith $"name %s{var_name} with type memory not found in an environment!"

    let rec private interpret_block (top_level_env: TopLevelM) (env: value_t M) (b: CmmDeclosured.block_t) =
        match b with
        | CmmDeclosured.Seq(statement, next_block) ->
            match statement with
            | CmmDeclosured.Assignment((name, _), e) ->
                let env' = env.Add name (e |> interpret_exp top_level_env env)
                next_block |> interpret_block top_level_env env'
        | CmmDeclosured.Return e -> e |> interpret_exp top_level_env env
        
    and private interpret_exp (top_level_env: TopLevelM) env (e: CmmDeclosured.expr_t) =
        let lookup_var var_name = env |> lookup_var var_name

        let lookup_i var_name = env |> lookup_i var_name

        let lookup_f var_name = env |> lookup_f var_name

        let lookup_mem var_name = env |> lookup_mem var_name

        let interpret_atom_exp =
            function
            | CmmDeclosured.Unit -> value_t.Unit
            | CmmDeclosured.Int i -> value_t.Int i
            | CmmDeclosured.Float f -> value_t.Float f
            | CmmDeclosured.FunctionPtr l -> value_t.FunctionPtr l

        match e with
        | CmmDeclosured.Atom atom_expr -> atom_expr |> interpret_atom_exp
        | CmmDeclosured.Var v -> lookup_var v

        // Операции над целыми:
        | CmmDeclosured.Neg op -> value_t.Int -(lookup_i op)
        | CmmDeclosured.Add(lhs, rhs) -> value_t.Int(lookup_i lhs + lookup_i rhs)
        | CmmDeclosured.Sub(lhs, rhs) -> value_t.Int(lookup_i lhs - lookup_i rhs)

        // Операции над числами с плавающей запятой:
        | CmmDeclosured.FNeg op -> value_t.Float -(lookup_f op)
        | CmmDeclosured.FAdd(lhs, rhs) -> value_t.Float(lookup_f lhs + lookup_f rhs)
        | CmmDeclosured.FSub(lhs, rhs) -> value_t.Float(lookup_f lhs - lookup_f rhs)
        | CmmDeclosured.FMul(lhs, rhs) -> value_t.Float(lookup_f lhs * lookup_f rhs)
        | CmmDeclosured.FDiv(lhs, rhs) -> value_t.Float(lookup_f lhs / lookup_f rhs)

        // Операции над памятью
        | CmmDeclosured.ExternalMemory _ -> failwith "todo: external memory"
        | CmmDeclosured.MemoryGet(mem_name, ix) ->
            let mem = lookup_mem mem_name
            let ix = lookup_i ix
            mem[int ix]
        | CmmDeclosured.MemoryPut(mem_name, ix, v) ->
            let mem = lookup_mem mem_name
            let ix = lookup_i ix
            mem[int ix] <- lookup_var v
            value_t.Unit

        // Ветвления
        | CmmDeclosured.BranchEq(lhs, rhs, then_block, else_block) ->
            let cmp_op = InterpreterShared.EQ

            match (lookup_var lhs, lookup_var rhs) ||> cmp_values cmp_op with
            | true -> then_block |> interpret_block top_level_env env
            | false -> else_block |> interpret_block top_level_env env
        | CmmDeclosured.BranchLE(lhs, rhs, then_block, else_block) ->
            let cmp_op = InterpreterShared.LE

            match (lookup_var lhs, lookup_var rhs) ||> cmp_values cmp_op with
            | true -> then_block |> interpret_block top_level_env env
            | false -> else_block |> interpret_block top_level_env env

        | CmmDeclosured.Apply(Id.L label, args) ->
            match top_level_env.TryFind label with
            | Some(fn) ->
                let arg_names = fn.args |> List.map fst
                let mutable env' = M.Empty()

                for name, v in (List.zip arg_names args) do
                    let v' = lookup_var v
                    env' <- env'.Add name v'

                fn.body |> interpret_block top_level_env env'
            | None ->
                match label, args with
                | "min_caml_alloc_vector", [ count ] ->
                    let count = lookup_i count
                    let mem = Array.create (int count) value_t.Unit
                    value_t.Memory mem
                | "min_caml_create_float_array", [ count; v ] ->
                    let count = lookup_i count
                    let v = lookup_f v
                    let mem = Array.create (int count) (value_t.Float v)
                    value_t.Memory mem
                | "min_caml_create_array", [ count; v ] ->
                    let count = lookup_i count
                    let v = lookup_var v
                    let mem = Array.create (int count) v
                    value_t.Memory mem
                | _ -> failwithf $"toplevel function with label %s{label}  not found"
    
    let f (p: CmmDeclosured.program_t) =
        let env = M.Empty()
        let mutable (top_level_env: TopLevelM) = M.Empty()

        for fn_definition in p.top_level_functions do
            let Id.L name, _ = fn_definition.name
            top_level_env <- top_level_env.Add name fn_definition

        p.entry |> interpret_block top_level_env env
