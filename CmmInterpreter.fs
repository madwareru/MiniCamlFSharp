module mini_caml_fsharp.CmmInterpreter

open mini_caml_fsharp.Id
open mini_caml_fsharp.M
open mini_caml_fsharp.InterpreterShared
open mini_caml_fsharp.Cmm

module CmmInterpreter =
    type TopLevelM = Cmm.fn_t M

    type value_t =
        | Unit
        | Int of int64
        | Float of double
        | Memory of value_t array
        
    let rec clone (v : value_t) =
        match v with
        | Memory mem ->Memory (mem |> Array.map clone)
        | _ -> v 

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

    let rec private interpret_block (top_level_env: TopLevelM) (env: value_t M) (b: Cmm.block_t) =
        match b with
        | Cmm.Seq(statement, next_block) ->
            match statement with
            | Cmm.Assignment((name, _), e) ->
                let env' = env.Add name (e |> interpret_exp top_level_env env)
                next_block |> interpret_block top_level_env env'
        | Cmm.Return e -> e |> interpret_exp top_level_env env

    and private interpret_exp (top_level_env: TopLevelM) env (e: Cmm.expr_t) =
        let lookup_var var_name = env |> lookup_var var_name

        let lookup_i var_name = env |> lookup_i var_name

        let lookup_f var_name = env |> lookup_f var_name

        let lookup_mem var_name = env |> lookup_mem var_name

        let interpret_atom_exp =
            function
            | Cmm.Unit -> value_t.Unit
            | Cmm.Int i -> value_t.Int i
            | Cmm.Float f -> value_t.Float f

        match e with
        | Cmm.Atom atom_expr -> atom_expr |> interpret_atom_exp
        | Cmm.Var v -> lookup_var v

        // Операции над целыми:
        | Cmm.Neg op -> value_t.Int -(lookup_i op)
        | Cmm.Add(lhs, rhs) -> value_t.Int(lookup_i lhs + lookup_i rhs)
        | Cmm.Sub(lhs, rhs) -> value_t.Int(lookup_i lhs - lookup_i rhs)

        // Операции над числами с плавающей запятой:
        | Cmm.FNeg op -> value_t.Float -(lookup_f op)
        | Cmm.FAdd(lhs, rhs) -> value_t.Float(lookup_f lhs + lookup_f rhs)
        | Cmm.FSub(lhs, rhs) -> value_t.Float(lookup_f lhs - lookup_f rhs)
        | Cmm.FMul(lhs, rhs) -> value_t.Float(lookup_f lhs * lookup_f rhs)
        | Cmm.FDiv(lhs, rhs) -> value_t.Float(lookup_f lhs / lookup_f rhs)

        // Операции над памятью
        | Cmm.ExternalMemory _ -> failwith "todo: external memory"
        | Cmm.MemoryGet(mem_name, ix) ->
            let mem = lookup_mem mem_name
            let ix = lookup_i ix
            mem[int ix]
        | Cmm.MemoryPut(mem_name, ix, v) ->
            let mem = lookup_mem mem_name
            let ix = lookup_i ix
            mem[int ix] <- lookup_var v
            value_t.Unit

        // Ветвления
        | Cmm.BranchEq(lhs, rhs, then_block, else_block) ->
            let cmp_op = InterpreterShared.EQ

            match (lookup_var lhs, lookup_var rhs) ||> cmp_values cmp_op with
            | true -> then_block |> interpret_block top_level_env env
            | false -> else_block |> interpret_block top_level_env env
        | Cmm.BranchLE(lhs, rhs, then_block, else_block) ->
            let cmp_op = InterpreterShared.LE

            match (lookup_var lhs, lookup_var rhs) ||> cmp_values cmp_op with
            | true -> then_block |> interpret_block top_level_env env
            | false -> else_block |> interpret_block top_level_env env

        | Cmm.ApplyDirect(Id.L label, args) ->
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
                | "min_caml_clone", [v] ->
                    let v = lookup_var v
                    clone v
                | _ -> failwithf $"toplevel function with label %s{label}  not found"
        | Cmm.ApplyClosure(label, args) ->
            let fn_mem = lookup_mem label
            match top_level_env.TryFind label with
            | Some(fn) ->
                // восстанавливаем биндинги к свободным переменным из полученного куска памяти
                let mutable free_var_bindings = []

                for i in 0 .. (fn.free_vars.Length - 1) do
                    free_var_bindings <- fn_mem[int i] :: free_var_bindings

                free_var_bindings <- free_var_bindings |> List.rev

                // формируем окружение
                let mutable env' = M.Empty()

                // Добавляем в окружение значения для свободных переменных
                let free_var_names = fn.free_vars |> List.map fst

                for name, v in (List.zip free_var_names free_var_bindings) do
                    env' <- env'.Add name v

                // Добавляем заново наше замыкание, для того, чтобы корректно работала рекурсия
                env' <- env'.Add label (value_t.Memory fn_mem)

                // Добавляем аргументы
                let arg_names = fn.args |> List.map fst

                for name, v in (List.zip arg_names args) do
                    let v' = lookup_var v
                    env' <- env'.Add name v'

                fn.body |> interpret_block top_level_env env'
            | _ -> failwithf $"toplevel function with label %s{label}  not found"

    let f (p: Cmm.program_t) =
        let env = M.Empty()
        let mutable (top_level_env: TopLevelM) = M.Empty()

        for fn_definition in p.top_level_functions do
            let Id.L name, _ = fn_definition.name
            top_level_env <- top_level_env.Add name fn_definition

        p.entry |> interpret_block top_level_env env
