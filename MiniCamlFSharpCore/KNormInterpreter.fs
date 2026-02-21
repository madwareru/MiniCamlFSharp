module mini_caml_fsharp_core.KNormInterpreter

open System
open mini_caml_fsharp_core.KNorm
open mini_caml_fsharp_core.M
open mini_caml_fsharp_core.InterpreterShared

/// Интерпретатор для K-нормальной формы.
/// Предполагается, что типы корректно выведены
/// на этапе Typing, поэтому при исполнении
/// дополнительные проверки не осуществляются
module KNormInterpreter =
    type value_t = KNorm.t InterpreterShared.value_t

    let rec private interpret (env: value_t M) (e: KNorm.t) =
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

        let lookup_fn var_name =
            env |> InterpreterShared.lookup_fn var_name

        match e with
        // Простые литералы:
        | KNorm.Unit -> value_t.Unit
        | KNorm.Int i -> value_t.Int i
        | KNorm.Float f -> value_t.Float f
        | KNorm.Var var_name -> lookup_var var_name
        | KNorm.Tuple ops -> value_t.Tuple(ops |> List.map lookup_var)

        // Операции над целыми:
        | KNorm.Neg op -> value_t.Int -(lookup_i op)
        | KNorm.Add(lhs, rhs) -> value_t.Int(lookup_i lhs + lookup_i rhs)
        | KNorm.Sub(lhs, rhs) -> value_t.Int(lookup_i lhs - lookup_i rhs)

        // Операции над числами с плавающей запятой:
        | KNorm.FNeg op -> value_t.Float -(lookup_f op)
        | KNorm.FAdd(lhs, rhs) -> value_t.Float(lookup_f lhs + lookup_f rhs)
        | KNorm.FSub(lhs, rhs) -> value_t.Float(lookup_f lhs - lookup_f rhs)
        | KNorm.FMul(lhs, rhs) -> value_t.Float(lookup_f lhs * lookup_f rhs)
        | KNorm.FDiv(lhs, rhs) -> value_t.Float(lookup_f lhs / lookup_f rhs)

        // Операции над массивами:
        | KNorm.Get(arr_name, ix) ->
            let arr = lookup_arr arr_name
            let ix = lookup_i ix
            arr[int ix]
        | KNorm.Put(arr_name, ix, v) ->
            let arr = lookup_arr arr_name
            let ix = lookup_i ix
            arr[int ix] <- lookup_var v
            value_t.Unit

        // Ветвления:
        | KNorm.BranchEq(lhs, rhs, then_e, else_e) ->
            let cmp_op = InterpreterShared.EQ

            match (lookup_var lhs, lookup_var rhs) ||> InterpreterShared.cmp_values cmp_op with
            | true -> then_e |> interpret env
            | false -> else_e |> interpret env
        | KNorm.BranchLE(lhs, rhs, then_e, else_e) ->
            let cmp_op = InterpreterShared.LE

            match (lookup_var lhs, lookup_var rhs) ||> InterpreterShared.cmp_values cmp_op with
            | true -> then_e |> interpret env
            | false -> else_e |> interpret env

        // Связывания имён:
        | KNorm.Let((name, _), body, cont) ->
            let res = body |> interpret env
            let env' = env.Add name res
            cont |> interpret env'
        | KNorm.LetTuple(bs, var_name, cont) ->
            let values = lookup_tuple var_name
            let bound_names = bs |> List.map fst
            let mutable env' = env

            for b_name, v in (List.zip bound_names values) do
                env' <- env'.Add b_name v

            cont |> interpret env'
        | KNorm.LetRec({ name = (name, _)
                         args = args
                         body = body },
                       cont) ->
            let arg_names = args |> List.map fst

            let v =
                value_t.Func
                    { recursive_name = name
                      arg_names = arg_names
                      env = env
                      body = body }

            let env' = env.Add name v
            cont |> interpret env'

        // Применения функций:
        | KNorm.Apply(func_name, args) ->
            let fn = lookup_fn func_name
            let mutable env' = fn.env.Add fn.recursive_name (value_t.Func fn)

            for name, v in (List.zip fn.arg_names args) do
                let v' = lookup_var v
                env' <- env'.Add name v'

            fn.body |> interpret env'
        | KNorm.ExtFunApply(func_name, args) ->
            match func_name, args with
            | "create_float_array", [ count; v ] ->
                let count = lookup_i count
                let v = lookup_f v
                let arr = Array.create (int count) (value_t.Float v)
                value_t.Array arr
            | "create_array", [ count; v ] ->
                let count = lookup_i count
                let v = lookup_var v
                let arr = Array.create (int count) v
                value_t.Array arr
            | "clone", [ v ] ->
                let v = lookup_var v
                InterpreterShared.clone v
            | _ -> failwith "unknown external function"

    let f e = interpret (M.Empty()) e
