module mini_caml_fsharp.InterpreterShared

open mini_caml_fsharp.M

module InterpreterShared =
    type 't value_t =
        | Unit
        | Int of int64
        | Float of double
        | Tuple of 't value_t list
        | Array of 't value_t array
        | Func of 't func_t

    and 't func_t =
        { recursive_name: string
          arg_names: string list
          env: 't value_t M
          body: 't }

    type Comparison =
        | EQ
        | LE

    let rec cmp_values cmp l r =
        match l, r with
        | value_t.Unit, value_t.Unit -> true
        | value_t.Int lhs, value_t.Int rhs ->
            match cmp with
            | EQ -> lhs = rhs
            | LE -> lhs <= rhs
        | value_t.Float lhs, value_t.Float rhs ->
            match cmp with
            | EQ -> lhs = rhs
            | LE -> lhs <= rhs
        | value_t.Tuple l_vs, value_t.Tuple r_vs ->
            let failed = (l_vs, r_vs) ||> List.exists2 (fun l r -> not (cmp_values cmp l r))
            not failed
        | value_t.Array l_vs, value_t.Array r_vs ->
            let failed = (l_vs, r_vs) ||> Array.exists2 (fun l r -> not (cmp_values cmp l r))
            not failed
        // функции сравниваются только на равенство и только ссылочно
        | value_t.Func f_l, value_t.Func f_r when cmp = EQ -> System.Object.ReferenceEquals(f_l, f_r)
        | _ -> failwith "can't compare incompatible types"

    let lookup_var var_name (env: 't value_t M) =
        match env.TryFind var_name with
        | Some v -> v
        | _ -> failwithf $"name %s{var_name} not found in an environment!"

    let lookup_i var_name (env: 't value_t M) =
        match env.TryFind var_name with
        | Some(value_t.Int i) -> i
        | _ -> failwithf $"name %s{var_name} with type i not found in an environment!"

    let lookup_f var_name (env: 't value_t M) =
        match env.TryFind var_name with
        | Some(value_t.Float f) -> f
        | _ -> failwithf $"name %s{var_name} with type f not found in an environment!"

    let lookup_arr var_name (env: 't value_t M) =
        match env.TryFind var_name with
        | Some(value_t.Array arr) -> arr
        | _ -> failwith $"name %s{var_name} with type [] not found in an environment!"

    let lookup_tuple var_name (env: 't value_t M) =
        match env.TryFind var_name with
        | Some(value_t.Tuple values) -> values
        | _ -> failwith $"name %s{var_name} with type (,) not found in an environment!"

    let lookup_fn var_name (env: 't value_t M) =
        match env.TryFind var_name with
        | Some(value_t.Func fn) -> fn
        | _ -> failwith $"name %s{var_name} with type fn not found in an environment!"
