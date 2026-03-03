module mini_caml_fsharp_core.CommonSubElim

open mini_caml_fsharp_core.Id
open mini_caml_fsharp_core.KNorm
open mini_caml_fsharp_core.M

/// Простая оптимизация, которая старается переиспользовать
/// для повторяющегося подвыражения аналогичное подвыражение,
/// введённое ранее. Реализовано только для простых арифметических
/// выражений и для повторяющихся литералов
module CommonSubElim =
    let private get_sig =
        function
        | KNorm.Neg(op) -> Some($"-(%s{op})")
        | KNorm.Add(lhs, rhs) when lhs <= rhs -> Some($"+(%s{lhs},%s{rhs})")
        // b + a эквивалентно a + b, приводим их к единой сигнатуре
        | KNorm.Add(lhs, rhs) -> Some($"+(%s{rhs},%s{lhs})")
        | KNorm.Sub(lhs, rhs) -> Some($"-(%s{lhs},%s{rhs})")
        | KNorm.FNeg(op) -> Some($"-.(%s{op})")
        | KNorm.FAdd(lhs, rhs) when lhs <= rhs -> Some($"+.(%s{lhs},%s{rhs})")
        // b +. a эквивалентно a +. b, приводим их к единой сигнатуре
        | KNorm.FAdd(lhs, rhs) -> Some($"+.(%s{rhs},%s{lhs})")
        | KNorm.FSub(lhs, rhs) -> Some($"-.(%s{lhs},%s{rhs})")
        | KNorm.FMul(lhs, rhs) when lhs <= rhs -> Some($"*.(%s{lhs},%s{rhs})")
        // b *. a эквивалентно a *. b, приводим их к единой сигнатуре
        | KNorm.FMul(lhs, rhs) -> Some($"*.(%s{rhs},%s{lhs})")
        | KNorm.FDiv(lhs, rhs) -> Some($"/.(%s{lhs},%s{rhs})")
        | KNorm.Unit -> Some("()")
        | KNorm.Int i -> Some($"int:{i}")
        | KNorm.Float f -> Some($"float:%f{f}")
        | KNorm.Tuple ids -> Some($"tup:%A{ids}")
        | _ -> None
            
    let rec private cse (env : Id.t M ) k =
        match get_sig k |> Option.bind env.TryFind with
        | Some var -> KNorm.Var var
        | None ->
            match k with
            | KNorm.Let((bound_name, t), binding, cont) ->
                match get_sig binding with
                | None -> KNorm.Let((bound_name, t), binding, cont |> cse env)
                | Some s -> 
                    match env.TryFind s with
                    | Some var -> KNorm.Let((bound_name, t), KNorm.Var var, cont |> cse env)
                    | None ->
                        let env' = env.Add s bound_name
                        KNorm.Let((bound_name, t), binding, cont |> cse env')
            | KNorm.LetTuple(ids, binding, cont) -> KNorm.LetTuple(ids, binding, cont |> cse env)
            | KNorm.LetRec(f_def, cont) -> KNorm.LetRec(f_def, cont |> cse env)
            | KNorm.BranchEq(x, y, e1, e2) -> KNorm.BranchEq(x, y, e1 |> cse env, e2 |> cse env)
            | KNorm.BranchLE(x, y, e1, e2) -> KNorm.BranchLE(x, y, e1 |> cse env, e2 |> cse env)
            | _ -> k
    
    let f k = k |> cse (M.Empty ())