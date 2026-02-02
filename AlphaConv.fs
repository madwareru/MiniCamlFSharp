module mini_caml_fsharp.AlphaConv

open Microsoft.FSharp.Collections
open mini_caml_fsharp.Id
open mini_caml_fsharp.KNorm
open mini_caml_fsharp.M

module AlphaConv =
    /// Принимает идентификатор и окружение, кладёт в окружение этот
    /// идентификатор с добавлением уникального суффикса, после чего
    /// возвращает наружу пару из окружения и нового идентификатора
    let add_alpha_binding x (env : Id.t M) =
        let x' = Id.gen_id x
        env.Add x x', x'
        
    /// Принимает список идентификаторов и окружение, формирует список
    /// идентификаторов с уникальным суфиксом, добавляет в окружение
    /// все новые идентификаторы, после чего возвращает наружу пару
    /// из окружения и списка новых идентификаторов 
    let add_alpha_bindings xs (env : Id.t M) =
        let xs' = xs |> List.map Id.gen_id
        env.AddList2 xs xs', xs'
        
    /// Делает преобразование выражения из K-нормальной формы
    /// в K-нормальную форму с добавлением уникальности всем
    /// связанным именам
    let rec alpha_convert (env : Id.t M) e =
        /// Вспомогательная функция для поиска затенения
        /// для идентификатора в окружении. Если в окружении
        /// затенения нет -> возвращается исходный идентификатор
        let find x =
            match env.TryFind x with
            | Some shadow_x -> shadow_x
            | _ -> x
        
        match e with
        | KNorm.Unit
        | KNorm.Int _
        | KNorm.Float _ -> e
        | KNorm.Var id -> KNorm.Var(find id)
        | KNorm.Tuple ids -> KNorm.Tuple(ids |> List.map find)
        | KNorm.Neg op -> KNorm.Neg(find op)
        | KNorm.Add(l, r) -> KNorm.Add(find l, find r)
        | KNorm.Sub(l, r) -> KNorm.Sub(find l, find r)
        | KNorm.FNeg op -> KNorm.FNeg(find op)
        | KNorm.FAdd(l, r) -> KNorm.FAdd(find l, find r)
        | KNorm.FSub(l, r) -> KNorm.FSub(find l, find r)
        | KNorm.FMul(l, r) -> KNorm.FMul(find l, find r)
        | KNorm.FDiv(l, r) -> KNorm.FDiv(find l, find r)
        | KNorm.Get(a, i) -> KNorm.Get(find a, find i)
        | KNorm.Put(a, i, v) -> KNorm.Put(find a, find i, find v)
        | KNorm.ExtArray id -> KNorm.ExtArray(find id)
        | KNorm.Apply(callee, args) ->
            KNorm.Apply(
                find callee,
                args |> List.map find
            )
        | KNorm.ExtFunApply(callee, args) ->
            KNorm.ExtFunApply(
                find callee,
                args |> List.map find
            )
        | KNorm.BranchEq(l, r, e1, e2) ->
            KNorm.BranchEq(
                find l,
                find r,
                e1 |> alpha_convert env,
                e2 |> alpha_convert env
            )
        | KNorm.BranchLE(l, r, e1, e2) ->
            KNorm.BranchLE(
                find l,
                find r,
                e1 |> alpha_convert env,
                e2 |> alpha_convert env
            )
        | KNorm.Let((id, t), binding, cont) ->
            let env', id' = env |> add_alpha_binding id
            let binding' = binding |> alpha_convert env
            let cont' = cont |> alpha_convert env'
            KNorm.Let((id', t), binding', cont')
        | KNorm.LetTuple(bs, binding, cont) ->
            let binding' = find binding
            let ids, ts = bs |> List.unzip
            let env', ids' = env |> add_alpha_bindings ids
            let cont' = cont |> alpha_convert env'
            KNorm.LetTuple(List.zip ids' ts, binding', cont')
        | KNorm.LetRec({ name = (name, ret_t); args = args; body = body }, cont) ->
            let env', name' = env |> add_alpha_binding name
            let ids, ts = args |> List.unzip
            let env'', ids' = env' |> add_alpha_bindings ids
            let body' = body |> alpha_convert env''
            let cont' = cont |> alpha_convert env'
            let args' = List.zip ids' ts
            KNorm.LetRec({ name = (name', ret_t); args = args'; body = body' }, cont')
    let f e = e |> alpha_convert (M.Empty ())