module mini_caml_fsharp_core.BetaReduction

open mini_caml_fsharp_core.Id
open mini_caml_fsharp_core.KNorm
open mini_caml_fsharp_core.M

module BetaReduction =
    /// Осуществляет Beta-редукцию для Let выражений.
    /// Выражение вида (let y = x in $expr ) редуцируется
    /// до $expr с подстановкой x вместо y. Подстановка
    /// происходит рекурсивно, то есть в случае выражения
    /// ( let z = (let y = x in y) in (+ z x) ) сначала
    /// будет получено ( let z = x in (+ z x) ), а потом
    /// уже это выражение средуцируется до (+ x x)
    let rec private beta_reduce (env: Id.t M) e =
        let find x =
            match env.TryFind x with
            | Some shadow_x -> shadow_x
            | _ -> x

        match e with
        // Большинство кода дословно повторяет такой же в AlphaConv:
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
        | KNorm.Apply(callee, args) -> KNorm.Apply(find callee, args |> List.map find)
        | KNorm.ExtFunApply(callee, args) -> KNorm.ExtFunApply(find callee, args |> List.map find)
        | KNorm.BranchEq(l, r, e1, e2) -> KNorm.BranchEq(find l, find r, e1 |> beta_reduce env, e2 |> beta_reduce env)
        | KNorm.BranchLE(l, r, e1, e2) -> KNorm.BranchLE(find l, find r, e1 |> beta_reduce env, e2 |> beta_reduce env)

        // В Let редуцируем сначала binding, и если он упростился до
        // ссылки на переменную, добавляем в окружение соответствующую
        // перегрузку для имени и редуцируем cont уже с этим окружением.
        // Таким образом, достигается желаемая подстановка
        | KNorm.Let((id, t), binding, cont) ->
            match binding |> beta_reduce env with
            | KNorm.Var id' ->
                let env' = env.Add id id'
                cont |> beta_reduce env'
            | binding' ->
                let cont' = cont |> beta_reduce env
                KNorm.Let((id, t), binding', cont')

        // Просто делаем рекурсивные вызовы, так как к сожалению повторить
        // логику как у формы Let либо невозможно либо очень сложно
        | KNorm.LetTuple(bs, binding, cont) ->
            let binding' = find binding
            let cont' = cont |> beta_reduce env
            KNorm.LetTuple(bs, binding', cont')

        // В случае формы LetRec только рекурсивный вызов для дочерних выражений
        | KNorm.LetRec({ name = xt; args = yts; body = e1 }, e2) ->
            let e1' = e1 |> beta_reduce env
            let e2' = e2 |> beta_reduce env
            KNorm.LetRec({ name = xt; args = yts; body = e1' }, e2')

    let f e = e |> beta_reduce (M.Empty())
