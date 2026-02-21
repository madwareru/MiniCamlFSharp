module mini_caml_fsharp_core.ConstFolding

open mini_caml_fsharp_core.KNorm
open mini_caml_fsharp_core.M

module ConstFolding =
    let rec const_fold (env: KNorm.t M) e =
        let as_int x =
            env.TryFind x
            |> Option.bind (function
                | KNorm.Int v -> Some v
                | _ -> None)

        let as_float x =
            env.TryFind x
            |> Option.bind (function
                | KNorm.Float v -> Some v
                | _ -> None)

        let as_tuple x =
            env.TryFind x
            |> Option.bind (function
                | KNorm.Tuple v -> Some v
                | _ -> None)

        match e with
        | KNorm.Var x ->
            match x |> as_int, x |> as_float, x |> as_tuple with
            | Some i, _, _ -> KNorm.Int i
            | _, Some f, _ -> KNorm.Float f
            | _, _, Some xs -> KNorm.Tuple xs
            | _ -> e

        // Целочисленные операции:
        | KNorm.Neg x ->
            match x |> as_int with
            | Some x -> KNorm.Int -x
            | _ -> e
        | KNorm.Add(l, r) ->
            match l |> as_int, r |> as_int with
            | Some l, Some r -> KNorm.Int(l + r)
            | _ -> e
        | KNorm.Sub(l, r) ->
            match l |> as_int, r |> as_int with
            | Some l, Some r -> KNorm.Int(l - r)
            | _ -> e

        // Операции над числами с плавающей точкой:
        | KNorm.FNeg x ->
            match x |> as_float with
            | Some x -> KNorm.Float -x
            | _ -> e
        | KNorm.FAdd(l, r) ->
            match l |> as_float, r |> as_float with
            | Some l, Some r -> KNorm.Float(l + r)
            | _ -> e
        | KNorm.FSub(l, r) ->
            match l |> as_float, r |> as_float with
            | Some l, Some r -> KNorm.Float(l - r)
            | _ -> e
        | KNorm.FMul(l, r) ->
            match l |> as_float, r |> as_float with
            | Some l, Some r -> KNorm.Float(l * r)
            | _ -> e
        | KNorm.FDiv(l, r) ->
            match l |> as_float, r |> as_float with
            | Some l, Some r -> KNorm.Float(l / r)
            | _ -> e

        // Ветвления:
        | KNorm.BranchEq(l, r, then_e, else_e) ->
            match l |> as_int, r |> as_int, l |> as_float, r |> as_float with
            | Some l, Some r, _, _ when l = r -> then_e |> const_fold env
            | Some l, Some r, _, _ when l <> r -> else_e |> const_fold env
            | _, _, Some l, Some r when l = r -> then_e |> const_fold env
            | _, _, Some l, Some r when l <> r -> else_e |> const_fold env
            | _ -> KNorm.BranchEq(l, r, then_e |> const_fold env, else_e |> const_fold env)
        | KNorm.BranchLE(l, r, then_e, else_e) ->
            match l |> as_int, r |> as_int, l |> as_float, r |> as_float with
            | Some l, Some r, _, _ when l <= r -> then_e |> const_fold env
            | Some l, Some r, _, _ when l > r -> else_e |> const_fold env
            | _, _, Some l, Some r when l <= r -> then_e |> const_fold env
            | _, _, Some l, Some r when l > r -> else_e |> const_fold env
            | _ -> KNorm.BranchLE(l, r, then_e |> const_fold env, else_e |> const_fold env)

        | KNorm.LetTuple(ids, v, cont) ->
            match v |> as_tuple with
            | Some bound_names ->
                let mutable e' = cont |> const_fold env

                for id, v in (ids, bound_names) ||> List.zip do
                    e' <- KNorm.Let(id, KNorm.Var v, e')

                e'
            | _ -> KNorm.LetTuple(ids, v, cont |> const_fold env)

        // В объявлении функции просто пробрасываем вызов в body и cont
        | KNorm.LetRec({ name = name
                         args = args
                         body = body },
                       cont) ->
            KNorm.LetRec(
                { name = name
                  args = args
                  body = body |> const_fold env },
                cont |> const_fold env
            )

        | KNorm.Let((name, t), binding, cont) ->
            let binding' = binding |> const_fold env
            let env' = env.Add name binding'
            let cont' = cont |> const_fold env'
            KNorm.Let((name, t), binding', cont')

        | _ -> e

    let f e = const_fold (M.Empty()) e
