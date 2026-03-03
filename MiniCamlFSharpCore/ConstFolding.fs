module mini_caml_fsharp_core.ConstFolding

open mini_caml_fsharp_core.KNorm
open mini_caml_fsharp_core.M
open mini_caml_fsharp_core.Id
open mini_caml_fsharp_core.Type

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
            
        let as_add x =
            env.TryFind x
            |> Option.bind (function
                | KNorm.Add(l, r) -> Some(l, r)
                | _ -> None)
            
        let as_f_add x =
            env.TryFind x
            |> Option.bind (function
                | KNorm.FAdd(l, r) -> Some(l, r)
                | _ -> None)
        
        let as_f_mul x =
            env.TryFind x
            |> Option.bind (function
                | KNorm.FMul(l, r) -> Some(l, r)
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
            match l |> as_int, r |> as_int, r |> as_add with
            | Some l, Some r, _ -> KNorm.Int(l + r)
            // В случае если справа целочисленная константа,
            // переносим её влево (операция сложения коммутативна)
            | None, Some _, _ -> KNorm.Add(r, l)
            // Сворачиваем специальный случай цепочки из сумм,
            // где обе суммы начинаются с констант
            | Some l, _, Some(l', r) ->
                match l' |> as_int with
                | Some l' ->
                    let l'' = Id.gen_tmp Type.IntType
                    KNorm.Let((l'', Type.IntType), KNorm.Int (l + l'), KNorm.Add(l'', r)) 
                | _ -> e
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
            match l |> as_float, r |> as_float, r |> as_f_add with
            | Some l, Some r, _ -> KNorm.Float(l + r)
            // В случае если справа константа,
            // переносим её влево (операция сложения коммутативна)
            | None, Some _, _ -> KNorm.FAdd(r, l)
            // Сворачиваем специальный случай цепочки из суии,
            // где обе суммы начинаются с констант
            | Some l, _, Some(l', r) ->
                match l' |> as_float with
                | Some l' ->
                    let l'' = Id.gen_tmp Type.IntType
                    KNorm.Let((l'', Type.IntType), KNorm.Float (l + l'), KNorm.FAdd(l'', r)) 
                | _ -> e
            | _ -> e
        | KNorm.FSub(l, r) ->
            match l |> as_float, r |> as_float with
            | Some l, Some r -> KNorm.Float(l - r)
            | _ -> e
        | KNorm.FMul(l, r) ->
            match l |> as_float, r |> as_float, r |> as_f_mul with
            | Some l, Some r, _ -> KNorm.Float(l * r)
            // В случае если справа константа,
            // переносим её влево (операция умножения коммутативна)
            | None, Some _, _ -> KNorm.FMul(r, l)
            // Сворачиваем специальный случай цепочки из произведений,
            // где оба произведения начинаются с констант
            | Some l, _, Some(l', r) ->
                match l' |> as_float with
                | Some l' ->
                    let l'' = Id.gen_tmp Type.IntType
                    KNorm.Let((l'', Type.IntType), KNorm.Float (l * l'), KNorm.FMul(l'', r)) 
                | _ -> e
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
