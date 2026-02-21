module mini_caml_fsharp_core.ClosureRepresentationConv

open mini_caml_fsharp_core.Type
open mini_caml_fsharp_core.KNorm
open mini_caml_fsharp_core.S
open mini_caml_fsharp_core.M
open mini_caml_fsharp_core.Id
open mini_caml_fsharp_core.ClosureRepresentation

module ClosureRepresentationConv =
    let rec private convert (env: Type.t M) (known: S) (toplevel: ClosureRepresentation.fundef list) e =
        match e with
        | KNorm.Unit -> ClosureRepresentation.Unit, toplevel
        | KNorm.Int i -> ClosureRepresentation.Int i, toplevel
        | KNorm.Float f -> ClosureRepresentation.Float f, toplevel
        | KNorm.Var x -> ClosureRepresentation.Var x, toplevel
        | KNorm.Tuple xs -> ClosureRepresentation.Tuple xs, toplevel

        | KNorm.Neg op -> ClosureRepresentation.Neg op, toplevel
        | KNorm.Add(l, r) -> ClosureRepresentation.Add(l, r), toplevel
        | KNorm.Sub(l, r) -> ClosureRepresentation.Sub(l, r), toplevel

        | KNorm.FNeg op -> ClosureRepresentation.FNeg op, toplevel
        | KNorm.FAdd(l, r) -> ClosureRepresentation.FAdd(l, r), toplevel
        | KNorm.FSub(l, r) -> ClosureRepresentation.FSub(l, r), toplevel
        | KNorm.FMul(l, r) -> ClosureRepresentation.FMul(l, r), toplevel
        | KNorm.FDiv(l, r) -> ClosureRepresentation.FDiv(l, r), toplevel

        | KNorm.BranchEq(x, y, e1, e2) ->
            let e1', toplevel' = e1 |> convert env known toplevel
            let e2', toplevel'' = e2 |> convert env known toplevel'
            ClosureRepresentation.BranchEq(x, y, e1', e2'), toplevel''
        | KNorm.BranchLE(x, y, e1, e2) ->
            let e1', toplevel' = e1 |> convert env known toplevel
            let e2', toplevel'' = e2 |> convert env known toplevel'
            ClosureRepresentation.BranchLE(x, y, e1', e2'), toplevel''

        | KNorm.Let((x, t), bind, cont) ->
            let env' = env.Add x t
            let bind', toplevel' = bind |> convert env' known toplevel
            let cont', toplevel'' = cont |> convert env' known toplevel'
            ClosureRepresentation.Let((x, t), bind', cont'), toplevel''
        | KNorm.LetTuple(xts, y, cont) ->
            let env' = env.AddList xts
            let cont', toplevel' = cont |> convert env' known toplevel
            ClosureRepresentation.LetTuple(xts, y, cont'), toplevel'

        | KNorm.Get(x, ix) -> ClosureRepresentation.Get(x, ix), toplevel
        | KNorm.Put(x, ix, v) -> ClosureRepresentation.Put(x, ix, v), toplevel

        // Преобразуем имя внешней функции к виду с префиксом
        | KNorm.ExtFunApply(x, ys) ->
            let x' = "min_caml_" + x |> Id.L
            ClosureRepresentation.ApplyDirect(x', ys), toplevel

        | KNorm.Apply(x, ys) when known.Contains x -> ClosureRepresentation.ApplyDirect(x |> Id.L, ys), toplevel
        | KNorm.Apply(f, ys) -> ClosureRepresentation.ApplyClosure(f, ys), toplevel

        | KNorm.LetRec({ KNorm.name = (name, t)
                         KNorm.args = argts
                         KNorm.body = body },
                       cont) ->
            let rec var_used_not_as_a_callee e =
                // Предполагается, что все идентификаторы уже прошли этап AlphaConv
                match e with
                | KNorm.Unit
                | KNorm.Int _
                | KNorm.Float _ -> false

                | KNorm.Neg x
                | KNorm.FNeg x
                | KNorm.Var x -> x = name

                | KNorm.Add(x, y)
                | KNorm.Sub(x, y)
                | KNorm.FAdd(x, y)
                | KNorm.FSub(x, y)
                | KNorm.FMul(x, y)
                | KNorm.FDiv(x, y)
                | KNorm.Get(x, y) -> (x = name) || (y = name)

                | KNorm.Put(x, y, z) -> (x = name) || (y = name) || (z = name)

                | KNorm.Tuple xs
                | KNorm.ExtFunApply(_, xs)
                | KNorm.Apply(_, xs) -> xs |> List.exists (fun x -> x = name)

                | KNorm.BranchEq(x, y, e1, e2)
                | KNorm.BranchLE(x, y, e1, e2) ->
                    (x = name)
                    || (y = name)
                    || (var_used_not_as_a_callee e1)
                    || (var_used_not_as_a_callee e2)

                | KNorm.LetRec({ body = body }, cont)
                | KNorm.Let(_, body, cont) -> (var_used_not_as_a_callee body) || (var_used_not_as_a_callee cont)
                | KNorm.LetTuple(_, v, cont) -> v = name || (var_used_not_as_a_callee cont)

            let args = argts |> List.map fst
            let body_used_vars = KNorm.used_vars body

            let body_free_vars =
                body_used_vars.Exclude((S.OfList args).Add name).Elements()
                // Сортировка большей частью нужна для стабильности при тестировании
                |> List.sort

            let mutable body_free_vars_with_types = []

            for free_var in body_free_vars do
                match env.TryFind free_var with
                | Some t -> body_free_vars_with_types <- (free_var, t) :: body_free_vars_with_types
                | _ -> failwith $"failed to find {free_var} in environment!"

            let is_closure = (not body_free_vars.IsEmpty) || (cont |> var_used_not_as_a_callee)
            let known' = if is_closure then known else known.Add name

            let env' = env.Add name t
            let env'' = env'.AddList argts
            let body', toplevel_with_body_defs = body |> convert env'' known' toplevel

            let fun_definition: ClosureRepresentation.fundef =
                { name = (Id.L(name), t)
                  args = argts
                  free_vars = body_free_vars_with_types |> List.rev
                  is_closure = is_closure
                  body = body' }

            let toplevel' = fun_definition :: toplevel_with_body_defs

            match cont |> convert env' known' toplevel' with
            | cont', toplevel'' when not is_closure -> cont', toplevel''
            | cont', toplevel'' -> ClosureRepresentation.LetClosure((name, t), name |> Id.L, cont'), toplevel''

    let f e : ClosureRepresentation.program =
        let main, toplevel = e |> convert (M.Empty()) (S.Empty()) []

        { top_level_functions = toplevel
          main = main }
