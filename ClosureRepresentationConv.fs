module mini_caml_fsharp.ClosureRepresentationConv

open mini_caml_fsharp.Type
open mini_caml_fsharp.KNorm
open mini_caml_fsharp.S
open mini_caml_fsharp.M
open mini_caml_fsharp.Id
open mini_caml_fsharp.ClosureRepresentation

module ClosureRepresentationConv =
    let rec private convert (env : Type.t M) (known : S) (toplevel : ClosureRepresentation.fundef list) e =
        match e with
        | KNorm.Unit -> ClosureRepresentation.Unit, toplevel
        | KNorm.Int i -> ClosureRepresentation.Int i, toplevel
        | KNorm.Float f -> ClosureRepresentation.Float f, toplevel
        | KNorm.Var x -> ClosureRepresentation.Var x, toplevel
        | KNorm.Tuple xs -> ClosureRepresentation.Tuple xs, toplevel
        
        |KNorm.Neg op -> ClosureRepresentation.Neg op, toplevel
        |KNorm.Add(l, r) -> ClosureRepresentation.Add(l, r), toplevel
        |KNorm.Sub(l, r) -> ClosureRepresentation.Sub(l, r), toplevel
        
        |KNorm.FNeg op -> ClosureRepresentation.FNeg op, toplevel
        |KNorm.FAdd(l, r) -> ClosureRepresentation.FAdd(l, r), toplevel
        |KNorm.FSub(l, r) -> ClosureRepresentation.FSub(l, r), toplevel
        |KNorm.FMul(l, r) -> ClosureRepresentation.FMul(l, r), toplevel
        |KNorm.FDiv(l, r) -> ClosureRepresentation.FDiv(l, r), toplevel
        
        |KNorm.BranchEq(x, y, e1, e2) ->
            let e1', toplevel' = e1 |> convert env known toplevel
            let e2', toplevel'' = e2 |> convert env known toplevel'
            ClosureRepresentation.BranchEq(x, y, e1', e2'), toplevel''
        |KNorm.BranchLE(x, y, e1, e2) ->
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
        
        | KNorm.ExtArray x -> ClosureRepresentation.ExtArray(Id.L x), toplevel
        // Преобразуем имя внешней функции к виду с префиксом
        | KNorm.ExtFunApply(x, ys) ->
            let x' = "min_caml_" + x |> Id.L
            ClosureRepresentation.ApplyDirect(x', ys), toplevel
            
        | KNorm.Apply(x, ys) when known.Contains x ->
            ClosureRepresentation.ApplyDirect(x |> Id.L, ys), toplevel
        | KNorm.Apply(f, ys) ->
            ClosureRepresentation.ApplyClosure(f, ys), toplevel
            
        | KNorm.LetRec({ KNorm.name = (name, t)
                         KNorm.args = argts
                         KNorm.body = body
                       }, cont) ->
            let args = argts |> List.map fst
            let env' = env.Add name t
            let known' = known.Add name
            let body', toplevel' = body |> convert env' known' toplevel
            let body_free_vars = (body' |> ClosureRepresentation.free_vars).Exclude(S.OfList args)
            let known', body', toplevel' =
                match body_free_vars.IsEmpty () with
                | true -> known', body', toplevel'
                | false ->
                    // В случае если свободные переменные в теле были найдены как непустые,
                    // откатываемся к вычислению тела с known не содержащим имени функции
                    // и с env содержащим аргументы
                    let env'' = env'.AddList argts
                    let body', toplevel' = body |> convert env'' known toplevel
                    known, body', toplevel'
            let body_free_vars =
                (body' |> ClosureRepresentation.free_vars)
                    .Exclude((S.OfList args).Add name)
                    .Elements ()
                    
            let mutable body_free_vars_with_types = []
            for free_var in body_free_vars do
                match env'.TryFind free_var with
                | Some t -> body_free_vars_with_types <- (free_var, t) :: body_free_vars_with_types
                | _ -> failwith $"failed to find {free_var} in environment!"
            
            let fun_definition : ClosureRepresentation.fundef =
                {
                    name = (Id.L(name), t)
                    args = argts
                    free_vars = body_free_vars_with_types |> List.rev
                    body = body'
                }
            let toplevel'' = fun_definition :: toplevel'
            let cont', toplevel''' = cont |> convert env' known' toplevel''
            let cont_free_vars = cont' |> ClosureRepresentation.free_vars
            match cont_free_vars.Contains name with
            | true ->
                ClosureRepresentation.MakeClosure(
                    (name, t),
                    {
                        entry = name |> Id.L
                        actual_free_vars = body_free_vars
                    },
                    cont'
                ), toplevel'''
            | _ -> cont', toplevel'''    
            
    let f e : ClosureRepresentation.program =
        let main, toplevel = e |> convert (M.Empty ()) (S.Empty ()) []
        {
            top_level_functions = toplevel
            main = main
        }