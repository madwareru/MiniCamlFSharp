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
            // 1. Cначала нужно получить свободные переменные из тела функции
            //    (это все связанные имена кроме имени функции и имён аргументов),
            //    а так же получить из окружения их типы для того, чтобы положить
            //    в closure в случае если мы имеем дело с замыканием
            let args = argts |> List.map fst
            let body_used_vars = KNorm.used_vars body
            let body_free_vars =
                body_used_vars
                    .Exclude((S.OfList args).Add name)
                    .Elements()
                    // Сортировка большей частью нужна для стабильности при тестировании
                    |> List.sort
                    
            let mutable body_free_vars_with_types = []
            for free_var in body_free_vars do
                match env.TryFind free_var with
                | Some t -> body_free_vars_with_types <- (free_var, t) :: body_free_vars_with_types
                | _ -> failwith $"failed to find {free_var} in environment!"
            
            // 2. В случае если список свободных переменных непуст, мы имеем дело с
            //    замыканием. В этом случае для рекурсивного вызова body и cont нужно
            //    передавать неизменённый набор known, в противном случае в него нужно
            //    добавить имя нашей функции (это нужно для того, чтобы впоследствии
            //    выбрать, прямой вызов функции нужно делать или вызов замыкания)
            let closure_found = not(body_free_vars.IsEmpty)
            let known' = if closure_found then known else known.Add name
            
            // 3. Рекурсивно вычисляем body, формируем новое объявление функции верхнего
            //    уровня и соединяем его с именами, полученными после рекурсивного вызова
            let env' = env.Add name t
            let env'' = env'.AddList argts
            let body', toplevel_with_body_defs = body |> convert env'' known' toplevel
            let fun_definition : ClosureRepresentation.fundef =
                {
                    name = (Id.L(name), t)
                    args = argts
                    free_vars = body_free_vars_with_types |> List.rev
                    body = body'
                }
            let toplevel' = fun_definition :: toplevel_with_body_defs
            
            // 4. Рекурсивно вычисляем cont и возвращаем итоговое значение и toplevel,
            //    при этом возможны 3 варианта развития событий:
            //    1. По какой-то причине в cont вообще не использовалось наше объявление
            //       функции. В таком случае не должно создаваться замыкание, вместо этого
            //       возвращаются модифицированные cont и toplevel
            //    2. Объявление использовалось в cont, но свободных переменных нет в body,
            //       в этом случае тоже не нужно создавать замыкание
            //    3. Во всех остальных случаях создаём замыкание
            let cont_used_vars = cont |> KNorm.used_vars
            
            match cont |> convert env' known' toplevel' with
            | cont', toplevel'' when not(cont_used_vars.Contains name) -> cont', toplevel''
            | cont', toplevel'' when body_free_vars.IsEmpty -> cont', toplevel''
            | cont', toplevel'' -> ClosureRepresentation.LetClosure((name, t), name |> Id.L, cont'), toplevel''
            
    let f e : ClosureRepresentation.program =
        let main, toplevel = e |> convert (M.Empty ()) (S.Empty ()) []
        {
            top_level_functions = toplevel
            main = main
        }