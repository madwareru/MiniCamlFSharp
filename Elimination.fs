module mini_caml_fsharp.Elimination

open mini_caml_fsharp.KNorm
open mini_caml_fsharp.S
open mini_caml_fsharp.Id

module Elimination =
    /// Функция, определяющая, является ли выражение свободным
    /// от побочных эффектов. Это нужно знать для дальнейшего
    /// решения, нужно ли уничтожать неиспользуемое объявление,
    /// так как в случае если вычисления чистые, то удалять можно
    /// с абсолютной уверенностью, что это никак не повлияет на
    /// работоспособность программы. Так как невозможно сказать,
    /// являются ли применения функций и запись в массив не грязными,
    /// они всегда считаются грязными. Все примитивные операции
    /// считаются чистыми. В случае связываний имён и бранчинга
    /// просто проверяются подвыражения
    let rec private is_pure e =
        match e with
        | KNorm.Apply _
        | KNorm.ExtFunApply _
        | KNorm.Put _ -> false
        
        | KNorm.Let(_, a, b)
        | KNorm.BranchEq(_, _, a, b)
        | KNorm.BranchLE(_, _, a, b) -> is_pure a && is_pure b
        
        | KNorm.LetRec(_, e)
        | KNorm.LetTuple(_, _, e) -> is_pure e
        
        | _ -> true
        
    let rec f =
        function
        | KNorm.BranchEq(x, y, then_e, else_e) -> KNorm.BranchEq(x, y, f then_e, f else_e)
        | KNorm.BranchLE(x, y, then_e, else_e) -> KNorm.BranchLE(x, y, f then_e, f else_e)
        | KNorm.Let((name, t), binding, cont) ->
            let binding' = binding |> f
            let cont' = cont |> f
            let live_set = cont' |> KNorm.used_vars
            if (is_pure binding') && not(live_set.Contains(name)) then
                printfn $"eliminating variable %s{name}"
                cont'
            else
                KNorm.Let((name, t), binding', cont')
        | KNorm.LetTuple(xts, v, cont) ->
            let names = xts |> List.map fst
            let name_set = S.OfList names
            let cont' = cont |> f
            let live_set = cont' |> KNorm.used_vars
            if live_set.Intersect(name_set).IsEmpty () then
                printfn $"eliminating variables %s{Id.pp_list names}"
                cont'
            else
                KNorm.LetTuple(xts, v, cont')
        | KNorm.LetRec({ name = (name, t); args = args; body = body }, cont) ->
            let body' = body |> f
            let cont' = cont |> f
            let live_set = cont' |> KNorm.used_vars
            if (is_pure body') && not(live_set.Contains(name)) then
                printfn $"eliminating variable %s{name}"
                cont'
            else
                KNorm.LetRec({ name = (name, t); args = args; body = body' }, cont')
        | e -> e