module mini_caml_fsharp.KNorm

open NUnit.Framework
open mini_caml_fsharp.Id
open mini_caml_fsharp.Type
open mini_caml_fsharp.S

module KNorm =
    type t =
        // Литералы
        | Unit
        | Int of int64
        | Float of double

        // Ссылка на переменную
        | Var of Id.t

        // Конструирование кортежа
        | Tuple of Id.t list

        // Операции над целыми числами
        | Neg of Id.t
        | Add of Id.t * Id.t
        | Sub of Id.t * Id.t

        // Операции над числами с плавающей точкой
        | FNeg of Id.t
        | FAdd of Id.t * Id.t
        | FSub of Id.t * Id.t
        | FMul of Id.t * Id.t
        | FDiv of Id.t * Id.t

        // Операции над массивами
        | Get of Id.t * Id.t
        | Put of Id.t * Id.t * Id.t
        | ExtArray of Id.t

        // Операции над функциями
        | Apply of Id.t * Id.t list
        | ExtFunApply of Id.t * Id.t list

        // Ветвления
        | BranchEq of Id.t * Id.t * t * t
        | BranchLE of Id.t * Id.t * t * t

        // Связывания имён
        | Let of (Id.t * Type.t) * t * t
        | LetTuple of (Id.t * Type.t) list * Id.t * t
        | LetRec of fundef * t

    and fundef =
        { name: Id.t * Type.t
          args: (Id.t * Type.t) list
          body: t }
    
    /// Функция, определяющая набор использованых объявлений имён
    /// в выражении. В случае если имя внешнее оно не добавляется
    /// в наабор
    let rec used_vars e =
        match e with
        | t.Unit
        | t.Int _
        | t.Float _
        | t.ExtArray _ -> S.Empty ()
        
        | t.Neg x
        | t.FNeg x
        | t.Var x -> S.Singleton x
        
        | t.Add(x, y)
        | t.Sub(x, y)
        | t.FAdd(x, y)
        | t.FSub(x, y)
        | t.FMul(x, y)
        | t.FDiv(x, y)
        | t.Get(x, y) -> S.OfList [x; y]
        
        | t.Put(x, y, z) -> S.OfList [x; y; z]
        
        | t.Tuple xs
        | t.ExtFunApply(_, xs) -> S.OfList xs
        
        | t.Apply(x, xs) -> S.OfList <| x::xs
        
        | t.BranchEq(x, y, e1, e2)
        | t.BranchLE(x, y, e1, e2) -> (used_vars e1).Union(used_vars e2).Add(x).Add(y)
        
        | t.LetRec({ name = (x, _); args = yts; body = body }, cont) ->
            // Получаем используемые в body переменные, но исключаем
            // из них имена аргументов функции и имя функции.
            (used_vars body)
                .Exclude(S.OfList(yts |> List.map fst))
                .Union(used_vars cont)
                .Remove(x)
        
        | t.Let((x, _), body, cont) ->
            // получаем имена из тела и из продолжения (убирая из
            // набора продолжения само имя x, при этом если имело
            // место затенение старого имени в теле, то это имя
            // должно сохраниться. По этой причине удалние выведено
            // внутрь скобок 
            (used_vars body).Union((used_vars cont).Remove x)
            
        | t.LetTuple(xts, v, cont) ->
            // получаем имена из продолжения, но исключаем из них
            // все вновь объявленные, и добавляем к ним имя v
            (used_vars cont).Exclude(S.OfList(xts |> List.map fst)).Add(v)
                