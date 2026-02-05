module mini_caml_fsharp.ClosureRepresentation

open mini_caml_fsharp.Id
open mini_caml_fsharp.Type
open mini_caml_fsharp.S

module ClosureRepresentation =
    /// Замыкание
    type closure =
        {
            /// Метка функции в toplevel
            entry: Id.l
            /// Список имён переменных, которые должны использоваться
            /// вместо free_vars из toplevel функции
            actual_free_vars: Id.t list
        }

    /// В ClosureRepresentation появляется концепция единицы трансляции.
    /// Эта единица трансляции содержит в себе набор объявлений функций верхнего уровня,
    /// а так же тело главной программы. Функции верхнего уровня помимо списка
    /// аргументов содержат так же список свободных переменных (то есть таких, которые
    /// отсутствуют в лексической области видимости внутри тела функции), и из этих
    /// функций можно создавать специальные объекты замыканий, которые содержат в себе
    /// пары из метки функции верхнего уровня и списка актуальных соответствий для свободных
    /// переменных. Можно воспринимать поле free_vars как некие "теневые" аргументы функции
    type program =
        { top_level_functions: fundef list
          main: t }

    and fundef =
        { name: Id.l * Type.t
          args: (Id.t * Type.t) list
          free_vars: (Id.t * Type.t) list
          body: t }

    and t =
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

        // Прямой вызов функции по метке из toplevel
        | ApplyDirect of Id.l * Id.t list
        // Вызов замыкания, созданного посредством операции MakeClosure
        | ApplyClosure of Id.t * Id.t list

        // Ветвления
        | BranchEq of Id.t * Id.t * t * t
        | BranchLE of Id.t * Id.t * t * t

        // Связывания имён
        | Let of (Id.t * Type.t) * t * t
        | LetTuple of (Id.t * Type.t) list * Id.t * t

        // Создаёт замыкание с новым именем от функции
        // верхнего уровня с меткой, указанной в closure,
        // и c сопоставлением  free_vars -> actual_free_vars
        // из closure. Фактически, данная операция подменяет
        // LetRec из Syntax и KNorm, с тем исключением, что
        // fundef уехал в toplevel и всегда там живёт, при
        // этом на уровне интерпретатора значения больше не
        // захвачены в env закреплённом за этой функцией,
        // и проставляются по факту вызова данной операции
        // (происходит воссоздание замыкания)
        | MakeClosure of (Id.t * Type.t) * closure * t

    let rec free_vars e =
        match e with
        | t.Unit
        | t.Int _
        | t.Float _
        | t.ExtArray _ -> S.Empty()

        | t.Neg x
        | t.FNeg x
        | t.Var x -> S.Singleton x

        | t.Add(x, y)
        | t.Sub(x, y)
        | t.FAdd(x, y)
        | t.FSub(x, y)
        | t.FMul(x, y)
        | t.FDiv(x, y)
        | t.Get(x, y) -> S.OfList [ x; y ]

        | t.Put(x, y, z) -> S.OfList [ x; y; z ]

        | t.Tuple xs

        | t.ApplyDirect(_, xs) -> S.OfList xs
        | t.ApplyClosure(x, xs) -> S.OfList <| x :: xs

        | t.BranchEq(x, y, e1, e2)
        | t.BranchLE(x, y, e1, e2) -> (free_vars e1).Union(free_vars e2).Add(x).Add y

        | t.MakeClosure((x, _), { actual_free_vars = ys }, cont) -> (S.OfList ys).Union(free_vars cont).Remove x

        | t.Let((x, _), body, cont) ->
            // получаем имена из тела и из продолжения (убирая из
            // набора продолжения само имя x, при этом если имело
            // место затенение старого имени в теле, то это имя
            // должно сохраниться. По этой причине удалние выведено
            // внутрь скобок
            (free_vars body).Union((free_vars cont).Remove x)

        | t.LetTuple(xts, v, cont) ->
            // получаем имена из продолжения, но исключаем из них
            // все вновь объявленные, и добавляем к ним имя v
            (free_vars cont).Exclude(S.OfList(xts |> List.map fst)).Add v
