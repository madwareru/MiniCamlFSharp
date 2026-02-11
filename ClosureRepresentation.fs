module mini_caml_fsharp.ClosureRepresentation

open mini_caml_fsharp.Id
open mini_caml_fsharp.Type

module ClosureRepresentation =
    /// В ClosureRepresentation появляется концепция единицы трансляции.
    /// Эта единица трансляции содержит в себе набор объявлений функций верхнего уровня,
    /// а так же тело главной программы. Функции верхнего уровня помимо списка
    /// аргументов содержат так же список свободных переменных (то есть таких, которые
    /// отсутствуют в лексической области видимости внутри тела функции), и из этих
    /// функций можно создавать специальные объекты замыканий, которые содержат в себе
    /// пары из метки функции верхнего уровня и списка актуальных соответствий для свободных
    /// переменных. Можно воспринимать поле free_vars как некие "теневые" аргументы функции.
    /// Если список свободных переменных пуст, такую функцию можно вызывать "прямым" способом
    /// по её метке, в противном случае нужно делать вызов у замыкания
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
        // верхнего уровня с меткой, указанной в closure.
        // Фактически, данная операция подменяет
        // LetRec из Syntax и KNorm, с тем исключением, что
        // fundef уехал в toplevel и всегда там живёт
        | LetClosure of (Id.t * Type.t) * Id.l * t
