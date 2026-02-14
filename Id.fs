module mini_caml_fsharp.Id

open mini_caml_fsharp.Type

module Id =
    /// Тип идентификатора для имён переменных
    type t = string
    /// Тип идентификатора для меток объявлений верхнего уровня
    type l = L of string

    let rec pp_list =
        function
        | [] -> ""
        | [ x ] -> x
        | x :: xs -> x + " " + pp_list xs

    /// Глобальный счётчик для генерации уникальных идентификаторов
    let mutable private counter = 0

    // Сброс счётчика. Большей частью предназначен для использования в юнит тестах
    let internal reset () = counter <- 0

    /// Используется для достижения уникальности существующих идентификаторов
    let gen_id s =
        counter <- counter + 1
        Printf.sprintf $"%s{s}.%d{counter}"

    let id_of_typ =
        function
        | Type.UnitType -> "u"
        | Type.BoolType -> "b"
        | Type.IntType -> "i"
        | Type.FloatType -> "d"
        | Type.FunType _ -> "f"
        | Type.TupleType _ -> "t"
        | Type.ArrayType _ -> "a"
        | Type.FunctionLabel -> "fl"
        | Type.VarType _ -> failwith "can't get id of type var"

    /// Используется для генерации имён временных переменных,
    /// возникающих в случае если синтаксическая форма раскрывается
    /// в вид, вводящий промежуточную переменную. Так же эта функция
    /// может быть использована в проходах компилятора, преобразующих
    /// код до более простого вида.
    let gen_tmp typ =
        counter <- counter + 1
        Printf.sprintf $"T%s{id_of_typ typ}%d{counter}"
