module mini_caml_fsharp_core.Type

module Type =
    type t =
        /// Тип с единственным значением ()
        | UnitType
        | BoolType
        | IntType
        | FloatType
        /// Тип функции, аргументы которой не каррированы
        | FunType of t list * t
        | TupleType of t list
        | ArrayType of t
        /// Специальный тип, используемый в местах, где мы должны выводить тип
        | VarType of t option ref
        /// Используется в замыканиях:
        | FunctionLabel

    /// Вспомогательная функция для генерации заглушек в местах где нужен вывод типа
    let gen_empty () = VarType <| ref None
