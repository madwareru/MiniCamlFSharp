module mini_caml_fsharp.Type

module Type =
    type t =
        /// A type of a value which has only one value which itself is ()
        | UnitType
        | BoolType
        | IntType
        | FloatType
        /// A type of a function with uncurried arguments
        | FunType of t list * t
        | TupleType of t list
        | ArrayType of t
        /// A special type used in places where we should infer the type
        | VarType of t option ref

    let gen_empty () = VarType <| ref None
