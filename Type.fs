module mini_caml_fsharp.Type

module Type =
    type t =
        | UnitType
        | BoolType
        | IntType
        | FloatType
        | FunType of t list * t (* arguments are uncurried *)
        | TupleType of t list
        | ArrayType of t
        | VarType of t option ref

    let gentyp () = VarType <| ref None
