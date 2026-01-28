module mini_caml_fsharp.Id

open mini_caml_fsharp.Type

module Id =
    /// Name of a variable
    type t = string
    /// Labels for top-level declarations
    type l = L of string

    let rec pp_list =
        function
        | [] -> ""
        | [ x ] -> x
        | x :: xs -> x + " " + pp_list xs

    let mutable counter = 0

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
        | Type.VarType _ -> failwith "can't get id of type var"

    let gen_tmp typ =
        counter <- counter + 1
        Printf.sprintf $"T%s{id_of_typ typ}%d{counter}"
