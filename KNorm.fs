module mini_caml_fsharp.KNorm

open mini_caml_fsharp.Id
open mini_caml_fsharp.Type

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
