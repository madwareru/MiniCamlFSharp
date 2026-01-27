module mini_caml_fsharp.Syntax

open mini_caml_fsharp.Id
open mini_caml_fsharp.Type

module Syntax =
    type t =
        | UnitNode
        | BoolNode of bool
        | IntNode of int64
        | FloatNode of double
        | NotNode of t
        | NegNode of t
        | AddNode of t * t
        | SubNode of t * t
        | FNegNode of t
        | FAddNode of t * t
        | FSubNode of t * t
        | FMulNode of t * t
        | FDivNode of t * t
        | EqNode of t * t
        | LENode of t * t
        | IfNode of t * t * t
        | LetNode of (Id.t * Type.t) * t * t
        | VarNode of Id.t
        | LetRecNode of fundef * t
        | ApplyNode of t * t list
        | TupleNode of t list
        | LetTuple of (Id.t * Type.t) list * t * t
        | ArrayNode of t * t
        | GetNode of t * t
        | PutNode of t * t

    and fundef =
        { name: Id.t * Type.t
          args: (Id.t * Type.t) list
          body: t }
