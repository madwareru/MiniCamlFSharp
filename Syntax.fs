module mini_caml_fsharp.Syntax

open mini_caml_fsharp.Id
open mini_caml_fsharp.Type

module Syntax =
    type t =
        /// ()
        | UnitNode
        /// #t | #f
        | BoolNode of bool
        /// $int-literal
        | IntNode of int64
        /// $double-literal
        | FloatNode of double
        /// (not $expr), где $expr: boolean
        | NotNode of t
        /// (- $expr), где $expr: int
        | NegNode of t
        /// (+ $expr0 $expr1), где $expr0, $expr1: int
        | AddNode of t * t
        /// (- $expr0 $expr1), где $expr0, $expr1: int
        | SubNode of t * t
        /// (-. $expr), где $expr: float
        | FNegNode of t
        /// (+. $expr0 $expr1), где $expr0, $expr1: float
        | FAddNode of t * t
        /// (-. $expr0 $expr1), где $expr0, $expr1: float
        | FSubNode of t * t
        /// (*. $expr0 $expr1), где $expr0, $expr1: float
        | FMulNode of t * t
        /// (/. $expr0 $expr1), где $expr0, $expr1: float
        | FDivNode of t * t
        /// (= $expr0 $expr1), где $expr0, $expr1: int | float
        | EqNode of t * t
        /// (<= $expr0 $expr1), где $expr0, $expr1: int | float
        | LENode of t * t
        /// (if $expr0 then $expr1 else $expr2), где $expr: boolean, а $expr1 и $expr2 одного типа
        | IfNode of t * t * t
        /// (let $id = $expr0 in $expr1)
        | LetNode of (Id.t * Type.t) * t * t
        /// $id
        | VarNode of Id.t
        /// (let-rec ($id0 $id1...+) = $expr0 in $expr1)
        | LetRecNode of fun_def * t
        /// ($expr0 $expr1...+), где $expr0 имеет тип функции, а количество $expr1 совпадает с её арностью
        | ApplyNode of t * t list
        /// (, $expr...+)
        | TupleNode of t list
        /// (let (, $id...+) = $expr0 in $expr1)
        | LetTuple of (Id.t * Type.t) list * t * t
        /// (new[] $expr0 $expr1)
        | ArrayNode of t * t
        /// ([get] $expr0 $expr1)
        | GetNode of t * t
        /// ([put] $expr0 $expr1 $expr2)
        | PutNode of t * t * t

    and fun_def =
        { name: Id.t * Type.t
          args: (Id.t * Type.t) list
          body: t }
