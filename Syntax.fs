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
        /// (not $e)
        | NotNode of t
        /// (- $e)
        | NegNode of t
        /// (+ $e $e)
        | AddNode of t * t
        /// (- $e $e)
        | SubNode of t * t
        /// (-. $e)
        | FNegNode of t
        /// (+. $e $e)
        | FAddNode of t * t
        /// (-. $e $e)
        | FSubNode of t * t
        /// (*. $e $e)
        | FMulNode of t * t
        /// (/. $e $e)
        | FDivNode of t * t
        /// (= $e $e)
        | EqNode of t * t
        /// (<= $e $e)
        | LENode of t * t
        /// (if $e then $e else $e)
        | IfNode of t * t * t
        /// (let $id = $e in $e)
        | LetNode of (Id.t * Type.t) * t * t
        /// $id
        | VarNode of Id.t
        /// (let-rec ($id $id...+) = $e in $e)
        | LetRecNode of fun_def * t
        /// ($e $e...+)
        | ApplyNode of t * t list
        /// (, $e...+)
        | TupleNode of t list
        /// (let (, $id...+) = $e in $e)
        | LetTuple of (Id.t * Type.t) list * t * t
        /// ([] $e $e)
        | ArrayNode of t * t
        /// ([get] $e $e)
        | GetNode of t * t
        /// ([put] $e $e $e)
        | PutNode of t * t * t 
    and fun_def =
        { name: Id.t * Type.t
          args: (Id.t * Type.t) list
          body: t }
