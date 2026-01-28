module mini_caml_fsharp.Syntax

open mini_caml_fsharp.Id
open mini_caml_fsharp.Type

module Syntax =
    type t =
        | UnitNode // ()
        | BoolNode of bool // #t | #f
        | IntNode of int64 // $int-literal
        | FloatNode of double // $double-literal
        | NotNode of t // (not $e)
        | NegNode of t // (- $e)
        | AddNode of t * t // (+ $e $e)
        | SubNode of t * t // (- $e $e)
        | FNegNode of t // (-. $e)
        | FAddNode of t * t // (+. $e $e)
        | FSubNode of t * t // (-. $e $e)
        | FMulNode of t * t // (*. $e $e)
        | FDivNode of t * t // (/. $e $e)
        | EqNode of t * t // (= $e $e)
        | LENode of t * t // (<= $e $e)
        | IfNode of t * t * t // (if $e then $e else $e)
        | LetNode of (Id.t * Type.t) * t * t // (let $id = $e in $e)
        | VarNode of Id.t // $id
        | LetRecNode of fun_def * t // (let-rec ($id $id...) = $e in $e)
        | ApplyNode of t * t list // ($e $e...)
        | TupleNode of t list // (, $e...)
        | LetTuple of (Id.t * Type.t) list * t * t // (let (, $id...) = $e in $e)
        | ArrayNode of t * t // ([] $e $e)
        | GetNode of t * t // ([get] $e $e)
        | PutNode of t * t * t // ([put] $e $e $e)
    and fun_def =
        { name: Id.t * Type.t
          args: (Id.t * Type.t) list
          body: t }
