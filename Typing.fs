module mini_caml_fsharp.Typing

open mini_caml_fsharp.Id
open mini_caml_fsharp.Type
open mini_caml_fsharp.Syntax
open mini_caml_fsharp.M

open Syntax

module Typing =
    exception UnifyException of Type.t * Type.t
    exception TypingException of t * Type.t * Type.t
    
    let extenv = M.Empty
    
    /// <summary>
    /// Функция, которая заменяет переменные типов их содержимым. Замена происходит рекурсивно.
    /// Комментарий из оригинала так же содержит текст: for pretty printing (and type normalization)
    /// </summary>
    let rec deref_typ =
        function
        | Type.FunType(t1s, t2) -> Type.FunType (List.map deref_typ t1s, deref_typ t2)
        | Type.TupleType ts -> Type.TupleType <| List.map deref_typ ts
        | Type.ArrayType t -> Type.ArrayType <| deref_typ t
        | Type.VarType({ contents = None } as r) ->
            eprintf "uninstantiated type variable detected; assuming int@.";
            r.Value <- Some(Type.IntType);
            Type.IntType
        | Type.VarType({ contents = Some(t) } as r) ->
            let t' = deref_typ t in
            r.Value <- Some(t');
            t'
        | t -> t
        
    /// <summary>
    /// То же, что и deref_typ, но для пары из идентификатора и типа
    /// </summary>
    let deref_id_typ (x : Id.t, t) = (x, deref_typ t)
    
    /// <summary>
    /// Разворачивает все типовые переменные в программе на
    /// языке MinCaml на их внутренное представление посредством
    /// вызова deref_id_typ
    /// </summary>
    let rec deref_term =
        function
        | NotNode(e) -> NotNode(deref_term e)
        | NegNode(e) -> NegNode(deref_term e)
        | AddNode(e1, e2) -> AddNode(deref_term e1, deref_term e2)
        | SubNode(e1, e2) -> SubNode(deref_term e1, deref_term e2)
        | EqNode(e1, e2) -> EqNode(deref_term e1, deref_term e2)
        | LENode(e1, e2) -> LENode(deref_term e1, deref_term e2)
        | FNegNode(e) -> FNegNode(deref_term e)
        | FAddNode(e1, e2) -> FAddNode(deref_term e1, deref_term e2)
        | FSubNode(e1, e2) -> FSubNode(deref_term e1, deref_term e2)
        | FMulNode(e1, e2) -> FMulNode(deref_term e1, deref_term e2)
        | FDivNode(e1, e2) -> FDivNode(deref_term e1, deref_term e2)
        | IfNode(e1, e2, e3) -> IfNode(deref_term e1, deref_term e2, deref_term e3)
        | LetNode(xt, e1, e2) -> LetNode(deref_id_typ xt, deref_term e1, deref_term e2)
        | LetRecNode({ name = xt; args = yts; body = e1 }, e2) ->
            LetRecNode(
                {
                    name = deref_id_typ xt;
                    args = List.map deref_id_typ yts;
                    body = deref_term e1
                },
                deref_term e2
            )
        | ApplyNode(e, es) -> ApplyNode(deref_term e, List.map deref_term es)
        | TupleNode(es) -> TupleNode(List.map deref_term es)
        | LetTuple(xts, e1, e2) -> LetTuple(List.map deref_id_typ xts, deref_term e1, deref_term e2)
        | ArrayNode(e1, e2) -> ArrayNode(deref_term e1, deref_term e2)
        | GetNode(e1, e2) -> GetNode(deref_term e1, deref_term e2)
        | PutNode(e1, e2, e3) -> PutNode(deref_term e1, deref_term e2, deref_term e3)
        | e -> e
    
    /// <summary>
    /// Проверяет тип <paramref name="r2"/> на вхождение в него типовой переменной с тем же типом,
    /// что и у <paramref name="r1"/>
    /// <param name="r1">Тип, который ищется в <paramref name="r2"/></param>
    /// <param name="r2">Тип, в котором ищется тип <paramref name="r1"/>. Поиск идёт рекурсивно</param>
    /// <returns>true в случае, если вхождение найдено</returns>
    /// </summary>
    let rec occur (r1 : Type.t) (r2 : Type.t) = (* occur check (caml2html: typing_occur) *)
        match r2 with
        | Type.FunType(t2s, t2) -> List.exists (occur r1) t2s || occur r1 t2
        | Type.TupleType(t2s) -> List.exists (occur r1) t2s
        | Type.ArrayType(t2) -> occur r1 t2
        | Type.VarType(r2) when r1.Equals(r2) -> true
        | Type.VarType({ contents = None }) -> false
        | Type.VarType({ contents = Some(t2) }) -> occur r1 t2
        | _ -> false
    
    let f (s : t) : t = s //todo