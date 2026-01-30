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
    let rec occur r1 r2 = (* occur check (caml2html: typing_occur) *)
        match r2 with
        | Type.FunType(arg_ts, ret_t) -> arg_ts |> List.exists (occur r1) || occur r1 ret_t
        | Type.TupleType(ts) -> List.exists (occur r1) ts
        | Type.ArrayType(elem_t) -> occur r1 elem_t
        | Type.VarType(r2) when r1.Equals(r2) -> true
        | Type.VarType({ contents = None }) -> false
        | Type.VarType({ contents = Some(t2) }) -> occur r1 t2
        | _ -> false
        
    /// <summary>
    /// Алгоритм унификации.
    /// Суть его в попытке найти такую подстановку, чтобы два типа были идентичными
    /// </summary>
    let rec unify t1 t2 =
        match t1, t2 with
        // Типы примитивные и совпадают -> ничего не делаем,
        // так как подстановка уже найдена
        | Type.UnitType, Type.UnitType
        | Type.BoolType, Type.BoolType
        | Type.IntType, Type.IntType
        | Type.FloatType, Type.FloatType -> ()
        
        // Типы -- типовые переменные и совпадают по
        // значению -> ничего не делаем, подстановка найдена
        | Type.VarType r1, Type.VarType r2 when r1.Equals(r2) -> ()
         
        // Встречены два типа массивов -> для поиска подстановки
        // рекурсивно унифицируем типы их элементов 
        | Type.ArrayType(t1), Type.ArrayType(t2) -> unify t1 t2
        
        // При встрече с двумя функциональными типами перво-наперво
        // сравниваем их арности. Арность не совпадает -> подстановка
        // невозможна. В противном случае рекурсивно поэлементно унифицируем
        // типы аргументов, после чего унифицируем типы возврата
        | Type.FunType(arg_ts_1, ret_t_1), Type.FunType(arg_ts_2, ret_t_2) ->
            let l_1, l_2 = arg_ts_1 |> List.length, arg_ts_2 |> List.length
            if l_1 <> l_2 then raise (UnifyException(t1, t2))
            List.iter2 unify arg_ts_1 arg_ts_2
            unify ret_t_1 ret_t_2
        
        // При встрече двух кортежей сверяем арности. Если арности
        // не совпали -> подстановку найти невозможно. Иначе
        // рекурсивно поэлементно унифицируем типы элементов
        | Type.TupleType ts_1, Type.TupleType ts_2 ->
            let l_1, l_2 = ts_1 |> List.length, ts_2 |> List.length
            if l_1 <> l_2 then raise (UnifyException(t1, t2))
            List.iter2 unify ts_1 ts_2
            
        // Если слева или справа встречена непустая типовая переменная,
        // другой операнд унифицируется с ней
        | Type.VarType { contents = Some(t1') }, _ -> unify t1' t2
        | _, Type.VarType({ contents = Some(t2') }) -> unify t1 t2'
            
        // Иначе, если левый или правый операнд является пустой типовой
        // переменной, проверяем другой операнд на наличие цикла, если цикл есть,
        // значит подстановку найти нелья, иначе считаем, что корректной
        // подстановкой является такая, где в типовую переменную записывается
        // тип другого операнда
        | Type.VarType({ contents = None } as r1), _ ->
            if occur r1 t2 then raise (UnifyException(t1, t2))
            r1.Value <- Some(t2)
        | _, Type.VarType({ contents = None } as r2) ->
            if occur r2 t1 then raise (UnifyException(t1, t2))
            r2.Value <- Some(t1)
        
        // Во всех остальных случаях имеем дело с несовместимыми типами,
        // для которых невозможно найти подстановку
        | _, _ -> raise (UnifyException(t1, t2))
    
    let f (s : t) : t = s //todo