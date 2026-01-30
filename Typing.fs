module mini_caml_fsharp.Typing

open Microsoft.FSharp.Collections
open mini_caml_fsharp.Id
open mini_caml_fsharp.Type
open mini_caml_fsharp.Syntax
open mini_caml_fsharp.M

open Syntax

module Typing =
    exception UnifyException of Type.t * Type.t
    exception TypingException of t * Type.t * Type.t
    
    // "Внешние" переменные
    let extenv : (Type.t M) ref = ref (M.Empty ())
    
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
    
    /// <summary>
    /// Проход по синтаксическому дереву, осуществляющий основную работу по выводу типов.
    /// Результатом работы функции так же является тип, так как вся программа на языке MinCaml
    /// является выражением. Подвыражения так же являются корректными программами на MinCaml,
    /// поэтому алгоритм может этим воспользоваться и производит по сути поиск в глубину. Сначала
    /// мы доберёмся до самых примитивных конструкций, тип которых вывести можно элементарно, далее
    /// алгоритм поднимается уровнем выше, и ему могут встретиться места где нужно сверить типы
    /// операндов или вывести типы для типовых переменных. И то и другое делается посредством
    /// унификации.
    /// <param name="env">Окружение, которое содержит информацию о текущих связанных именах. Область
    /// видимости лексическая, поэтому важно учитывать затенения переменных</param>
    /// <param name="e">текущее выражение на языке MinCaml</param>
    /// </simmary>
    let rec infer (env : Type.t M) e =
        try
            match e with
            // Типы примитивных значений выводятся элементарно
            | UnitNode -> Type.UnitType
            | BoolNode _ -> Type.BoolType
            | IntNode _ -> Type.IntType
            | FloatNode _ -> Type.FloatType
            
            // В случае унарных операций выводим сначала тип аргумента,
            // унифицируется с типом, который ожидается  в операции,
            // и возвращаем тип, который должна возвращать операция
            | NotNode op ->
                let op_t = op |> infer env
                unify Type.BoolType op_t
                Type.BoolType
            | NegNode op ->
                let op_t = op |> infer env
                unify Type.IntType op_t
                Type.IntType
            | FNegNode op ->
                let op_t = op |> infer env
                unify Type.FloatType op_t
                Type.FloatType
            
            // c бинарными (кроме операций сравнения) операция схожая     
            | AddNode(l, r) 
            | SubNode(l, r) ->
                let l_t = l |> infer env
                let r_t = r |> infer env
                unify Type.IntType l_t
                unify Type.IntType r_t
                Type.IntType
            | FAddNode(l, r)
            | FSubNode(l, r)
            | FMulNode(l, r)
            | FDivNode(l, r) ->
                let l_t = l |> infer env
                let r_t = r |> infer env
                unify Type.FloatType l_t
                unify Type.FloatType r_t
                Type.FloatType
                
            // Операции сравнения унифицируют типы своих аргументов,
            // так как могут сравнивать между собой разные типы
            | EqNode(l, r)
            | LENode(l, r) ->
                let l_t = l |> infer env
                let r_t = r |> infer env
                unify l_t r_t
                Type.BoolType
                
            // В случае кортежа просто вызываем рекурсивно вывод типов    
            | TupleNode elems -> Type.TupleType(elems |> List.map (infer env))
            
            // Для массива рекурсивно выводим типы значения и количества,
            // унифицируем количество с целым типом и возвращаем новый тип массива
            | ArrayNode(v, count) ->
                let v_t = v |> infer env
                let count_t = count |> infer env
                unify Type.IntType count_t
                Type.ArrayType(v_t)
            
            // В условном выражении убеждаемся, что условие имеет
            // логический тип, и что типы обеих веток совпадают
            | IfNode(cond, then_e, else_e) ->
                let cond_t = cond |> infer env
                unify Type.BoolType cond_t
                let then_t = then_e |> infer env
                let else_t = else_e |> infer env
                unify then_t else_t
                then_t
            
            // В случае выражения Let, сначала выводится тип связанного значения,
            // затем он унифицируется с типом связанного имени, после чего создаётся
            // копия окружения, расширенная информацией о том, что теперь в области
            // видимости есть новое имя name с типом t, далее рекурсивно вызывается
            // вывод типа у выражения cond, с учётом расширенного окружения
            | LetNode((name, t), binding, cont) ->
                let binding_t = binding |> infer env
                unify t binding_t
                let env' = env.Add name t
                cont |> infer env'
            
            // Работает практически идентично с обычным Let выражением    
            | LetTuple(names, binding, cont) ->
                let binding_t = binding |> infer env
                let t = Type.TupleType(names |> List.map snd)
                unify t binding_t
                let env' = env.AddList names
                cont |> infer env'
                
            // Для корректной типизации рекурсивной функции сначала нужно расширить окружение
            // именем функции с её типом, затем для вывода типа тела (но только для него)
            // создаётся ещё одно окружение, с этим окружением выводится тип тела. Далее формируется
            // тип для функции и он унифицируется с t, после чего выводится тип для cont, с окружением,
            // в котором введено новое имя рекурсивной функции       
            | LetRecNode({ name = (name, t); args = args; body = body }, cont) ->
                let env' = env.Add name t
                let env'' = env'.AddList args
                let body_t = body |> infer env''
                let fun_t = Type.FunType(args |> List.map snd, body_t)
                unify t fun_t
                cont |> infer env'
            
            // В случае применения функции выводим тип вызываемого значения,
            // а так же его аргументов, далее создаём новую типовую переменную,
            // из списка выведенных типов аргументов и этой типовой переменной
            // конструируем тип функции, после чего унифицируем его с типом
            // вызываемого выражения. В результате, если унификация прошла
            // успешно, в t будет находиться выведенный тип
            | ApplyNode(callee, args) ->
                let t = Type.gen_empty ()
                let callee_t = callee |> infer env
                let arg_ts = args |> List.map (infer env)
                let foo_t = Type.FunType(arg_ts, t)
                unify callee_t foo_t
                t
            
            // Строим тип массива с типовой переменной t
            // и унифицируем его с выведенным типом для arr,
            // тем самым выводим тип результата. Выводим тип
            // индекса и унифицируем его с целым типом. Если
            // всё прошло хорошо, возвращаем тип элемента
            | GetNode(arr, idx) ->
                let t = Type.gen_empty ()
                let arr_t = Type.ArrayType(t)
                unify arr_t (arr |> infer env)
                unify Type.IntType (idx |> infer env)
                t
            
            // Большей частью логика повторяет Put, но добавляется
            // унификация t с выведенным типом значения, которое нужно
            // положить в массив. Данное выражение возвращает (), поэтому
            // возвращаем соответствующий тип    
            | PutNode(arr, idx, v) ->
                let t = Type.gen_empty ()
                let arr_t = Type.ArrayType(t)
                unify arr_t (arr |> infer env)
                unify Type.IntType (idx |> infer env)
                unify t (v |> infer env)
                Type.UnitType
                
            // Если в окружении не найдено имя, оно считается внешним
            // и в таком случае ищется в extenv, если в extenv типа для имени
            // нет, там заводится пустая типовая переменная и пишется ошибка
            // в лог, но вывод типов не падает
            | VarNode name ->
                match env.TryFind name with
                | Some t -> t
                | _ ->
                    match extenv.Value.TryFind name with
                    | Some t -> t
                    | _ ->
                        eprintf $"free variable %s{name} assumed as external@."
                        let t = Type.gen_empty()
                        extenv.Value <- extenv.Value.Add name t
                        t
        with UnifyException(t1, t2) -> raise (TypingException(deref_term e, deref_typ t1, deref_typ t2))
    
    let f e =
        extenv.Value <- M.Empty ()
        let inferred_t = e |> infer (M.Empty ())
        printf $"inferred type of expression result is {inferred_t}"
        extenv.Value <- extenv.Value.Map (fun _ -> deref_typ) 
        deref_term e