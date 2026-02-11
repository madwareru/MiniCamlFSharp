module mini_caml_fsharp.KNormalisation

open mini_caml_fsharp.Id
open mini_caml_fsharp.Type
open mini_caml_fsharp.Syntax
open mini_caml_fsharp.M
open mini_caml_fsharp.Typing
open mini_caml_fsharp.KNorm

module KNormalisation =
    /// <summary>
    /// Вспомогательная функция, которая проверяет,
    /// является ли аргумент переменной в KNorm.t и
    /// если да, то просто пробрасывает имя этой
    /// переменной для дальнейшего использования в
    /// <paramref name="cont"/>, иначе создаёт
    /// новый временный идентификатор для нужного
    /// типа и заворачивает результат mapping в
    /// KNorm.Let
    /// </summary>
    /// <returns>
    /// Пара из нормализованного выражения и его типа
    /// </returns>
    let private insert_let cont =
        function
        | KNorm.Var x, _ -> cont x
        | expr, t ->
            let name = Id.gen_tmp t
            let cont, cont_t = cont name
            KNorm.Let((name, t), expr, cont), cont_t

    let rec private replace_bool t =
        match t with
        | Type.VarType _ -> failwith "unexpected type variable found"
        | Type.UnitType
        | Type.IntType
        | Type.FloatType -> t
        | Type.BoolType -> Type.IntType
        | Type.ArrayType tt -> replace_bool tt |> Type.ArrayType
        | Type.TupleType ts -> ts |> List.map replace_bool |> Type.TupleType
        | Type.FunType(arg_ts, t) ->
            let arg_ts = arg_ts |> List.map replace_bool
            let t = replace_bool t
            Type.FunType(arg_ts, t)

    let rec private normalize (env: Type.t M) e =
        let unary ctr t e =
            (normalize env e) |> insert_let (fun e' -> ctr e', t)

        let binary ctr t l r =
            (normalize env l)
            |> insert_let (fun l' -> (normalize env r) |> insert_let (fun y' -> ctr (l', y'), t))

        match e with
        | Syntax.UnitNode -> KNorm.Unit, Type.UnitType
        | Syntax.IntNode i -> KNorm.Int i, Type.IntType
        | Syntax.FloatNode f -> KNorm.Float f, Type.FloatType

        // Операции над целыми числами
        | Syntax.NegNode e -> e |> unary KNorm.Neg Type.IntType
        | Syntax.AddNode(l, r) -> (l, r) ||> binary KNorm.Add Type.IntType
        | Syntax.SubNode(l, r) -> (l, r) ||> binary KNorm.Sub Type.IntType

        // Операции над числами с плавающей точкой
        | Syntax.FNegNode e -> e |> unary KNorm.FNeg Type.FloatType
        | Syntax.FAddNode(l, r) -> (l, r) ||> binary KNorm.FAdd Type.FloatType
        | Syntax.FSubNode(l, r) -> (l, r) ||> binary KNorm.FSub Type.FloatType
        | Syntax.FMulNode(l, r) -> (l, r) ||> binary KNorm.FMul Type.FloatType
        | Syntax.FDivNode(l, r) -> (l, r) ||> binary KNorm.FDiv Type.FloatType

        // Булевы типы и операции превращаются в целочисленные
        | Syntax.BoolNode b -> KNorm.Int(if b then 1 else 0), Type.IntType
        | Syntax.NotNode e -> Syntax.IfNode(e, Syntax.BoolNode(false), Syntax.BoolNode(true)) |> normalize env
        | Syntax.EqNode _
        | Syntax.LENode _ as cmp ->
            Syntax.IfNode(cmp, Syntax.BoolNode(true), Syntax.BoolNode(false))
            |> normalize env

        // Разворот отрицания в ветвлении через изменение порядка операндов
        | Syntax.IfNode(Syntax.NotNode(cond_e), then_e, else_e) ->
            Syntax.IfNode(cond_e, else_e, then_e) |> normalize env
        | Syntax.IfNode(Syntax.EqNode(l, r), then_e, else_e) ->
            normalize env l
            |> insert_let (fun l' ->
                normalize env r
                |> insert_let (fun r' ->
                    let then_e', then_t = normalize env then_e
                    let else_e', _ = normalize env else_e
                    KNorm.BranchEq(l', r', then_e', else_e'), then_t))
        | Syntax.IfNode(Syntax.LENode(l, r), then_e, else_e) ->
            normalize env l
            |> insert_let (fun l' ->
                normalize env r
                |> insert_let (fun r' ->
                    let then_e', then_t = normalize env then_e
                    let else_e', _ = normalize env else_e
                    KNorm.BranchLE(l', r', then_e', else_e'), then_t))
        | Syntax.IfNode(cond_e, then_e, else_e) ->
            Syntax.IfNode(Syntax.EqNode(cond_e, Syntax.BoolNode false), else_e, then_e)
            |> normalize env

        | Syntax.VarNode name ->
            match env.TryFind name with
            | Some t -> KNorm.Var name, t
            | _ -> failwithf $"variable with a name %s{name} does not found"

        | Syntax.TupleNode exs ->
            let rec bind ids ts =
                function
                // Идентификаторы и типы накапливаются в обратном порядке,
                // по этой причине мы должны в конце развернуть их
                // Альтернативно мы могли бы заменить id :: ids и t :: ts
                // на ids @ [id] и ts @ [t], но это привело бы к тому, что
                // алгоритм из линейного стремился бы к квадратичному,
                // так как каждая конкатенация работает за линейное время
                | [] -> KNorm.Tuple(ids |> List.rev), Type.TupleType(ts |> List.rev)
                | e :: rest ->
                    let _, t as g_e = normalize env e
                    g_e |> insert_let (fun id -> bind (id :: ids) (t :: ts) rest)

            bind [] [] exs
        | Syntax.ApplyNode(Syntax.VarNode f, exs) when (env.TryFind f).IsNone ->
            match Typing.extenv.Value.TryFind f with
            | Some(Type.FunType(_, t)) ->
                let rec bind ids =
                    function
                    | [] -> KNorm.ExtFunApply(f, ids |> List.rev), t
                    | e :: rest -> normalize env e |> insert_let (fun id -> bind (id :: ids) rest)

                bind [] exs
            | _ -> failwith "failed to k-normalize external function application"
        | Syntax.ApplyNode(callee_e, exs) ->
            match normalize env callee_e with
            | _, Type.FunType(_, t) as g_e ->
                g_e
                |> insert_let (fun f ->
                    let rec bind ids =
                        function
                        | [] -> KNorm.Apply(f, ids |> List.rev), t
                        | e :: rest -> normalize env e |> insert_let (fun id -> bind (id :: ids) rest)

                    bind [] exs)
            | _ -> failwith "failed to k-normalize function application"
        
        // Клонирование делегируется вызову внешней функции    
        | Syntax.CloneNode e ->
            let _, e_t as g_e = normalize env e
            g_e |> insert_let (fun e' -> KNorm.ExtFunApply("clone", [e']), e_t)

        // Создание массива делегируется вызову внешней функции
        | Syntax.ArrayNode(v_e, count_e) ->
            normalize env count_e
            |> insert_let (fun count_e' ->
                let _, v_t as g_v_e = normalize env v_e

                g_v_e
                |> insert_let (fun v_e' ->
                    let l =
                        match v_t with
                        | Type.FloatType -> "create_float_array"
                        | _ -> "create_array"

                    KNorm.ExtFunApply(l, [ count_e'; v_e' ]), Type.ArrayType(v_t)))

        | Syntax.GetNode(arr_e, ix_e) ->
            match normalize env arr_e with
            | _, Type.ArrayType t as g_arr_e ->
                g_arr_e
                |> insert_let (fun arr_e' ->
                    normalize env ix_e |> insert_let (fun ix_e' -> KNorm.Get(arr_e', ix_e'), t))
            | _ -> failwith "failed to k-normalize Get node (first operand is not an array)"
        | Syntax.PutNode(arr_e, ix_e, v_e) ->
            normalize env arr_e
            |> insert_let (fun arr_e' ->
                normalize env ix_e
                |> insert_let (fun ix_e' ->
                    normalize env v_e
                    |> insert_let (fun v_e' -> KNorm.Put(arr_e', ix_e', v_e'), Type.UnitType)))

        | Syntax.LetNode((name, t), binding_e, cont_e) ->
            let binding_e', binding_t = normalize env binding_e
            let env' = env.Add name t
            let cont_e', cont_t = normalize env' cont_e
            KNorm.Let((name, binding_t), binding_e', cont_e'), cont_t
        | Syntax.LetRecNode(fun_def, cont_e) ->
            let { Syntax.name = (name, t)
                  Syntax.args = args
                  Syntax.body = body_e } =
                fun_def

            let t = replace_bool t
            let env' = env.Add name t
            let cont_e', cont_t = normalize env' cont_e
            let args = args |> List.map (fun (id, t) -> id, replace_bool t)

            let env'' = env'.AddList args
            let body_e', _ = normalize env'' body_e

            KNorm.LetRec(
                { name = name, replace_bool t
                  args = args
                  body = body_e' },
                cont_e'
            ),
            cont_t
        | Syntax.LetTuple(binds, e, cont_e) ->
            let binds = binds |> List.map (fun (id, t) -> id, replace_bool t)

            normalize env e
            |> insert_let (fun e' ->
                let env' = env.AddList binds
                let cont_e', cont_t = normalize env' cont_e
                KNorm.LetTuple(binds, e', cont_e'), cont_t)

    let f e = normalize (M.Empty()) e |> fst
