module mini_caml_fsharp.CmmConv

open mini_caml_fsharp.Id
open mini_caml_fsharp.Type
open mini_caml_fsharp.M
open mini_caml_fsharp.ClosureRepresentation
open mini_caml_fsharp.Cmm

module CmmConv =
    open Cmm

    type private free_var_m = (Id.t * Type.t) list M

    let rec private convert_expr (top_level_free_var_map: free_var_m) (env: Type.t M) e : block_t =
        match e with
        | ClosureRepresentation.Unit -> Return <| Atom Unit
        | ClosureRepresentation.Int i -> Return(Atom <| Int i)
        | ClosureRepresentation.Float f -> Return(Atom <| Float f)
        | ClosureRepresentation.Var var_name -> Return <| Var var_name

        // Операции над целыми:
        | ClosureRepresentation.Neg op -> Return <| Neg op
        | ClosureRepresentation.Add(lhs, rhs) -> Return <| Add(lhs, rhs)
        | ClosureRepresentation.Sub(lhs, rhs) -> Return <| Sub(lhs, rhs)

        // Операции над числами с плавающей запятой:
        | ClosureRepresentation.FNeg op -> Return <| FNeg op
        | ClosureRepresentation.FAdd(lhs, rhs) -> Return <| FAdd(lhs, rhs)
        | ClosureRepresentation.FSub(lhs, rhs) -> Return <| FSub(lhs, rhs)
        | ClosureRepresentation.FMul(lhs, rhs) -> Return <| FMul(lhs, rhs)
        | ClosureRepresentation.FDiv(lhs, rhs) -> Return <| FDiv(lhs, rhs)

        // Операции над памятью:
        | ClosureRepresentation.Get(name, ix) -> Return <| MemoryGet(name, ix)
        | ClosureRepresentation.Put(name, ix, v) -> Return <| MemoryPut(name, ix, v)

        // Ветвления:
        | ClosureRepresentation.BranchEq(lhs, rhs, then_e, else_e) ->
            let then_e' = then_e |> convert_expr top_level_free_var_map env
            let else_e' = else_e |> convert_expr top_level_free_var_map env
            Return <| BranchEq(lhs, rhs, then_e', else_e')
        | ClosureRepresentation.BranchLE(lhs, rhs, then_e, else_e) ->
            let then_e' = then_e |> convert_expr top_level_free_var_map env
            let else_e' = else_e |> convert_expr top_level_free_var_map env
            Return <| BranchLE(lhs, rhs, then_e', else_e')

        | ClosureRepresentation.ApplyDirect(l, ops) -> Return <| ApplyDirect(l, ops)
        | ClosureRepresentation.ApplyClosure(id, ops) -> Return <| ApplyClosure(id, ops)

        | ClosureRepresentation.Tuple ops ->
            // Разворачиваем создание кортежа в последовательность из
            // выделения памяти и заполнения его с последующим возвратом.
            //
            // К примеру, (, a b c d e f) : (, i i f f f i) превратится (в псевдокоде) в следующий текст:
            // new_memory : (, i i f f f i) := min_caml_alloc_vector(6);
            // id_0 : i := 0;
            // new_memory[id_0] <- a;
            // id_1 : i := 1;
            // new_memory[1] <- b;
            // id_2 : i := 2;
            // new_memory[id_2] <- c;
            // id_3 : i := 3;
            // new_memory[id_3] <- d;
            // id_4 : i := 4;
            // new_memory[id_4] <- e;
            // id_5 : i := 5;
            // new_memory[id_5] <- f;
            // return new_memory;
            // Порядок присваиваний тут получается обратный, но на семантику это не влияет

            let mutable op_ts = []

            for name in ops do
                match env.TryFind name with
                | None -> failwithf $"the type of a variable with a name {name} not found"
                | Some t -> op_ts <- t :: op_ts

            op_ts <- op_ts |> List.rev
            let t = Type.TupleType op_ts

            let memory_id = Id.gen_tmp <| Type.TupleType op_ts
            // сгенерировано return new_memory;
            let mutable ret = Return <| Var memory_id
            let mutable i = ops.Length - 1

            for op in ops |> List.rev do
                // Генерация
                // $tmp_id_i := $i;
                // new_memory[$tmp_id_i] <- $op;
                // $ret
                // Операции собираются в обратном порядке (с хвоста), по этой причине мы сначала кладём данные по
                // ещё не присвоенному индексу, а уже после этого присваем значение самому индексу
                let id_i = Id.gen_tmp Type.IntType
                ret <- Seq(Assignment((Id.gen_tmp Type.UnitType, Type.UnitType), MemoryPut(memory_id, id_i, op)), ret)
                ret <- Seq(Assignment((id_i, Type.IntType), Atom <| Int i), ret)
                i <- i - 1

            let count_id = Id.gen_tmp Type.IntType
            let apply_expr = ApplyDirect(Id.L "min_caml_alloc_vector", [ count_id ])
            let result_assignment = Assignment((memory_id, t), apply_expr)
            ret <- Seq(result_assignment, ret)

            let count_id_assignment =
                Assignment((count_id, Type.IntType), Atom <| Int ops.Length)

            ret <- Seq(count_id_assignment, ret)

            ret

        | ClosureRepresentation.LetTuple(idts, v, cont) ->
            // Работает по тому же принципу, что и Tupple, только в обратную сторону,
            // и в данном случае нам необходимо в наш env' добавить вновь созданные переменные,
            // чтобы cont вычислялся со знанием об их типах
            let env' = env.AddList idts
            let mutable ret = cont |> convert_expr top_level_free_var_map env'
            let mutable i = idts.Length - 1

            for id, t in idts |> List.rev do
                let id_i = Id.gen_tmp Type.IntType
                ret <- Seq(Assignment((id, t), MemoryGet(v, id_i)), ret)
                ret <- Seq(Assignment((id_i, Type.IntType), Atom <| Int i), ret)
                i <- i - 1

            ret

        | ClosureRepresentation.Let((id, t), binding, cont) ->
            let binding' = binding |> convert_expr top_level_free_var_map env
            let env' = env.Add id t
            let cont' = cont |> convert_expr top_level_free_var_map env'

            let rec insert b =
                match b with
                | Seq(stmt, rest) -> Seq(stmt, insert rest)
                | Return e -> Seq(Assignment((id, t), e), cont')

            insert binding'

        | ClosureRepresentation.LetClosure((id, _), Id.L label, cont) ->
            match top_level_free_var_map.TryFind label with
            | None -> failwithf $"free variables not found for function with label '{label}'!"
            | Some free_vars ->
                // Во многом тут мы повторяем код из Tuple,
                // здесь мы тоже выделяем вектор и заполняем его
                // значениями, где первое значение будет ссылкой
                // на функцию верхнего уровня, а следующие за ним
                // значения соответствуют свободным переменным в
                // этой функции.
                let free_vars, free_var_ts = free_vars |> List.unzip
                let closure_t = Type.TupleType(Type.FunctionLabel :: free_var_ts)

                let env' = env.Add id closure_t

                let mutable ret = cont |> convert_expr top_level_free_var_map env'

                let l = free_vars.Length + 1
                let mutable i = l - 1

                for var_name in free_vars |> List.rev do
                    let id_i = Id.gen_tmp Type.IntType

                    let mem_put = MemoryPut(id, id_i, var_name)
                    let hole_id = Id.gen_tmp Type.UnitType

                    ret <- Seq(Assignment((hole_id, Type.UnitType), mem_put), ret)
                    ret <- Seq(Assignment((id_i, Type.IntType), Atom <| Int i), ret)
                    i <- i - 1

                let fn_ptr_id_tmp = Id.gen_tmp Type.FunctionLabel
                let id_i = Id.gen_tmp Type.IntType

                let mem_put = MemoryPut(id, id_i, fn_ptr_id_tmp)
                let hole_id = Id.gen_tmp Type.UnitType

                ret <- Seq(Assignment((hole_id, Type.UnitType), mem_put), ret)
                ret <- Seq(Assignment((id_i, Type.IntType), Atom <| Int i), ret)
                ret <- Seq(Assignment((fn_ptr_id_tmp, Type.FunctionLabel), Atom <| FunctionPointer(Id.L label)), ret)

                let count_id = Id.gen_tmp Type.IntType
                let apply_expr = ApplyDirect(Id.L "min_caml_alloc_vector", [ count_id ])
                let result_assignment = Assignment((id, closure_t), apply_expr)
                ret <- Seq(result_assignment, ret)

                let count_id_assignment = Assignment((count_id, Type.IntType), Atom <| Int l)
                ret <- Seq(count_id_assignment, ret)

                ret

    let private convert_function top_level_free_var_map (fn: ClosureRepresentation.fundef) =
        let mutable env = M.Empty()

        match fn.is_closure with
        | false ->
            for name, t in fn.args do
                env <- env.Add name t

            { name = fn.name
              args = fn.args
              body = fn.body |> convert_expr top_level_free_var_map env }
        | true ->
            for name, t in fn.free_vars do
                env <- env.Add name t

            let free_var_ts = fn.free_vars |> List.map snd
            let closure_t = Type.TupleType(Type.FunctionLabel :: free_var_ts)

            let Id.L label, t = fn.name
            let new_args = (label, closure_t) :: fn.args

            let t =
                match t with
                | Type.FunType(_, ret_t) -> Type.FunType(new_args |> List.map snd, ret_t)
                | _ -> failwith "unreachable"

            env <- env.Add label closure_t

            for name, t in fn.args do
                env <- env.Add name t

            let mutable ret = fn.body |> convert_expr top_level_free_var_map env

            let mutable i = fn.free_vars.Length

            for id, t in fn.free_vars |> List.rev do
                let id_i = Id.gen_tmp Type.IntType
                ret <- Seq(Assignment((id, t), MemoryGet(label, id_i)), ret)
                ret <- Seq(Assignment((id_i, Type.IntType), Atom <| Int i), ret)
                i <- i - 1

            { name = (Id.L label, t)
              args = new_args
              body = ret }

    let f (p: ClosureRepresentation.program) =
        let mutable top_level_free_var_map = M.Empty()

        for fn_def in p.top_level_functions do
            let Id.L name, _ = fn_def.name
            top_level_free_var_map <- top_level_free_var_map.Add name fn_def.free_vars

        let top_level_functions =
            p.top_level_functions |> List.map (convert_function top_level_free_var_map)

        let entry = p.main |> convert_expr top_level_free_var_map (M.Empty())

        { top_level_functions = top_level_functions
          entry = entry }
