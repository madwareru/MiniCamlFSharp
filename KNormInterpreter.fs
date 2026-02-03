module mini_caml_fsharp.KNormInterpreter

open System
open NUnit.Framework
open mini_caml_fsharp.Type
open mini_caml_fsharp.KNorm
open mini_caml_fsharp.M

module KNormInterpreter =
    type value_t =
        | Unit
        | Int of int64
        | Float of double
        | Tuple of value_t list
        | Array of array_t
        | Func of func_t
    and func_t = {
        recursive_name: string
        func_type: Type.t
        arg_names: string list
        arg_types: Type.t list
        env: value_t M
        body: KNorm.t
    }
    and array_t = {
        element_type: Type.t
        value: value_t array
    }
    
    let rec element_type v =
        match v with
        | value_t.Unit -> Type.UnitType
        | value_t.Int _ -> Type.IntType
        | value_t.Float _ -> Type.FloatType
        | value_t.Tuple values -> Type.TupleType (values |> List.map element_type)
        | value_t.Array { element_type = t } -> Type.ArrayType t
        | value_t.Func { func_type = t } -> t

    let rec private interpret (env : value_t M) (e : KNorm.t) =
        let rec check_type v t =
            let v_t = element_type v
            match v_t, t with
            | Type.UnitType, Type.UnitType -> ()
            | Type.IntType, Type.IntType -> ()
            | Type.FloatType, Type.FloatType -> ()
            | Type.TupleType values, Type.TupleType value_types ->
                if values.Length <> value_types.Length then
                    failwith "value has a type of a tuple with wrong arity"
                else
                    for v_t, t in (List.zip values value_types) do
                        if v_t <> t then
                            failwith "tuple element types aren't match!"
                    ()
            | Type.ArrayType et, Type.ArrayType t ->
                if et <> t then
                    failwith "array element types aren't match"
                else
                    ()
            | Type.FunType(ts, t), Type.FunType(arg_ts, ret_t) ->
                match (ts, t) with
                | ts, _ when ts.Length <> arg_ts.Length ->
                    failwith "function arity aren't match!"
                | ts, _ when (List.zip ts arg_ts) |> List.exists (fun (l, r) -> not(l.Equals(r))) ->
                    failwith "argument types aren't match!"
                | _, t when not(t.Equals(ret_t)) ->
                    failwith "return types aren't match!"
                | _ -> ()
            | _ -> failwith "types aren't match!"
        
        let lookup_var var_name =
            match env.TryFind var_name with
            | Some v -> v
            | _ -> failwithf $"name %s{var_name} not found in an environment!"
        
        match e with
        // Простые литералы:
        | KNorm.Unit -> value_t.Unit
        | KNorm.Int i -> value_t.Int i
        | KNorm.Float f -> value_t.Float f
        | KNorm.Var var_name -> lookup_var var_name
        | KNorm.Tuple ops -> value_t.Tuple(ops |> List.map lookup_var)
        
        // Операции над целыми:
        | KNorm.Neg op ->
            match lookup_var op with
            | value_t.Int i -> value_t.Int -i
            | _ -> failwith "can't da an int negate of a non int type!"
        | KNorm.Add(lhs, rhs) ->
            match lookup_var lhs, lookup_var rhs with
            | value_t.Int lhs, value_t.Int rhs -> value_t.Int(lhs + rhs)
            | _ -> failwith "can't do an int addition of non int types!"
        | KNorm.Sub(lhs, rhs) ->
            match lookup_var lhs, lookup_var rhs with
            | value_t.Int lhs, value_t.Int rhs -> value_t.Int(lhs - rhs)
            | _ -> failwith "can't do an int subtraction of non int types!"
        
        // Операции над числами с плавающей запятой:
        | KNorm.FNeg op ->
            match lookup_var op with
            | value_t.Float f -> value_t.Float -f
            | _ -> failwith "can't da a float negate of a non float type!"
        | KNorm.FAdd(lhs, rhs) ->
            match lookup_var lhs, lookup_var rhs with
            | value_t.Float lhs, value_t.Float rhs -> value_t.Float(lhs + rhs)
            | _ -> failwith "can't do an float addition of non float types!"
        | KNorm.FSub(lhs, rhs) ->
            match lookup_var lhs, lookup_var rhs with
            | value_t.Float lhs, value_t.Float rhs -> value_t.Float(lhs - rhs)
            | _ -> failwith "can't do an float subtraction of non float types!"
        | KNorm.FMul(lhs, rhs) ->
            match lookup_var lhs, lookup_var rhs with
            | value_t.Float lhs, value_t.Float rhs -> value_t.Float(lhs * rhs)
            | _ -> failwith "can't do an float multipy of non float types!"
        | KNorm.FDiv(lhs, rhs) ->
            match lookup_var lhs, lookup_var rhs with
            | value_t.Float lhs, value_t.Float rhs -> value_t.Float(lhs / rhs)
            | _ -> failwith "can't do an float subtraction of non float types!"
            
        // Операции над массивами:
        | KNorm.ExtArray _ -> failwith "todo: ext array"
        | KNorm.Get(arr_name, ix) ->
            match lookup_var arr_name with
            | value_t.Array { value = arr } ->
                match lookup_var ix with
                | value_t.Int ix -> arr[int ix]
                | _ -> failwith "an array index should be of type int!"
            | _ -> failwith "can't do an array get on a non array type!"
        | KNorm.Put(arr_name, ix, v) ->
            match lookup_var arr_name with
            | value_t.Array { element_type = et; value = arr } ->
                match lookup_var ix with
                | value_t.Int ix ->
                    let value = lookup_var v
                    check_type value et
                    arr[int ix] <- value
                    value_t.Unit
                | _ -> failwith "an array index should be of type int!"
            | _ -> failwith "can't do an array put on a non array type!"
            
        // Связывания имён:
        | KNorm.Let((name, t), body, cont) ->
            let res = body |> interpret env
            check_type res t
            let env' = env.Add name res
            cont |> interpret env'
        | KNorm.LetTuple(bs, var_name, cont) ->
            match lookup_var var_name with
            | value_t.Tuple vals ->
                if vals.Length <> bs.Length then
                    failwith "tuple arity aren't match!"
                else
                    let bound_names, bound_types = List.unzip bs
                    let mutable env' = env
                    
                    for b_name, b_type, v in (List.zip3 bound_names bound_types vals) do
                        check_type v b_type
                        env' <- env'.Add b_name v
                    cont |> interpret env'
            | _ -> failwith "value of a let binding should be of tuple type!"
        | KNorm.LetRec({ name = (name, t)
                         args = args
                         body = body }, cont ) ->
            let arg_names, arg_types = args |> List.unzip
            let v = value_t.Func { recursive_name = name
                                   func_type = t
                                   arg_names = arg_names
                                   arg_types = arg_types
                                   env = env
                                   body = body }
            let env' = env.Add name v
            cont |> interpret env'
            
        // Ветвления:
        | KNorm.BranchEq(lhs, rhs, then_e, else_e) ->
            match lookup_var lhs, lookup_var rhs with
            | value_t.Unit, value_t.Unit -> then_e |> interpret env
            | value_t.Int lhs, value_t.Int rhs when lhs = rhs ->
                then_e |> interpret env
            | value_t.Float lhs, value_t.Float rhs when lhs = rhs ->
                then_e |> interpret env
            | value_t.Int _, value_t.Int _
            | value_t.Float _, value_t.Float _ ->
                else_e |> interpret env
            | _ -> failwith "todo: implement comparison of complex types"
        | KNorm.BranchLE(lhs, rhs, then_e, else_e) ->
            match lookup_var lhs, lookup_var rhs with
            | value_t.Unit, value_t.Unit -> then_e |> interpret env
            | value_t.Int lhs, value_t.Int rhs when lhs <= rhs ->
                then_e |> interpret env
            | value_t.Float lhs, value_t.Float rhs when lhs <= rhs ->
                then_e |> interpret env
            | value_t.Int _, value_t.Int _
            | value_t.Float _, value_t.Float _ ->
                else_e |> interpret env
            | _ -> failwith "todo: implement comparison of complex types"
            
        // Применения функций:
        | KNorm.Apply(func_name, args) ->
            match lookup_var func_name with
            | value_t.Func { recursive_name = name
                             func_type = t
                             arg_names = arg_names
                             arg_types = arg_types
                             env = func_env
                             body = body } as func ->
                if args.Length <> arg_names.Length then
                    failwith "function application: arity aren't match!"
                else
                    let mutable env' = func_env.Add name func
                    for name, arg_t, v in (List.zip3 arg_names arg_types args) do
                        let v' = lookup_var v
                        check_type v' arg_t
                        env' <- env'.Add name v'
                    let res = body |> interpret env'
                    match t with
                    | Type.FunType(_, ret_t) ->
                        check_type res ret_t
                        res
                    | _ -> failwith "unreachable"
            | _ -> failwith "can not apply a non function type!"
        | KNorm.ExtFunApply(func_name, args) ->
            match func_name, args with
            | "create_float_array", [count; v] ->
                match lookup_var count, lookup_var v with
                | value_t.Int count, (value_t.Float _ as v) ->
                    value_t.Array { element_type = Type.FloatType; value = Array.create (int count) v }
                | _ -> failwith "create_float_array: count should be of type int and value should be of type float"
            | "create_array", [count; v] ->
                match lookup_var count, lookup_var v with
                | value_t.Int count, (value_t.Int _ as v) ->
                    value_t.Array { element_type = Type.IntType; value = Array.create (int count) v }
                | value_t.Int count, v ->
                    let element_type = element_type v
                    value_t.Array { element_type = element_type; value = Array.create (int count) v }
                | _ -> failwith "create_array: count should be of type int"
            | _ -> failwith "unknown external function"
        
    let f e = interpret (M.Empty ()) e