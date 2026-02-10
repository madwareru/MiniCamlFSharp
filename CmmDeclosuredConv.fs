module mini_caml_fsharp.CmmDeclosuredConv

open mini_caml_fsharp.Cmm
open mini_caml_fsharp.CmmDeclosured
open mini_caml_fsharp.Id
open mini_caml_fsharp.Type

module CmmDeclosuredConv =
    let rec private convert_block (block : Cmm.block_t) =
        match block with
        | Cmm.Seq(Cmm.Assignment(n, e), next) ->
            CmmDeclosured.Seq(CmmDeclosured.Assignment(n, e |> convert_expr), next |> convert_block)
        | Cmm.Return e -> CmmDeclosured.Return(e |> convert_expr)
    
    and private convert_expr (e : Cmm.expr_t) =
        match e with
        | Cmm.Atom(Cmm.Unit) -> CmmDeclosured.Atom(CmmDeclosured.Unit)
        | Cmm.Atom(Cmm.Int i) -> CmmDeclosured.Atom(CmmDeclosured.Int i)
        | Cmm.Atom(Cmm.Float f) -> CmmDeclosured.Atom(CmmDeclosured.Float f)
        | Cmm.Var x -> CmmDeclosured.Var x
        | Cmm.Neg op -> CmmDeclosured.Neg op
        | Cmm.Add(l, r) -> CmmDeclosured.Add(l, r)
        | Cmm.Sub(l, r) -> CmmDeclosured.Sub(l, r)
        | Cmm.FNeg op -> CmmDeclosured.FNeg op
        | Cmm.FAdd(l, r) -> CmmDeclosured.FAdd(l, r)
        | Cmm.FSub(l, r) -> CmmDeclosured.FSub(l, r)
        | Cmm.FMul(l, r) -> CmmDeclosured.FMul(l, r)
        | Cmm.FDiv(l, r) -> CmmDeclosured.FDiv(l, r)
        | Cmm.MemoryGet(mem, ix) -> CmmDeclosured.MemoryGet(mem, ix)
        | Cmm.MemoryPut(mem, ix, v) -> CmmDeclosured.MemoryPut(mem, ix, v)
        | Cmm.ExternalMemory x -> CmmDeclosured.ExternalMemory x
        | Cmm.BranchEq(a, b, then_block, else_block) ->
            CmmDeclosured.BranchEq(a, b, then_block |> convert_block, else_block |> convert_block)
        | Cmm.BranchLE(a, b, then_block, else_block) ->
            CmmDeclosured.BranchLE(a, b, then_block |> convert_block, else_block |> convert_block)
        | Cmm.ApplyDirect(l, args) -> CmmDeclosured.Apply(l, args)
        // Имя замыкания и имя метки совпадают, поэтому можно сделать такое преобразование
        | Cmm.ApplyClosure(name, args) -> CmmDeclosured.Apply(Id.L name, name :: args)
    
    let private convert_fn (fn : Cmm.fn_t) =
        if fn.free_vars.IsEmpty then
            {
                CmmDeclosured.name = fn.name
                CmmDeclosured.args = fn.args
                CmmDeclosured.body = fn.body |> convert_block
            }
        else
            let free_vars_ts = fn.args |> List.map snd
            let Id.L closure_id, _ = fn.name
            
            let closure_t = Type.TupleType <| free_vars_ts
            
            let args' = (closure_id, closure_t) :: fn.args
            
            let mutable body' = fn.body |> convert_block
            let mutable i = fn.free_vars.Length-1
            for id, t in fn.free_vars |> List.rev do
                let id_i = Id.gen_tmp Type.IntType
                body' <- CmmDeclosured.Seq(
                    CmmDeclosured.Assignment((id, t), CmmDeclosured.MemoryGet(closure_id, id_i)),
                    body'
                )
                body' <- CmmDeclosured.Seq(
                    CmmDeclosured.Assignment((id_i, Type.IntType), CmmDeclosured.Atom <| CmmDeclosured.Int i),
                    body'
                )
                i <- i - 1
            {
                CmmDeclosured.name = fn.name
                CmmDeclosured.args = args'
                CmmDeclosured.body = body'
            }
    
    let f (p : Cmm.program_t) =
        let top_level_functions' = p.top_level_functions |> List.map convert_fn
        let entry' = p.entry |> convert_block
        {
            CmmDeclosured.top_level_functions = top_level_functions'
            CmmDeclosured.entry = entry'
        }    