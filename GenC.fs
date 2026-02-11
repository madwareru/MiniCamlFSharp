module mini_caml_fsharp.GenC

open mini_caml_fsharp.Id
open mini_caml_fsharp.Type
open mini_caml_fsharp.M
open mini_caml_fsharp.CmmDeclosured

module GenC =
    let private std_prelude =
        @"#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
typedef enum min_caml_unit { UNIT } u_t;
typedef struct min_caml_value {
    union {
        u_t u;
        int64_t i;
        double f;
        struct min_caml_value * v;
    };
    int64_t extra;
} v_t;
static v_t min_caml_make_unit() { 
    v_t res = { .extra = 0, .u = UNIT }; 
    return res;
} 
static v_t min_caml_make_int(int i) { 
    v_t res = { .extra = 0, .i = i }; 
    return res;
} 
static v_t min_caml_make_float(double f) { 
    v_t res = { .extra = 0, .f = f }; 
    return res;
}
static v_t min_caml_alloc_vector(v_t count) {
    if (count.i <= 0) {
        printf(""can't allocate vector with size <= 0"");
        exit(-1);
    }

    v_t* res_v = (v_t*) malloc(sizeof(v_t) * (size_t) count.i);
    if (!res_v) {
        printf(""failed to allocate vector, die in panic, see ya!\n"");
        exit(-1);
    }

    v_t v = min_caml_make_unit();
    v_t res = { .extra = count.i, .v = res_v };
    for(int64_t i = 0; i < count.i; i++) {
        res.v[i] = v;
    }
    return res;
}
static v_t min_caml_clone(v_t v) {
    if (!v.extra)
        return v;

    v_t count = min_caml_make_int(v.extra);
    v_t res = min_caml_alloc_vector(count);
    for(int64_t i = 0; i < v.extra; i++) {
        res.v[i] = min_caml_clone(v.v[i]);
    }
    return res;
}
static v_t min_caml_create_array(v_t count, v_t v) {
    v_t res = min_caml_alloc_vector(count);
    for(int64_t i = 0; i < count.i; i++) {
        res.v[i] = min_caml_clone(v);
    }
    return res;
}
static v_t min_caml_create_float_array(v_t count, v_t v) {
    v_t res = min_caml_alloc_vector(count);
    for(int64_t i = 0; i < count.i; i++) {
        res.v[i].f = v.f;
    }
    return res;
}
static v_t min_caml_float_of_int(v_t v) {
    v_t res = min_caml_make_float((double) v.i);
    return res;
}
static v_t min_caml_int_of_float(v_t v)  {
    v_t res = min_caml_make_int((int64_t) v.f);
    return res;
}
static v_t min_caml_print_int(v_t v) {
    printf(""%lld\n"", v.i);
    return min_caml_make_unit();
}
static v_t min_caml_print_float(v_t v) {
    printf(""%f\n"", v.f);
    return min_caml_make_unit();
}
static v_t min_caml_print_bool(v_t v) {
    if (v.i) {
        printf(""true\n"");
    } else {
        printf(""false\n"");
    }
    return min_caml_make_unit();
}
"

    let private std_epilogue =
        @"int main(void) {
    v_t program_result = min_caml_entry_point();
    return 0;
}
"
    
    let private gen_c_compliant_idents (p : CmmDeclosured.program_t) =
        let mutable env_labels = M.Empty()
        let mutable env_ids = M.Empty()
        let mutable env_id_ts = M.Empty()
        let mutable label_counter = 0
        let mutable id_counter = 0
        
        let gen_label label =
            let (Id.L l) = label
            match env_labels.TryFind l with
            | Some _ -> ()
            | None ->
                let (new_label : Id.t) = $"fn_{label_counter}"
                label_counter <- label_counter + 1
                env_labels <- env_labels.Add l new_label
                ()
                
        let gen_id id t =
            match env_ids.TryFind id with
            | Some _ -> ()
            | None ->
                let (new_id : Id.t) = $"v_{id_counter}"
                id_counter <- id_counter + 1
                env_ids <- env_ids.Add id new_id
                env_id_ts <- env_id_ts.Add id t
                ()
                
        let rec visit_block (b : CmmDeclosured.block_t) =
            match b with
            | CmmDeclosured.Seq(CmmDeclosured.Assignment((id, t), e), next_block) ->
                gen_id id t
                match e with
                | CmmDeclosured.BranchEq(_, _, then_block, else_block)
                | CmmDeclosured.BranchLE(_, _, then_block, else_block) ->
                    visit_block then_block
                    visit_block else_block
                    visit_block next_block
                | _ -> visit_block next_block
            | CmmDeclosured.Return e ->
                match e with
                | CmmDeclosured.BranchEq(_, _, then_block, else_block)
                | CmmDeclosured.BranchLE(_, _, then_block, else_block) ->
                    visit_block then_block
                    visit_block else_block
                | _ -> ()
        
        let visit_fn (fn : CmmDeclosured.fn_t) =
            let l, _ = fn.name
            gen_label l
            for arg_id, arg_t in fn.args do
                gen_id arg_id arg_t
            visit_block fn.body
            
        for fn in p.top_level_functions do
            visit_fn fn
            
        visit_block p.entry
        
        env_labels, env_ids, env_id_ts
        
    let f p =
        let mutable text = std_prelude
        
        let rec print_block (ls : Id.t M) (ids : Id.t M) (id_ts : Type.t M) cont indentation (b : CmmDeclosured.block_t) =
            let find_id id =
                match ids.TryFind id, id_ts.TryFind id with
                | Some id, Some t -> id, t
                | _ -> failwith $"could not find a replacement for id '%s{id}'"
            let find_label l =
                match ls.TryFind l with
                | Some l' -> l'
                | _ -> l
            match b with
            | CmmDeclosured.Seq(CmmDeclosured.Assignment((id, _), expr), next_block) ->
                let id', _ = find_id id
                match expr with
                | CmmDeclosured.BranchEq(a, b, then_block, else_block) ->
                    let a', a_t = find_id a
                    let b', _ = find_id b
                    let comparison_text =
                        match a_t with
                        | Type.UnitType -> "true"
                        | Type.IntType ->  $"%s{a'}.i == %s{b'}.i"
                        | Type.FloatType -> $"%s{a'}.f == %s{b'}.f"
                        | _ -> failwith "todo: support structural comparison"
                    let cont' = (fun (indent, s) -> text <- text + $"%s{indent}/*%s{id}*/ %s{id'} = %s{s};\n")
                    let indentation' = "    " + indentation
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'};\n"
                    text <- text + $"%s{indentation}if (%s{comparison_text}) {{\n"
                    (indentation', then_block) ||> print_block ls ids id_ts cont'
                    text <- text + $"%s{indentation}}} else {{\n"
                    (indentation', else_block) ||> print_block ls ids id_ts cont'
                    text <- text + $"%s{indentation}}}\n"
                    (indentation, next_block) ||> print_block ls ids id_ts cont
                | CmmDeclosured.BranchLE(a, b, then_block, else_block) ->
                    let a', a_t = find_id a
                    let b', _ = find_id b
                    let comparison_text =
                        match a_t with
                        | Type.UnitType -> "true"
                        | Type.IntType ->  $"%s{a'}.i <= %s{b'}.i"
                        | Type.FloatType -> $"%s{a'}.f <= %s{b'}.f"
                        | _ -> failwith "todo: support structural comparison"
                    let cont' = (fun (indent, s) -> text <- text + $"%s{indent}/*%s{id}*/ %s{id'} = %s{s};\n")
                    let indentation' = "    " + indentation
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'};\n"
                    text <- text + $"%s{indentation}if (%s{comparison_text}) {{\n"
                    (indentation', then_block) ||> print_block ls ids id_ts cont'
                    text <- text + $"%s{indentation}}} else {{\n"
                    (indentation', else_block) ||> print_block ls ids id_ts cont'
                    text <- text + $"%s{indentation}}}\n"
                    (indentation, next_block) ||> print_block ls ids id_ts cont
                | CmmDeclosured.Atom(CmmDeclosured.Unit) ->
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_unit();\n"
                    (indentation, next_block) ||> print_block ls ids id_ts cont
                | CmmDeclosured.Atom(CmmDeclosured.Int i) ->
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_int({i});\n"
                    (indentation, next_block) ||> print_block ls ids id_ts cont
                | CmmDeclosured.Atom(CmmDeclosured.Float f) ->
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_float({f});\n"
                    (indentation, next_block) ||> print_block ls ids id_ts cont
                | CmmDeclosured.Var v ->
                    let v', _ = find_id v
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = %s{v'};\n"
                    (indentation, next_block) ||> print_block ls ids id_ts cont
                | CmmDeclosured.Neg v ->
                    let v', _ = find_id v
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_int(-%s{v'}.i);\n"
                    (indentation, next_block) ||> print_block ls ids id_ts cont
                | CmmDeclosured.Add(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_int(%s{a'}.i + %s{b'}.i);\n"
                    (indentation, next_block) ||> print_block ls ids id_ts cont
                | CmmDeclosured.Sub(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_int(%s{a'}.i - %s{b'}.i);\n"
                    (indentation, next_block) ||> print_block ls ids id_ts cont
                | CmmDeclosured.FNeg v ->
                    let v', _ = find_id v
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_float(-%s{v'}.f);\n"
                    (indentation, next_block) ||> print_block ls ids id_ts cont
                | CmmDeclosured.FAdd(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_float(%s{a'}.f + %s{b'}.f);\n"
                    (indentation, next_block) ||> print_block ls ids id_ts cont
                | CmmDeclosured.FSub(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_float(%s{a'}.f - %s{b'}.f);\n"
                    (indentation, next_block) ||> print_block ls ids id_ts cont
                | CmmDeclosured.FMul(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_float(%s{a'}.f * %s{b'}.f);\n"
                    (indentation, next_block) ||> print_block ls ids id_ts cont
                | CmmDeclosured.FDiv(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_float(%s{a'}.f / %s{b'}.f);\n"
                    (indentation, next_block) ||> print_block ls ids id_ts cont
                | CmmDeclosured.MemoryGet(mem, ix) ->
                    let mem', _ = find_id mem
                    let ix', _ = find_id ix
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = %s{mem'}.v[%s{ix'}.i];\n"
                    (indentation, next_block) ||> print_block ls ids id_ts cont
                | CmmDeclosured.MemoryPut(mem, ix, v) ->
                    let mem', _ = find_id mem
                    let ix', _ = find_id ix
                    let v', _ = find_id v
                    text <- text + $"%s{indentation}%s{mem'}.v[%s{ix'}.i] = %s{v'};\n"
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_unit();\n"
                    (indentation, next_block) ||> print_block ls ids id_ts cont
                | CmmDeclosured.Apply(Id.L l, vs) ->
                    let vs' = vs |> List.map find_id |> List.map fst
                    let l' = find_label l
                    match vs' with
                    | [] -> failwith "can't compile, functions with arity 0 not supported"
                    | x :: xs ->
                        let mutable arg_list_str = x
                        for x_next in xs do
                            arg_list_str <- $"%s{arg_list_str}, %s{x_next}"
                        text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = %s{l'}(%s{arg_list_str});\n"
                        (indentation, next_block) ||> print_block ls ids id_ts cont
            | CmmDeclosured.Return e ->
                match e with
                | CmmDeclosured.BranchEq(a, b, then_block, else_block) ->
                    let a', a_t = find_id a
                    let b', _ = find_id b
                    let comparison_text =
                        match a_t with
                        | Type.UnitType -> "true"
                        | Type.IntType ->  $"%s{a'}.i == %s{b'}.i"
                        | Type.FloatType -> $"%s{a'}.f == %s{b'}.f"
                        | _ -> failwith "todo: support structural comparison"
                    let indentation' = "    " + indentation
                    text <- text + $"%s{indentation}if (%s{comparison_text}) {{\n"
                    (indentation', then_block) ||> print_block ls ids id_ts cont
                    text <- text + $"%s{indentation}}} else {{\n"
                    (indentation', else_block) ||> print_block ls ids id_ts cont
                    text <- text + $"%s{indentation}}}\n"
                | CmmDeclosured.BranchLE(a, b, then_block, else_block) ->
                    let a', a_t = find_id a
                    let b', _ = find_id b
                    let comparison_text =
                        match a_t with
                        | Type.UnitType -> "true"
                        | Type.IntType ->  $"%s{a'}.i <= %s{b'}.i"
                        | Type.FloatType -> $"%s{a'}.f <= %s{b'}.f"
                        | _ -> failwith "todo: support structural comparison"
                    let indentation' = "    " + indentation
                    text <- text + $"%s{indentation}if (%s{comparison_text}) {{\n"
                    (indentation', then_block) ||> print_block ls ids id_ts cont
                    text <- text + $"%s{indentation}}} else {{\n"
                    (indentation', else_block) ||> print_block ls ids id_ts cont
                    text <- text + $"%s{indentation}}}\n"
                | CmmDeclosured.Atom(CmmDeclosured.Unit) -> cont(indentation, "min_caml_make_unit()")
                | CmmDeclosured.Atom(CmmDeclosured.Int i) -> cont(indentation, $"min_caml_make_int({i})")
                | CmmDeclosured.Atom(CmmDeclosured.Float f) -> cont(indentation, $"min_caml_make_float({f})")
                | CmmDeclosured.Var v -> 
                    let v', _ = find_id v
                    cont(indentation, v')
                | CmmDeclosured.Neg v ->
                    let v', _ = find_id v
                    cont(indentation, $"min_caml_make_int(-%s{v'}.i)")
                | CmmDeclosured.Add(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b
                    cont(indentation, $"min_caml_make_int(%s{a'}.i + %s{b'}.i)")
                | CmmDeclosured.Sub(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b
                    cont(indentation, $"min_caml_make_int(%s{a'}.i - %s{b'}.i)")
                | CmmDeclosured.FNeg v ->
                    let v', _ = find_id v
                    cont(indentation, $"min_caml_make_float(-%s{v'}.f)")
                | CmmDeclosured.FAdd(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b
                    cont(indentation, $"min_caml_make_float(%s{a'}.f + %s{b'}.f)")
                | CmmDeclosured.FSub(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b
                    cont(indentation, $"min_caml_make_float(%s{a'}.f - %s{b'}.f)")
                | CmmDeclosured.FMul(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b
                    cont(indentation, $"min_caml_make_float(%s{a'}.f * %s{b'}.f)")
                | CmmDeclosured.FDiv(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b
                    cont(indentation, $"min_caml_make_float(%s{a'}.f / %s{b'}.f)")
                | CmmDeclosured.MemoryGet(mem, ix) ->
                    let mem', _ = find_id mem
                    let ix', _ = find_id ix
                    cont(indentation, $"%s{mem'}.v[%s{ix'}.i]")
                | CmmDeclosured.MemoryPut(mem, ix, v) ->
                    let mem', _ = find_id mem
                    let ix', _ = find_id ix
                    let v', _ = find_id v
                    text <- text + $"%s{indentation}%s{mem'}.v[%s{ix'}.i] = %s{v'};\n"
                    cont(indentation, "min_caml_make_unit()")
                | CmmDeclosured.Apply(Id.L l, vs) ->
                    let vs' = vs |> List.map find_id |> List.map fst
                    let l' = find_label l
                    match vs' with
                    | [] -> failwith "can't compile, functions with arity 0 not supported"
                    | x :: xs ->
                        let mutable arg_list_str = x
                        for x_next in xs do
                            arg_list_str <- $"%s{arg_list_str}, %s{x_next}"
                        cont(indentation, $"%s{l'}(%s{arg_list_str})")
        
        let print_fn_general fdecl (ls : Id.t M) (ids : Id.t M) (id_ts : Type.t M) (fn : CmmDeclosured.fn_t) =
            let find_id id =
                match ids.TryFind id with
                | Some id -> id
                | _ -> failwith $"could not find a replacement for id '%s{id}'"
                
            let Id.L l, _ = fn.name
            let l' =
                match ls.TryFind l with
                | Some l' -> l'
                | _ -> l
            let vs' = fn.args |> List.map fst |> List.map find_id
            match vs' with
                | [] -> failwith "can't compile, functions with arity 0 not supported"
                | x :: xs ->
                    let mutable arg_list_str = $"v_t {x}"
                    for x_next in xs do
                        arg_list_str <- $"%s{arg_list_str}, v_t %s{x_next}"
                    if fdecl then
                        text <- text + $"v_t /*%s{l}*/ %s{l'}(%s{arg_list_str});\n"
                    else
                        text <- text + $"v_t /*%s{l}*/ %s{l'}(%s{arg_list_str}) {{\n"
                        ("    ", fn.body)
                        ||> print_block ls ids id_ts (fun (indent, s) -> text <- text + $"%s{indent}return %s{s};\n")
                        text <- text + "}\n"
        let print_fn = print_fn_general false
        let print_fn_forward_decl = print_fn_general true
        
        let ls, ids, id_ts = gen_c_compliant_idents p
        
        for fn in p.top_level_functions do
            fn |> print_fn_forward_decl ls ids id_ts
        
        for fn in p.top_level_functions do
            fn |> print_fn ls ids id_ts
            
        text <- text + "static v_t min_caml_entry_point(void) {\n"
        ("    ", p.entry) ||> print_block ls ids id_ts (fun (indent, s) -> text <- text + $"%s{indent}return %s{s};\n")
        text <- text + "}\n"
        
        text <- text + std_epilogue
        
        text