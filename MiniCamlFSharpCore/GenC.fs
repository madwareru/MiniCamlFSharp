module mini_caml_fsharp_core.GenC

open mini_caml_fsharp_core.Id
open mini_caml_fsharp_core.Type
open mini_caml_fsharp_core.M
open mini_caml_fsharp_core.Cmm
open mini_caml_fsharp_core.GenShared

module GenC =
    let private std_prelude =
        @"#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
typedef enum min_caml_tag {
    UNIT_TAG,
    INT_TAG,
    FLOAT_TAG,
    MEMORY_TAG,
    FUNCTION_PTR_TAG
} tag_t;
typedef enum min_caml_unit { UNIT } u_t;

struct min_caml_value;
typedef struct min_caml_memory {
    struct min_caml_value * v;
    int64_t length;
} mem_t;

typedef struct min_caml_value {
    tag_t tag;
    union {
        u_t u;
        int64_t i;
        double f;
        mem_t m;
        void* f_ptr;
    };
} v_t;
static v_t min_caml_make_unit() {
    v_t res = { .tag = UNIT_TAG, .u = UNIT };
    return res;
}
static v_t min_caml_make_int(int i) {
    v_t res = { .tag = INT_TAG, .i = i };
    return res;
}
static v_t min_caml_make_float(double f) {
    v_t res = { .tag = FLOAT_TAG, .f = f };
    return res;
}
static v_t min_caml_make_f_ptr(void* f_ptr) {
    v_t res = { .tag = FUNCTION_PTR_TAG, .f_ptr = f_ptr };
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
    mem_t mem = { .length = count.i, .v = res_v };
    v_t res = { .tag = MEMORY_TAG, .m = mem };
    for(int64_t i = 0; i < count.i; i++) {
        res.m.v[i] = v;
    }
    return res;
}
static v_t min_caml_clone(v_t v) {
    if (v.tag != MEMORY_TAG)
        return v;

    v_t count = min_caml_make_int(v.m.length);
    v_t res = min_caml_alloc_vector(count);
    for(int64_t i = 0; i < v.m.length; i++) {
        res.m.v[i] = min_caml_clone(v.m.v[i]);
    }
    return res;
}
static v_t min_caml_create_array(v_t count, v_t v) {
    v_t res = min_caml_alloc_vector(count);
    for(int64_t i = 0; i < count.i; i++) {
        res.m.v[i] = min_caml_clone(v);
    }
    return res;
}
static v_t min_caml_create_float_array(v_t count, v_t v) {
    v_t res = min_caml_alloc_vector(count);
    for(int64_t i = 0; i < count.i; i++) {
        res.m.v[i].tag = FLOAT_TAG;
        res.m.v[i].f = v.f;
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
    printf(""%lld"", v.i);
    return min_caml_make_unit();
}
static v_t min_caml_print_int_ln(v_t v) {
    printf(""%lld\n"", v.i);
    return min_caml_make_unit();
}
static v_t min_caml_print_float(v_t v) {
    printf(""%f"", v.f);
    return min_caml_make_unit();
}
static v_t min_caml_print_float_ln(v_t v) {
    printf(""%f\n"", v.f);
    return min_caml_make_unit();
}
static v_t min_caml_print_bool(v_t v) {
    if (v.i) {
        printf(""true"");
    } else {
        printf(""false"");
    }
    return min_caml_make_unit();
}
static v_t min_caml_print_bool_ln(v_t v) {
    if (v.i) {
        printf(""true\n"");
    } else {
        printf(""false\n"");
    }
    return min_caml_make_unit();
}
static v_t min_caml_print_ln(v_t v_unused) {
    printf(""\n"");
    return min_caml_make_unit();
}
static v_t min_caml_print_tab(v_t v_unused) {
    printf(""\t"");
    return min_caml_make_unit();
}
static v_t min_caml_put_char(v_t v) {
    putchar((char)(v.i & 0xFF));
    return min_caml_make_unit();
}
static v_t min_caml_less_eq(v_t lhs, v_t rhs) {
    switch(lhs.tag) {
        case UNIT_TAG:
            return min_caml_make_int(1);
        case INT_TAG:
            return min_caml_make_int(lhs.i <= rhs.i ? 1 : 0);
        case FLOAT_TAG:
            return min_caml_make_int(lhs.f <= rhs.f ? 1 : 0);
        case FUNCTION_PTR_TAG:
            return min_caml_make_int(lhs.f_ptr == rhs.f_ptr ? 1 : 0);
        default:
            for(int64_t i = 0; i < lhs.m.length; i++) {
                v_t r = min_caml_less_eq(lhs.m.v[i], rhs.m.v[i]);
                if (!r.i)
                    return r;
            }
            return min_caml_make_int(1);
    }
}
static v_t min_caml_eq(v_t lhs, v_t rhs) {
    switch(lhs.tag) {
        case UNIT_TAG:
            return min_caml_make_int(1);
        case INT_TAG:
            return min_caml_make_int(lhs.i == rhs.i ? 1 : 0);
        case FLOAT_TAG:
            return min_caml_make_int(lhs.f == rhs.f ? 1 : 0);
        case FUNCTION_PTR_TAG:
            return min_caml_make_int(lhs.f_ptr == rhs.f_ptr ? 1 : 0);
        default:
            for(int64_t i = 0; i < lhs.m.length; i++) {
                v_t r = min_caml_eq(lhs.m.v[i], rhs.m.v[i]);
                if (!r.i)
                    return r;
            }
            return min_caml_make_int(1);
    }
}
"

    let private std_epilogue =
        @"int main(void) {
    v_t program_result = min_caml_entry_point();
    return 0;
}
"

    let f (p: Cmm.program_t) =
        let mutable text = std_prelude

        let ls, ids, id_ts, l_ts = GenShared.gen_c_compliant_idents p

        let rec print_block (env: (Id.t * Type.t) M) cont indentation (b: Cmm.block_t) =
            let find_label l =
                match ls.TryFind l, l_ts.TryFind l with
                | Some l', Some t -> l', t
                | _ -> l, Type.FunctionLabel

            let find_replacement_id id =
                match ids.TryFind id, id_ts.TryFind id with
                | Some id, Some t -> id, t
                | _ -> failwith $"could not find a replacement for id '%s{id}'"

            let find_id id =
                match env.TryFind id with
                | Some(id, t) -> id, t
                | _ ->
                    let l, t = find_label id
                    $"min_caml_make_f_ptr(%s{l})", t

            match b with
            | Cmm.Seq(Cmm.Assignment((id, _), expr), next_block) ->
                let id', t = find_replacement_id id
                let env' = env.Add id (id', t)

                match expr with
                | Cmm.BranchEq(a, b, then_block, else_block) ->
                    let a', a_t = find_id a
                    let b', _ = find_id b

                    let comparison_text =
                        match a_t with
                        | Type.UnitType -> "true"
                        | Type.IntType -> $"%s{a'}.i == %s{b'}.i"
                        | Type.FloatType -> $"%s{a'}.f == %s{b'}.f"
                        | Type.FunType _ -> $"%s{a'}.f_ptr == %s{b'}.f_ptr"
                        | _ -> $"min_caml_eq(%s{a'}, %s{b'})"

                    let cont' =
                        (fun (indent, s) -> text <- text + $"%s{indent}/*%s{id}*/ %s{id'} = %s{s};\n")

                    let indentation' = "    " + indentation
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'};\n"
                    text <- text + $"%s{indentation}if (%s{comparison_text}) {{\n"
                    (indentation', then_block) ||> print_block env cont'
                    text <- text + $"%s{indentation}}} else {{\n"
                    (indentation', else_block) ||> print_block env cont'
                    text <- text + $"%s{indentation}}}\n"
                    (indentation, next_block) ||> print_block env' cont
                | Cmm.BranchLE(a, b, then_block, else_block) ->
                    let a', a_t = find_id a
                    let b', _ = find_id b

                    let comparison_text =
                        match a_t with
                        | Type.UnitType -> "true"
                        | Type.IntType -> $"%s{a'}.i <= %s{b'}.i"
                        | Type.FloatType -> $"%s{a'}.f <= %s{b'}.f"
                        | Type.FunType _ -> $"%s{a'}.f_ptr == %s{b'}.f_ptr"
                        | _ -> $"min_caml_less_eq(%s{a'}, %s{b'})"

                    let cont' =
                        (fun (indent, s) -> text <- text + $"%s{indent}/*%s{id}*/ %s{id'} = %s{s};\n")

                    let indentation' = "    " + indentation
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'};\n"
                    text <- text + $"%s{indentation}if (%s{comparison_text}) {{\n"
                    (indentation', then_block) ||> print_block env cont'
                    text <- text + $"%s{indentation}}} else {{\n"
                    (indentation', else_block) ||> print_block env cont'
                    text <- text + $"%s{indentation}}}\n"
                    (indentation, next_block) ||> print_block env' cont
                | Cmm.Atom(Cmm.Unit) ->
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_unit();\n"
                    (indentation, next_block) ||> print_block env' cont
                | Cmm.Atom(Cmm.Int i) ->
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_int({i});\n"
                    (indentation, next_block) ||> print_block env' cont
                | Cmm.Atom(Cmm.Float f) ->
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_float(%f{f});\n"
                    (indentation, next_block) ||> print_block env' cont
                | Cmm.Atom(Cmm.FunctionPointer(Id.L l)) ->
                    let l', _ = find_label l
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_f_ptr(%s{l'});\n"
                    (indentation, next_block) ||> print_block env' cont
                | Cmm.Var v ->
                    let v', _ = find_id v
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = %s{v'};\n"
                    (indentation, next_block) ||> print_block env' cont
                | Cmm.Neg v ->
                    let v', _ = find_id v

                    text <-
                        text
                        + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_int(-%s{v'}.i);\n"

                    (indentation, next_block) ||> print_block env' cont
                | Cmm.Add(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b

                    text <-
                        text
                        + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_int(%s{a'}.i + %s{b'}.i);\n"

                    (indentation, next_block) ||> print_block env' cont
                | Cmm.Sub(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b

                    text <-
                        text
                        + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_int(%s{a'}.i - %s{b'}.i);\n"

                    (indentation, next_block) ||> print_block env' cont
                | Cmm.FNeg v ->
                    let v', _ = find_id v

                    text <-
                        text
                        + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_float(-%s{v'}.f);\n"

                    (indentation, next_block) ||> print_block env' cont
                | Cmm.FAdd(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b

                    text <-
                        text
                        + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_float(%s{a'}.f + %s{b'}.f);\n"

                    (indentation, next_block) ||> print_block env' cont
                | Cmm.FSub(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b

                    text <-
                        text
                        + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_float(%s{a'}.f - %s{b'}.f);\n"

                    (indentation, next_block) ||> print_block env' cont
                | Cmm.FMul(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b

                    text <-
                        text
                        + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_float(%s{a'}.f * %s{b'}.f);\n"

                    (indentation, next_block) ||> print_block env' cont
                | Cmm.FDiv(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b

                    text <-
                        text
                        + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_float(%s{a'}.f / %s{b'}.f);\n"

                    (indentation, next_block) ||> print_block env' cont
                | Cmm.MemoryGet(mem, ix) ->
                    let mem', _ = find_id mem
                    let ix', _ = find_id ix
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = %s{mem'}.m.v[%s{ix'}.i];\n"
                    (indentation, next_block) ||> print_block env' cont
                | Cmm.MemoryPut(mem, ix, v) ->
                    let mem', _ = find_id mem
                    let ix', _ = find_id ix
                    let v', _ = find_id v
                    text <- text + $"%s{indentation}%s{mem'}.m.v[%s{ix'}.i] = %s{v'};\n"
                    text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = min_caml_make_unit();\n"
                    (indentation, next_block) ||> print_block env' cont
                | Cmm.ApplyDirect(Id.L l, vs) ->
                    let vs' = vs |> List.map find_id |> List.map fst
                    let l', _ = find_label l

                    match vs' with
                    | [] -> failwith "can't compile, functions with arity 0 not supported"
                    | x :: xs ->
                        let mutable arg_list_str = x

                        for x_next in xs do
                            arg_list_str <- $"%s{arg_list_str}, %s{x_next}"

                        text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = %s{l'}(%s{arg_list_str});\n"
                        (indentation, next_block) ||> print_block env' cont
                | Cmm.ApplyClosure(f_id, vs) ->
                    let f_id', _ = find_id f_id
                    let vs' = vs |> List.map find_id |> List.map fst

                    match vs' with
                    | [] -> failwith "can't compile, functions with arity 0 not supported"
                    | x :: xs ->
                        let mutable fn_type_str = "v_t (*)(v_t"

                        for _ in 1 .. vs.Length do
                            fn_type_str <- $"{fn_type_str}, v_t"

                        fn_type_str <- $"{fn_type_str})"
                        let mutable arg_list_str = $"%s{f_id'}, %s{x}"

                        for x_next in xs do
                            arg_list_str <- $"%s{arg_list_str}, %s{x_next}"

                        text <- text + $"%s{indentation}v_t /*%s{id}*/ %s{id'} = "
                        text <- text + $"((%s{fn_type_str})%s{f_id'}.m.v[0].f_ptr)(%s{arg_list_str});\n"
                        (indentation, next_block) ||> print_block env' cont

            | Cmm.Return e ->
                match e with
                | Cmm.BranchEq(a, b, then_block, else_block) ->
                    let a', a_t = find_id a
                    let b', _ = find_id b

                    let comparison_text =
                        match a_t with
                        | Type.UnitType -> "true"
                        | Type.IntType -> $"%s{a'}.i == %s{b'}.i"
                        | Type.FloatType -> $"%s{a'}.f == %s{b'}.f"
                        | Type.FunType _ -> $"%s{a'}.f_ptr == %s{b'}.f_ptr"
                        | _ -> $"min_caml_eq(%s{a'}, %s{b'})"

                    let indentation' = "    " + indentation
                    text <- text + $"%s{indentation}if (%s{comparison_text}) {{\n"
                    (indentation', then_block) ||> print_block env cont
                    text <- text + $"%s{indentation}}} else {{\n"
                    (indentation', else_block) ||> print_block env cont
                    text <- text + $"%s{indentation}}}\n"
                | Cmm.BranchLE(a, b, then_block, else_block) ->
                    let a', a_t = find_id a
                    let b', _ = find_id b

                    let comparison_text =
                        match a_t with
                        | Type.UnitType -> "true"
                        | Type.IntType -> $"%s{a'}.i <= %s{b'}.i"
                        | Type.FloatType -> $"%s{a'}.f <= %s{b'}.f"
                        | Type.FunType _ -> $"%s{a'}.f_ptr == %s{b'}.f_ptr"
                        | _ -> $"min_caml_less_eq(%s{a'}, %s{b'})"

                    let indentation' = "    " + indentation
                    text <- text + $"%s{indentation}if (%s{comparison_text}) {{\n"
                    (indentation', then_block) ||> print_block env cont
                    text <- text + $"%s{indentation}}} else {{\n"
                    (indentation', else_block) ||> print_block env cont
                    text <- text + $"%s{indentation}}}\n"
                | Cmm.Atom(Cmm.Unit) -> cont (indentation, "min_caml_make_unit()")
                | Cmm.Atom(Cmm.Int i) -> cont (indentation, $"min_caml_make_int({i})")
                | Cmm.Atom(Cmm.Float f) -> cont (indentation, $"min_caml_make_float(%f{f})")
                | Cmm.Atom(Cmm.FunctionPointer(Id.L l)) ->
                    let l', _ = find_label l
                    cont (indentation, $"min_caml_make_f_ptr(%s{l'})")
                | Cmm.Var v ->
                    let v', _ = find_id v
                    cont (indentation, v')
                | Cmm.Neg v ->
                    let v', _ = find_id v
                    cont (indentation, $"min_caml_make_int(-%s{v'}.i)")
                | Cmm.Add(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b
                    cont (indentation, $"min_caml_make_int(%s{a'}.i + %s{b'}.i)")
                | Cmm.Sub(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b
                    cont (indentation, $"min_caml_make_int(%s{a'}.i - %s{b'}.i)")
                | Cmm.FNeg v ->
                    let v', _ = find_id v
                    cont (indentation, $"min_caml_make_float(-%s{v'}.f)")
                | Cmm.FAdd(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b
                    cont (indentation, $"min_caml_make_float(%s{a'}.f + %s{b'}.f)")
                | Cmm.FSub(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b
                    cont (indentation, $"min_caml_make_float(%s{a'}.f - %s{b'}.f)")
                | Cmm.FMul(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b
                    cont (indentation, $"min_caml_make_float(%s{a'}.f * %s{b'}.f)")
                | Cmm.FDiv(a, b) ->
                    let a', _ = find_id a
                    let b', _ = find_id b
                    cont (indentation, $"min_caml_make_float(%s{a'}.f / %s{b'}.f)")
                | Cmm.MemoryGet(mem, ix) ->
                    let mem', _ = find_id mem
                    let ix', _ = find_id ix
                    cont (indentation, $"%s{mem'}.m.v[%s{ix'}.i]")
                | Cmm.MemoryPut(mem, ix, v) ->
                    let mem', _ = find_id mem
                    let ix', _ = find_id ix
                    let v', _ = find_id v
                    text <- text + $"%s{indentation}%s{mem'}.m.v[%s{ix'}.i] = %s{v'};\n"
                    cont (indentation, "min_caml_make_unit()")
                | Cmm.ApplyDirect(Id.L l, vs) ->
                    let vs' = vs |> List.map find_id |> List.map fst
                    let l', _ = find_label l

                    match vs' with
                    | [] -> failwith "can't compile, functions with arity 0 not supported"
                    | x :: xs ->
                        let mutable arg_list_str = x

                        for x_next in xs do
                            arg_list_str <- $"%s{arg_list_str}, %s{x_next}"

                        cont (indentation, $"%s{l'}(%s{arg_list_str})")
                | Cmm.ApplyClosure(f_id, vs) ->
                    let f_id', _ = find_id f_id
                    let vs' = vs |> List.map find_id |> List.map fst

                    match vs' with
                    | [] -> failwith "can't compile, functions with arity 0 not supported"
                    | x :: xs ->
                        let mutable fn_type_str = "v_t (*)(v_t"

                        for _ in 1 .. vs.Length do
                            fn_type_str <- $"{fn_type_str}, v_t"

                        fn_type_str <- $"{fn_type_str})"
                        let mutable arg_list_str = $"%s{f_id'}, %s{x}"

                        for x_next in xs do
                            arg_list_str <- $"%s{arg_list_str}, %s{x_next}"

                        cont (indentation, $"((%s{fn_type_str})%s{f_id'}.m.v[0].f_ptr)(%s{arg_list_str})")

        let print_fn_general fdecl (fn: Cmm.fn_t) =
            let find_id id =
                match ids.TryFind id with
                | Some id -> id
                | _ -> failwith $"could not find a replacement for id '%s{id}'"

            let Id.L l, _ = fn.name

            let l' =
                match ls.TryFind l with
                | Some l' -> l'
                | _ -> l

            let mutable env = M.Empty()
            let vs, ts = fn.args |> List.unzip
            let vs' = vs |> List.map find_id

            match (vs, vs', ts) |||> List.zip3 with
            | [] -> failwith "can't compile, functions with arity 0 not supported"
            | (x_orig, x, t) :: xs ->
                env <- env.Add x_orig (x, t)
                let mutable arg_list_str = $"v_t /*%s{x_orig}*/ {x}"

                for x_orig_next, x_next, t_next in xs do
                    env <- env.Add x_orig_next (x_next, t_next)
                    arg_list_str <- $"%s{arg_list_str}, v_t /*%s{x_orig_next}*/ %s{x_next}"

                if fdecl then
                    text <- text + $"v_t /*%s{l}*/ %s{l'}(%s{arg_list_str});\n"
                else
                    text <- text + $"v_t /*%s{l}*/ %s{l'}(%s{arg_list_str}) {{\n"
                    let cont = (fun (indent, s) -> text <- text + $"%s{indent}return %s{s};\n")
                    ("    ", fn.body) ||> print_block env cont
                    text <- text + "}\n"

        let print_fn = print_fn_general false
        let print_fn_forward_decl = print_fn_general true

        for fn in p.top_level_functions do
            fn |> print_fn_forward_decl

        for fn in p.top_level_functions do
            fn |> print_fn

        text <- text + "static v_t min_caml_entry_point(void) {\n"
        let cont = (fun (indent, s) -> text <- text + $"%s{indent}return %s{s};\n")
        ("    ", p.entry) ||> print_block (M.Empty()) cont
        text <- text + "}\n"

        text <- text + std_epilogue

        text

