module mini_caml_fsharp.GenCSharp

open mini_caml_fsharp.Id
open mini_caml_fsharp.Type
open mini_caml_fsharp.M
open mini_caml_fsharp.Cmm
open mini_caml_fsharp.GenShared

module GenCSharp =
    let private csproj_template =
        @"<Project Sdk=""Microsoft.NET.Sdk"">

  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
    <RootNamespace>##NAME_SPACE</RootNamespace>
  </PropertyGroup>

</Project>
"

    let private header_template =
        @"using System;

namespace ##NAME_SPACE;

"

    let private std_prelude =
        @"public static partial class TopLevel {
    public enum u_t : long {
        UNIT = 0
    }

    public readonly struct mem_t {
        public readonly v_t[] v;
        public readonly long length;
        public mem_t(v_t[] v, long length) {
            this.v = v;
            this.length = length;
        }
    }

    public abstract class v_t {
        public sealed class UnitValue : v_t {
            public readonly u_t v;
            public UnitValue() => this.v = u_t.UNIT;
        }
        public sealed class IntValue : v_t {
            public readonly long v;
            public IntValue(long v) => this.v = v;
        }
        public sealed class FloatValue : v_t {
            public readonly double v;
            public FloatValue(double v) => this.v = v;
        }
        public sealed class MemoryValue : v_t {
            public readonly mem_t v;
            public MemoryValue(mem_t v) => this.v = v;
        }
        public sealed class FPtrValue : v_t {
            public readonly Delegate v;
            public FPtrValue(Delegate v) => this.v = v;
        }
        public u_t u => ((UnitValue)this).v;
        public long i => ((IntValue)this).v;
        public double f => ((FloatValue)this).v;
        public mem_t m => ((MemoryValue)this).v;
        public Delegate f_ptr => ((FPtrValue)this).v;
    }
    
    static v_t min_caml_make_unit() => 
        new v_t.UnitValue();
        
    static v_t min_caml_make_int(long i) =>
        new v_t.IntValue(i);
        
    static v_t min_caml_make_float(double f) =>
        new v_t.FloatValue(f);
        
    static v_t min_caml_make_f_ptr(Delegate f_ptr) =>
        new v_t.FPtrValue(f_ptr);
        
    static v_t min_caml_alloc_vector(v_t count) {
        if (count.i <= 0) {
            throw new ArgumentException(""can't allocate vector with size <= 0"");
        }
        v_t v = min_caml_make_unit();
        mem_t mem = new(new v_t[count.i], count.i);
        v_t res = new v_t.MemoryValue(mem);
        for(long i = 0; i < count.i; i++) {
            res.m.v[i] = v;
        }
        return res;
    }
    
    static v_t min_caml_clone(v_t v) {
        if (v is not v_t.MemoryValue) 
            return v;
        v_t count = min_caml_make_int(v.m.length);
        v_t res = min_caml_alloc_vector(count);
        for(long i = 0; i < count.i; i++) {
            res.m.v[i] = min_caml_clone(v.m.v[i]);
        }
        return res;
    }

    static v_t min_caml_create_array(v_t count, v_t v) {
        v_t res = min_caml_alloc_vector(count);
        for(long i = 0; i < count.i; i++) {
            res.m.v[i] = min_caml_clone(v);
        }
        return res;
    }
    
    static v_t min_caml_create_float_array(v_t count, v_t v) {
        v_t res = min_caml_alloc_vector(count);
        for(long i = 0; i < count.i; i++) {
            res.m.v[i] = min_caml_make_float(v.f);
        }
        return res;
    }
    
    static v_t min_caml_less_eq(v_t lhs, v_t rhs) {
        switch(lhs) {
            case v_t.UnitValue:
                return min_caml_make_int(1);
            case v_t.IntValue:
                return min_caml_make_int(lhs.i <= rhs.i ? 1 : 0);
            case v_t.FloatValue:
                return min_caml_make_int(lhs.f <= rhs.f ? 1 : 0);
            case v_t.FPtrValue:
                return min_caml_make_int(lhs.f_ptr == rhs.f_ptr ? 1 : 0);
            default:
                for(long i = 0; i < lhs.m.length; i++) {
                    v_t r = min_caml_less_eq(lhs.m.v[i], rhs.m.v[i]);
                    if (r.i == 0)
                        return r;
                }
                return min_caml_make_int(1);
        }
    }
    static v_t min_caml_eq(v_t lhs, v_t rhs) {
        switch(lhs) {
            case v_t.UnitValue:
                return min_caml_make_int(1);
            case v_t.IntValue:
                return min_caml_make_int(lhs.i == rhs.i ? 1 : 0);
            case v_t.FloatValue:
                return min_caml_make_int(Math.Abs(lhs.f - rhs.f) <= double.Epsilon ? 1 : 0);
            case v_t.FPtrValue:
                return min_caml_make_int(lhs.f_ptr == rhs.f_ptr ? 1 : 0);
            default:
                for(long i = 0; i < lhs.m.length; i++) {
                    v_t r = min_caml_eq(lhs.m.v[i], rhs.m.v[i]);
                    if (r.i == 0)
                        return r;
                }
                return min_caml_make_int(1);
        }
    }
    static v_t min_caml_float_of_int(v_t v) {
        v_t res = min_caml_make_float(v.i);
        return res;
    }
    static v_t min_caml_int_of_float(v_t v)  {
        v_t res = min_caml_make_int((long) v.f);
        return res;
    }
    static v_t min_caml_print_int(v_t v) {
        Console.Write(v.i.ToString());
        return min_caml_make_unit();
    }
    static v_t min_caml_print_int_ln(v_t v) {
        Console.WriteLine(v.i.ToString());
        return min_caml_make_unit();
    }
    static v_t min_caml_print_float(v_t v) {
        Console.Write(v.f.ToString());
        return min_caml_make_unit();
    }
    static v_t min_caml_print_float_ln(v_t v) {
        Console.WriteLine(v.f.ToString());
        return min_caml_make_unit();
    }
    static v_t min_caml_print_bool(v_t v) {
        if (v.i != 0) {
            Console.Write(""true"");
        } else {
            Console.Write(""false"");
        }
        return min_caml_make_unit();
    }
    static v_t min_caml_print_bool_ln(v_t v) {
        if (v.i != 0) {
            Console.WriteLine(""true"");
        } else {
            Console.WriteLine(""false"");
        }
        return min_caml_make_unit();
    }
    static v_t min_caml_print_ln(v_t v_unused) {
        Console.WriteLine();
        return min_caml_make_unit();
    }
    static v_t min_caml_print_tab(v_t v_unused) {
        Console.Write(""\t"");
        return min_caml_make_unit();
    }
    static v_t min_caml_put_char(v_t v) {
        Console.Write(((char)(v.i & 0xFF)).ToString());
        return min_caml_make_unit();
    }
}
"

    let private std_epilogue =
        @"public static class Program {
    public static void Main() {
        TopLevel.min_caml_entry_point();
    }
}
"

    let f name_space (p: Cmm.program_t) =
        let mutable text = header_template.Replace("##NAME_SPACE", name_space)
        text <- text + std_prelude

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
                        | Type.FloatType -> $"Math.Abs(%s{a'}.f - %s{b'}.f) <= double.Epsilon"
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
                        let mutable fn_type_str = "Func<v_t"

                        for _ in 1 .. vs.Length do
                            fn_type_str <- $"{fn_type_str}, v_t"

                        fn_type_str <- $"{fn_type_str}, v_t>"
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
                        | Type.FloatType -> $"Math.Abs(%s{a'}.f - %s{b'}.f) <= double.Epsilon"
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
                        let mutable fn_type_str = "Func<v_t"

                        for _ in 1 .. vs.Length do
                            fn_type_str <- $"{fn_type_str}, v_t"

                        fn_type_str <- $"{fn_type_str}, v_t>"
                        let mutable arg_list_str = $"%s{f_id'}, %s{x}"

                        for x_next in xs do
                            arg_list_str <- $"%s{arg_list_str}, %s{x_next}"

                        cont (indentation, $"((%s{fn_type_str})%s{f_id'}.m.v[0].f_ptr)(%s{arg_list_str})")

        let print_fn (fn: Cmm.fn_t) =
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
                
                text <- text + $"    static v_t /*%s{l}*/ %s{l'}(%s{arg_list_str}) {{\n"
                let cont = (fun (indent, s) -> text <- text + $"%s{indent}return %s{s};\n")
                ("        ", fn.body) ||> print_block env cont
                text <- text + "    }\n"

        text <- text + "public static partial class TopLevel {"
        
        for fn in p.top_level_functions do
            fn |> print_fn

        text <- text + "    public static v_t min_caml_entry_point() {\n"
        let cont = (fun (indent, s) -> text <- text + $"%s{indent}return %s{s};\n")
        ("        ", p.entry) ||> print_block (M.Empty()) cont
        text <- text + "    }\n"
        
        text <- text + "}\n"

        text <- text + std_epilogue

        csproj_template.Replace("##NAME_SPACE", name_space), text

