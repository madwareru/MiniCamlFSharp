module mini_caml_fsharp.Program

open Microsoft.FSharp.Core
open mini_caml_fsharp.SExpr
open mini_caml_fsharp.Id
open mini_caml_fsharp.Parsing
open mini_caml_fsharp.Typing
open mini_caml_fsharp.KNormalisation
open mini_caml_fsharp.AlphaConv
open mini_caml_fsharp.BetaReduction
open mini_caml_fsharp.Assoc
open mini_caml_fsharp.Inlining
open mini_caml_fsharp.ConstFolding
open mini_caml_fsharp.Elimination
open mini_caml_fsharp.ClosureRepresentationConv
open mini_caml_fsharp.CmmConv
open mini_caml_fsharp.GenC
open mini_caml_fsharp.GenCSharp

open System.IO

type Target =
    | C
    | CSharp

[<EntryPoint>]
let main args =
    let mutable target = C
    let mutable file_name = ""
    let mutable build_directory = Path.GetFullPath("mini_caml_fsharp_build")
    for arg in args do
        if arg = "/target:CSharp" then
            target <- CSharp
        elif arg.StartsWith "/source-file:" then
            file_name <- Path.GetFullPath(arg.Replace("/source-file:", ""))
        elif arg.StartsWith "/build-dir:" then
            build_directory <- Path.GetFullPath(arg.Replace("/build-dir:", ""))
    if file_name.Length = 0 || not(file_name.EndsWith(".sexpr")) then
        printfn "Please use a proper format! Command line arguments should include /source-file:file-name.sexpr"
        0
    else
        printfn $"compiling the file {file_name} for {target} target"
        
        let source = File.ReadAllText(file_name).Replace("\r\n", "\n")
        Id.reset ()
        let k_form =
            source
            |> SExpr.parse
            |> Parsing.f
            |> Typing.f Typing.ProgramShouldReturnUnit
            |> KNormalisation.f
            |> AlphaConv.f
            
        let limit = 100
        let rec iter n e =
            printfn $"iteration %d{n}."
            match n with
            | 0 -> e
            | _ ->
                let e' = e |> BetaReduction.f
                let e' = e' |> Assoc.f
                let e' = e' |> Inlining.f 16
                let e' = e' |> ConstFolding.f
                let e' = e' |> Elimination.f
                if e = e' then e' else iter (n - 1) e'

        let converted = (limit, k_form) ||> iter
        let cmm_program =
            converted
            |> ClosureRepresentationConv.f
            |> CmmConv.f
            
        let name_space = Path.GetFileNameWithoutExtension(file_name)
        match target with
        | C ->
            let generated_c = cmm_program |> GenC.f
            let c_file_path = Path.Combine(build_directory, $"{name_space}.c")
            printfn "c generation is complete"
            if not(Directory.Exists(build_directory)) then
                Directory.CreateDirectory(build_directory) |> ignore
                
            printfn $"writing generated code to {c_file_path}"
            File.WriteAllText(c_file_path, generated_c)
            
            printfn "Done!"
        | CSharp ->
            printfn $"name_space = {name_space}"
            let generated_proj, generated_cs = cmm_program |> GenCSharp.f name_space
            printfn "csharp generation is complete"
            
            if not(Directory.Exists(build_directory)) then
                Directory.CreateDirectory(build_directory) |> ignore
                
            let csproj_file_path = Path.Combine(build_directory, $"{name_space}.csproj")
            printfn $"writing csproj to {csproj_file_path}"
            File.WriteAllText(csproj_file_path, generated_proj)
            
            let cs_file_path = Path.Combine(build_directory, "Program.cs")
            printfn $"writing generated code to {csproj_file_path}"
            File.WriteAllText(cs_file_path, generated_cs)
            
            printfn "Done!"
        0
