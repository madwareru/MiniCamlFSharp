module mini_caml_fsharp.Program

open Microsoft.FSharp.Core
open mini_caml_fsharp.GenShared
open mini_caml_fsharp.GenC
open mini_caml_fsharp.GenCSharp
open mini_caml_fsharp.Typing

open System.IO

type Target =
    | C
    | CSharp

[<EntryPoint>]
let main args =
    let mutable target = C
    let mutable file_name = ""
    let mutable build_directory = Path.GetFullPath("mini_caml_fsharp_build")
    let pre_gen_settings = { GenShared.inlining_threshold = 16
                             GenShared.optimization_loop_limit = 100
                             GenShared.typing_rule = Typing.ProgramShouldReturnUnit }
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
        let cmm_program = GenShared.pre_gen source pre_gen_settings
            
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
