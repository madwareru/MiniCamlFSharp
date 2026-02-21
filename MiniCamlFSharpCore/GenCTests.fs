module mini_caml_fsharp_core.GenCTests

open Microsoft.FSharp.Core
open NUnit.Framework
open mini_caml_fsharp_core.GenShared
open mini_caml_fsharp_core.GenC
open mini_caml_fsharp_core.GenCSharp
open mini_caml_fsharp_core.Typing

open System.IO
open System.Diagnostics
open System.Runtime.InteropServices

type OutputType =
    | Txt
    | Ppm

type CommandResult =
    { ExitCode: int
      StandardOutput: string
      StandardError: string }

let executeShellCommand exec args =
    let start_info = ProcessStartInfo()
    start_info.FileName <- exec
    start_info.RedirectStandardError <- true
    start_info.RedirectStandardOutput <- true
    start_info.UseShellExecute <- false
    start_info.CreateNoWindow <- true
    for arg in args do
        start_info.ArgumentList.Add(arg)

    use p = new Process()
    p.StartInfo <- start_info

    let mutable output_builder = System.Text.StringBuilder()
    let mutable error_builder = System.Text.StringBuilder()

    p.OutputDataReceived.Add(fun args ->
        output_builder <-
            match args.Data with
            | text when text <> null -> output_builder.AppendLine(text)
            | _ -> output_builder)

    p.ErrorDataReceived.Add(fun args ->
        error_builder <-
            match args.Data with
            | text when text <> null -> error_builder.AppendLine(text)
            | _ -> error_builder)

    p.Start() |> ignore

    p.BeginOutputReadLine()
    p.BeginErrorReadLine()

    p.WaitForExit()

    {
        ExitCode = p.ExitCode
        StandardOutput = output_builder.ToString()
        StandardError = error_builder.ToString()
    }

[<Test>]
let testGenC () =
    let tests = [
        "tst1", Txt
        "tst2", Txt
        "tst3", Txt
        "tst4", Txt
        "tst5", Txt
        "tst6", Txt
        "tst7", Txt
        "tst8", Txt
        "tak", Txt
        "loop_idiom", Txt
        "mandelbrot", Txt
        "mandelbrot2", Txt
        "mandelbrot3", Txt
        "mandelbrot_colored", Ppm
    ]

    for test_name, output_type in tests do
        let c_file_path = Path.GetFullPath(Path.Combine("GenCTests", $"{test_name}.c"))
        let exe_file_path = Path.GetFullPath(Path.Combine("GenCTests", $"{test_name}.exe"))
        let output_expected_path =
            match output_type with
            | Txt -> Path.GetFullPath(Path.Combine("GenCTests", $"{test_name}_output.txt"))
            | Ppm -> Path.GetFullPath(Path.Combine("GenCTests", $"{test_name}_output.ppm"))
        let source_path = Path.GetFullPath(Path.Combine("GenCTests", $"{test_name}.sexpr"))

        printfn $"compiling {source_path}"
        let source = File.ReadAllText(source_path).Replace("\r\n", "\n")
        let res_text =
            GenShared.pre_gen source { inlining_threshold = 16
                                       optimization_loop_limit = 100
                                       typing_rule = Typing.ProgramShouldReturnUnit}
            |> GenC.f

        printfn $"c generation is complete, writing to {c_file_path}"
        File.WriteAllText(c_file_path, res_text)

        let is_windows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)

        File.Delete(exe_file_path)
        printfn $"compiling {c_file_path}"
        let compilation_result =
            if not is_windows then
                executeShellCommand "cc" [c_file_path; "-O3"; "-o"; exe_file_path]
            else
                executeShellCommand "cl" ["/O2"; $"/Fe{exe_file_path}"; c_file_path]
        printfn $"standard output: {compilation_result.StandardOutput}"
        printfn $"standard error: {compilation_result.StandardError}"

        Assert.AreEqual(0, compilation_result.ExitCode)

        let result = executeShellCommand exe_file_path []
        let output_expected = File.ReadAllText(output_expected_path)
        Assert.AreEqual(0, result.ExitCode)
        Assert.AreEqual(output_expected, result.StandardOutput)

[<Test>]
let testGenCSharp () =
    let tests = [
        "tst1", Txt
        "tst2", Txt
        "tst3", Txt
        "tst4", Txt
        "tst5", Txt
        "tst6", Txt
        "tst7", Txt
        "tst8", Txt
        "tak", Txt
        "loop_idiom", Txt
        "mandelbrot", Txt
        "mandelbrot2", Txt
        "mandelbrot3", Txt
        "mandelbrot_colored", Ppm
    ]

    for test_name, output_type in tests do
        let cs_proj_directory = Path.GetFullPath(Path.Combine("GenCTests", $"{test_name}"))
        if not(Directory.Exists(cs_proj_directory)) then
            Directory.CreateDirectory(cs_proj_directory) |> ignore
        let cs_proj_file_path = Path.Combine(cs_proj_directory, $"{test_name}.csproj")
        let cs_file_path = Path.Combine(cs_proj_directory, "Program.cs")
        
        let output_expected_path =
            match output_type with
            | Txt -> Path.GetFullPath(Path.Combine("GenCTests", $"{test_name}_output.txt"))
            | Ppm -> Path.GetFullPath(Path.Combine("GenCTests", $"{test_name}_output.ppm"))
        let source_path = Path.GetFullPath(Path.Combine("GenCTests", $"{test_name}.sexpr"))

        printfn $"compiling {source_path}"
        let source = File.ReadAllText(source_path).Replace("\r\n", "\n")
        
        let project_text, cs_text =
            GenShared.pre_gen source { inlining_threshold = 16
                                       optimization_loop_limit = 100
                                       typing_rule = Typing.ProgramShouldReturnUnit }
            |> GenCSharp.f test_name

        printfn $"csharp generation is complete"
        printfn $"writing csproj content to {cs_proj_file_path}"
        File.WriteAllText(cs_proj_file_path, project_text)
        
        printfn $"writing Program.cs content to {cs_file_path}"
        File.WriteAllText(cs_file_path, cs_text)

        printfn $"compiling {cs_proj_directory}"
        let compilation_result = executeShellCommand "dotnet" ["build"; "-c"; "Release"; cs_proj_directory]
            
        printfn $"standard output: {compilation_result.StandardOutput}"
        printfn $"standard error: {compilation_result.StandardError}"

        Assert.AreEqual(0, compilation_result.ExitCode)

        let result = executeShellCommand "dotnet" ["run"; "-c"; "Release"; "--project"; cs_proj_directory]
        let output_expected = File.ReadAllText(output_expected_path)
        Assert.AreEqual(0, result.ExitCode)
        Assert.AreEqual(output_expected, result.StandardOutput)