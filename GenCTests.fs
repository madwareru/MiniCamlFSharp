module mini_caml_fsharp.GenCTests

open Microsoft.FSharp.Core
open NUnit.Framework
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

open System.IO
open System.Diagnostics


type CommandResult =
    { ExitCode: int
      StandardOutput: string
      StandardError: string }

let executeShellCommand exec args =
    let startInfo = ProcessStartInfo()
    startInfo.FileName <- exec
    startInfo.RedirectStandardError <- true
    startInfo.RedirectStandardOutput <- true
    startInfo.UseShellExecute <- false
    startInfo.CreateNoWindow <- true
    for arg in args do
        startInfo.ArgumentList.Add(arg)
    use p = new Process()
    p.StartInfo <- startInfo
    p.Start() |> ignore
    p.WaitForExit()
    {
        ExitCode = p.ExitCode
        StandardOutput = p.StandardOutput.ReadToEnd()
        StandardError = p.StandardError.ReadToEnd()
    }

[<Test>]
let testGenC () =
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
    
    let tests = [
        "tst1"
        "tst2"
        "tst3"
        "tst4"
        "tst5"
        "tst6"
        "tst7"
        "tst8"
        "tak"
        "loop_idiom"
        "mandelbrot"
        "mandelbrot2"
        "mandelbrot3"
    ]
    
    for testName in tests do
        let c_file_path = Path.Combine("GenCTests", $"{testName}.c")
        let exe_file_path = Path.Combine("GenCTests", $"{testName}.exe")
        let output_expected_path = Path.Combine("GenCTests", $"{testName}_output.txt")
        let source_path = Path.Combine("GenCTests", $"{testName}.sexpr")
        
        printfn $"compiling {source_path}"
        let source = File.ReadAllText(source_path).Replace("\r\n", "\n")
        Id.reset ()
        let k_form =
            source
            |> SExpr.parse
            |> Parsing.f
            |> Typing.f Typing.ProgramShouldReturnUnit
            |> KNormalisation.f
            |> AlphaConv.f
            
        let converted = (limit, k_form) ||> iter
        let res_text =
            converted
            |> ClosureRepresentationConv.f
            |> CmmConv.f
            |> GenC.f
        
        printfn $"c generation is complete, writing to {c_file_path}"    
        File.WriteAllText(c_file_path, res_text)
        
        File.Delete(exe_file_path)
        printfn $"compiling {c_file_path}"    
        let compilation_result = executeShellCommand "cc" [c_file_path; "-O2"; "-o"; exe_file_path]
        printfn $"standard output: {compilation_result.StandardOutput}"
        printfn $"standard error: {compilation_result.StandardError}"
        
        Assert.IsTrue(File.Exists(exe_file_path))
        
        let result = executeShellCommand exe_file_path []
        let output_expected = File.ReadAllText(output_expected_path)
        Assert.AreEqual(output_expected, result.StandardOutput)