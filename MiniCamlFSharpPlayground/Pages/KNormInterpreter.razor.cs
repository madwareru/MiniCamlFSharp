using SExpr = mini_caml_fsharp_core.SExpr.SExpr;
using Parsing = mini_caml_fsharp_core.Parsing.Parsing;
using Typing = mini_caml_fsharp_core.Typing.Typing;
using KNormalisation = mini_caml_fsharp_core.KNormalisation.KNormalisation;
using AlphaConv = mini_caml_fsharp_core.AlphaConv.AlphaConv;
using BetaReduction = mini_caml_fsharp_core.BetaReduction.BetaReduction;
using Assoc = mini_caml_fsharp_core.Assoc.Assoc;
using Inlining = mini_caml_fsharp_core.Inlining.Inlining;
using ConstFolding = mini_caml_fsharp_core.ConstFolding.ConstFolding;
using Elimination = mini_caml_fsharp_core.Elimination.Elimination;
using Interpreter = mini_caml_fsharp_core.KNormInterpreter.KNormInterpreter;
using Microsoft.AspNetCore.Components;
using mini_caml_fsharp_core;

namespace MiniCamlFSharpPlayground.Pages;

public partial class KNormInterpreter
{
    private string _parsedText = "";
    private string _srcText = "";
    
    private void ChangeSelection(ChangeEventArgs e) => 
        _srcText = DemoProgramProvider.GetDemoText(e.Value?.ToString() ?? "");

    private void ClearInput() =>
        _srcText = "";

    private void Run() =>
        DemoUtils.Do(
            _srcText, 
            out _parsedText,
            input =>
            {
                Id.Id.reset();
                var typingRule = Typing.program_output_typing_rule_t.ProgramShouldNotReturnFunction;
                var ast = Typing.f(typingRule, Parsing.f(SExpr.parse(input)));
                var kNorm = AlphaConv.f(KNormalisation.f(ast));
                for (var i = 0; i < 100; i++)
                {
                    var optimized = Elimination.f(ConstFolding.f(Inlining.f(16, Assoc.f(BetaReduction.f(kNorm)))));
                    if (optimized.Equals(kNorm))
                        break;
                    kNorm = optimized;
                }
                var result = Interpreter.f(kNorm);
                return result.ToString();
            });
}