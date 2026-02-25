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
using Microsoft.AspNetCore.Components;
using mini_caml_fsharp_core;

namespace MiniCamlFSharpPlayground.Pages;

public partial class KNorm
{
    private string _parsedText = "";
    private string _srcText = "";
    
    private void ChangeSelection(ChangeEventArgs e) => 
        _srcText = DemoProgramProvider.GetDemoText(e.Value?.ToString() ?? "");

    private void ClearInput() =>
        _srcText = "";

    private void KNormalize() =>
        DemoUtils.Do(
            _srcText, 
            out _parsedText,
            input =>
            {
                Id.Id.reset();
                var typingRule = Typing.program_output_typing_rule_t.ProgramShouldNotReturnFunction;
                var ast = Typing.f(typingRule, Parsing.f(SExpr.parse(input)));
                var kNorm = KNormalisation.f(ast);
                return kNorm.ToString();
            });
    
    private void AlphaConvert() =>
        DemoUtils.Do(
            _srcText, 
            out _parsedText,
            input =>
            {
                Id.Id.reset();
                var typingRule = Typing.program_output_typing_rule_t.ProgramShouldNotReturnFunction;
                var ast = Typing.f(typingRule, Parsing.f(SExpr.parse(input)));
                var kNorm = AlphaConv.f(KNormalisation.f(ast));
                return kNorm.ToString();
            });
    
    private void BetaReduce() =>
        DemoUtils.Do(
            _srcText, 
            out _parsedText,
            input =>
            {
                Id.Id.reset();
                var typingRule = Typing.program_output_typing_rule_t.ProgramShouldNotReturnFunction;
                var ast = Typing.f(typingRule, Parsing.f(SExpr.parse(input)));
                var kNorm = BetaReduction.f(AlphaConv.f(KNormalisation.f(ast)));
                return kNorm.ToString();
            });
    
    private void Reassoc() =>
        DemoUtils.Do(
            _srcText, 
            out _parsedText,
            input =>
            {
                Id.Id.reset();
                var typingRule = Typing.program_output_typing_rule_t.ProgramShouldNotReturnFunction;
                var ast = Typing.f(typingRule, Parsing.f(SExpr.parse(input)));
                var kNorm = Assoc.f(BetaReduction.f(AlphaConv.f(KNormalisation.f(ast))));
                return kNorm.ToString();
            });
    
    private void Inline() =>
        DemoUtils.Do(
            _srcText, 
            out _parsedText,
            input =>
            {
                Id.Id.reset();
                var typingRule = Typing.program_output_typing_rule_t.ProgramShouldNotReturnFunction;
                var ast = Typing.f(typingRule, Parsing.f(SExpr.parse(input)));
                var kNorm = Inlining.f(16, Assoc.f(BetaReduction.f(AlphaConv.f(KNormalisation.f(ast)))));
                return kNorm.ToString();
            });
    
    private void ConstFold() =>
        DemoUtils.Do(
            _srcText, 
            out _parsedText,
            input =>
            {
                Id.Id.reset();
                var typingRule = Typing.program_output_typing_rule_t.ProgramShouldNotReturnFunction;
                var ast = Typing.f(typingRule, Parsing.f(SExpr.parse(input)));
                var kNorm = AlphaConv.f(KNormalisation.f(ast));
                kNorm = ConstFolding.f(Inlining.f(16, Assoc.f(BetaReduction.f(kNorm))));
                return kNorm.ToString();
            });
    
    private void DeadCodeEliminate() =>
        DemoUtils.Do(
            _srcText, 
            out _parsedText,
            input =>
            {
                Id.Id.reset();
                var typingRule = Typing.program_output_typing_rule_t.ProgramShouldNotReturnFunction;
                var ast = Typing.f(typingRule, Parsing.f(SExpr.parse(input)));
                var kNorm = AlphaConv.f(KNormalisation.f(ast));
                kNorm = Elimination.f(ConstFolding.f(Inlining.f(16, Assoc.f(BetaReduction.f(kNorm)))));
                return kNorm.ToString();
            });
    
    private void Optimize() =>
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
                return kNorm.ToString();
            });
}