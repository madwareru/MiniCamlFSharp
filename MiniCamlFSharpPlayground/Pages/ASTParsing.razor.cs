using SExpr = mini_caml_fsharp_core.SExpr.SExpr;
using Typing = mini_caml_fsharp_core.Typing.Typing;
using Parsing = mini_caml_fsharp_core.Parsing.Parsing;

using Microsoft.AspNetCore.Components;
using mini_caml_fsharp_core;

namespace MiniCamlFSharpPlayground.Pages;

public partial class ASTParsing
{
    private string _srcText = "";
    private string _parsedText = "";

    private void ChangeSelection(ChangeEventArgs e) => 
        _srcText = DemoProgramProvider.GetDemoText(e.Value?.ToString() ?? "");

    private void ClearInput() => _srcText = "";

    private void TryParse() =>
        DemoUtils.Do(
            _srcText,
            out _parsedText,
            input =>
            {
                Id.Id.reset();
                return Parsing.f(SExpr.parse(input)).ToString();
            });
    
    private void InferTypes() =>
        DemoUtils.Do(
            _srcText,
            out _parsedText,
            input =>
            {
                Id.Id.reset();
                var typingRule = Typing.program_output_typing_rule_t.ProgramShouldNotReturnFunction;
                var ast = Typing.f(typingRule, Parsing.f(SExpr.parse(input)));
                return ast.ToString();
            });
}