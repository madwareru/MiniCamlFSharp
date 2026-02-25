using mini_caml_fsharp_core;
using SExpr = mini_caml_fsharp_core.SExpr.SExpr;
using System.Text;
using Microsoft.AspNetCore.Components;

namespace MiniCamlFSharpPlayground.Pages;

public partial class SExprParser
{
    private string _parsedText = "";
    private string _srcText = "";
    
    private void ChangeSelection(ChangeEventArgs e) => 
        _srcText = DemoProgramProvider.GetDemoText(e.Value?.ToString() ?? "");

    private void ClearInput() =>
        _srcText = "";

    private void TryParse() =>
        DemoUtils.Do(
            _srcText, 
            out _parsedText,
            input => SExpr.parse(input).ToString());

    private void Tokenize() =>
        DemoUtils.Do(
            _srcText, 
            out _parsedText,
            input =>
            {
                var lexer = ParserCombinators.toSeqLexer(SExpr.lex_step);
                var convertedString = Microsoft.FSharp.Collections.SeqModule.ToList(input);
                var tokens = lexer.Invoke(convertedString);
                if (tokens == null) return "";
                
                var sb = new StringBuilder();
                foreach (var token in tokens.Where(it => it != null))
                    sb.AppendLine(token.ToString());

                return sb.ToString();
            });
}