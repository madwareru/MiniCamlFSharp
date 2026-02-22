namespace MiniCamlFSharpPlayground;

public static class DemoProgramProvider
{
    private static readonly Dictionary<string, string> _demoPrograms = new()
    {
        { "1", "(if (>= 5 3) then 42 else 13)" },
        { "2", "(let-rec (add x y) = (+. x y) in (add 12.5 12.5))" },
        { "3", @"(let-rec (fib x) =
  (if (<= x 1)
    then 1
    else (+ (fib (- x 1)) (fib (- x 2)))) in (fib 6))"},
        { "4", @"(let-rec (fib x) : (i) -> i =
  (let-rec (fib-tail x-2 x-1 x) : (i i i) -> i =
    (if (<= x 1)
      then x-1
      else
        (let (, x-2 x-1 x) = (, x-1 (+ x-2 x-1) (- x 1))
        in (fib-tail x-2 x-1 x)))
    in (fib-tail 1 1 x))
  in (fib 6))"}
    };

    public static string GetDemoText(string demoName) =>
        _demoPrograms.TryGetValue(demoName, out var text) ? text : "";
}