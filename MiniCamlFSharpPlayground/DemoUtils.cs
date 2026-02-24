namespace MiniCamlFSharpPlayground;

public static class DemoUtils
{
    public static void Do(in string input, out string resultTarget, Func<string, string> func)
    {
        try
        {
            var res = func.Invoke(input);
            resultTarget = res;
        }
        catch(Exception exception)
        {
            resultTarget = $"Возникла ошибка: {exception.Message}";
        }
    }
}