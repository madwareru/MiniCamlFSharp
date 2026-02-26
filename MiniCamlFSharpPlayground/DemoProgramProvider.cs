namespace MiniCamlFSharpPlayground;

public static class DemoProgramProvider
{
    private static readonly Dictionary<string, (string, string)> DemoPrograms = new()
    {
        ["1"] = (
            "Демо 1. Целое число", 
            "123"
        ),
        ["2"] = (
            "Демо 2. Кортежи, значения с плавающей точкой, Unit, идентификаторы, связывание имён", 
            @"(let это-идентификатор,-и-он-очень-горд-собою! = 123 in 
  (, это-идентификатор,-и-он-очень-горд-собою! 123.456 #f #t ()))"
        ),
        ["3"] = (
            "Демо 3. Объявление имени с аннотированием типом", 
            @"(let x : i = 123 in x)"
        ),
        ["4"] = (
            "Демо 4. Деструктуризация кортежа", 
            @"(let (, x y) = (, 123 234) in (+ x y))"
        ),
        ["5"] = (
            "Демо 5. Деструктуризация кортежа с аннотированием, частичное аннотирование", 
            @"(let (, x y) : (, i _) = (, 123 234) in (+ x y))"
        ),
        ["6"] = (
            "Демо 6. Комментарии", 
            "(;таким-образом-можно-прокомментировать-любое-выражение 123)"
        ),
        ["7"] = (
            "Демо 7. Ветвления", 
            "(if (>= 5 3) then 42 else 13)"
        ),
        ["8"] = (
            "Демо 8. Простая функция над числами с плавающей точкой", 
            @"(let-rec (add x y) = 
  (+. x y) in 
  (add 12.5 12.5))"
        ),
        ["9"] = (
            "Демо 9. Простая функция над целыми числами с аннотацией", 
            @"(let-rec (add x y) : (i i) -> i  = 
  (+ x y) in 
  (add 12 13))"
        ),
        ["10"] = (
            "Демо 10. Fibonacci", 
            @"(let-rec (fib x) =
  (if (<= x 1)
    then 1
    else (+ (fib (- x 1)) (fib (- x 2)))) in (fib 6))"
        ),
        ["11"] = (
            "Демо 11. Хвосторекурсивный Fibonacci", 
            @"(let-rec (fib x) : (i) -> i =
  (let-rec (fib-tail x-2 x-1 x) : (i i i) -> i =
    (if (<= x 1)
      then x-1
      else
        (let (, x-2 x-1 x) = (, x-1 (+ x-2 x-1) (- x 1))
        in (fib-tail x-2 x-1 x)))
    in (fib-tail 1 1 x))
  in (fib 6))"
        ),
        ["12"] = (
            "Демо 12. Факториал, массивы и операции над ними, последовательности", 
            @"(let acc : ([] f) = (new[] 1.0 1) in
  (let-rec (fact-step x) =
  (if (<= x 1.0)
    then ()
    else
      (let v = (get[] acc 0) in
      (let v' = (*. x v) in
      (;эта-форма-так-же-позволяет-выстроить-последовательность-императивных-команд
        (;первая-императивная-команда (set[] acc 0 <- v'))
        (fact-step (-. x 1.0)))))) in
          (;функция-fact-step-тоже-императивна-так-как-возвращает-Unit
              (fact-step 6.0)
              (get[] acc 0))))"
        ),
        ["13"] = (
            "Демо 13. Преобразования между типами", 
            "(int_of_float (+. (float_of_int 123) 12.5))"
        ),
        ["14"] = (
            "Демо 14. Передача функций как значений в параметры других функций",
            @"(let-rec (do-loop start end iter-action) : (_ _ (fn (i) -> u)) -> _ =
  (if (<= start end)
    then (; (iter-action start) (do-loop (+ start 1) end iter-action))
    else ()) in
(let cell = (;мутабельная-ячейка (new[] 0 1)) in
(let-rec (adder x) = (set[] cell 0 <- (+ x (get[] cell 0))) in
(; 
  (do-loop 0 10 adder) 
  (get[] cell 0)))))"
        ),
        ["15"] = (
            "Демо 15. Лямбда-выражения, каррирование",
            "(let sum_curry = (lam (x) -> (lam (y) -> (+ x y))) in ((sum_curry 5) 8))"
        ),
        ["16"] = (
            "Демо 16. Передача лямбда-выражений в параметры функций",
            @"(let-rec (do-loop start end iter-action) : (_ _ (fn (i) -> u)) -> _ =
  (if (<= start end)
    then (; (iter-action start) (do-loop (+ start 1) end iter-action))
    else ()) in
(let cell = (;мутабельная-ячейка (new[] 0 1)) in
(; 
  (do-loop 0 10 (lam (x) -> (set[] cell 0 <- (+ x (get[] cell 0))))) 
  (get[] cell 0))))"
        )
    };

    public static IEnumerable<(string, string)> DemoKeysAndCaptions
    {
        get
        {
            foreach (var (key, (caption, _)) in DemoPrograms)
                yield return (key, caption);
        }
    }

    public static string GetDemoText(string demoName) =>
        DemoPrograms.TryGetValue(demoName, out var pair) ? pair.Item2 : "";
}