module mini_caml_fsharp.ParsingTests

open Microsoft.FSharp.Core
open NUnit.Framework
open mini_caml_fsharp.SExpr
open mini_caml_fsharp.Id
open mini_caml_fsharp.Type
open mini_caml_fsharp.Syntax
open mini_caml_fsharp.Parsing

type private test_case = { s_expr: string; expected_syntax: Syntax.t }

let private parsing_tests: test_case list = [
    // Простые примеры литералов для значений:
    {
        s_expr = "()"
        expected_syntax = Syntax.UnitNode
    }
    {
        s_expr = "#t"
        expected_syntax = Syntax.BoolNode true
    }
    {
        s_expr = "#f"
        expected_syntax = Syntax.BoolNode false
    }
    {
        s_expr = "123"
        expected_syntax = Syntax.IntNode 123
    }
    {
        s_expr = "123.456"
        expected_syntax = Syntax.FloatNode 123.456
    }
    {
        s_expr = "Want-some-kebab?"
        expected_syntax = Syntax.VarNode "Want-some-kebab?"
    }
    {
        s_expr = "(, 1 foo #f 123.456)"
        expected_syntax = Syntax.TupleNode([
            Syntax.IntNode 1
            Syntax.VarNode "foo"
            Syntax.BoolNode false
            Syntax.FloatNode 123.456
        ])
    }
    {
        s_expr = "(, (, 1 foo #f) 123.456)"
        expected_syntax = Syntax.TupleNode([
            Syntax.TupleNode([
                Syntax.IntNode 1
                Syntax.VarNode "foo"
                Syntax.BoolNode false
            ])
            Syntax.FloatNode 123.456
        ])
    }

    // Базовые операции:
    {
        s_expr = "(not #t)"
        expected_syntax = Syntax.BoolNode false
    }
    {
        s_expr = "(not #f)"
        expected_syntax = Syntax.BoolNode true
    }
    {
        s_expr = "(clone! (, 1 2 3))"
        expected_syntax = Syntax.CloneNode(Syntax.TupleNode [Syntax.IntNode 1; Syntax.IntNode 2; Syntax.IntNode 3])
    }
    {
        s_expr = "(- 1)"
        expected_syntax = Syntax.IntNode -1
    }
    {
        s_expr = "(-. 1.5)"
        expected_syntax = Syntax.FloatNode -1.5
    }
    {
        s_expr = "(not x)"
        expected_syntax = Syntax.NotNode(Syntax.VarNode "x")
    }
    {
        s_expr = "(- x)"
        expected_syntax = Syntax.NegNode(Syntax.VarNode "x")
    }
    {
        s_expr = "(-. x)"
        expected_syntax = Syntax.FNegNode(Syntax.VarNode "x")
    }
    {
        s_expr = "(+ x y)"
        expected_syntax = Syntax.AddNode(Syntax.VarNode "x", Syntax.VarNode "y")
    }
    {
        // Хотя сама операция и бинарная, можно схлопывать длинные цепочки
        // во вложенные выражения, и тем самым избавить пользователя от
        // необходимости писать огромное количество скобочек
        s_expr = "(+ x y z w)"
        expected_syntax = Syntax.AddNode(
            Syntax.AddNode(
                Syntax.AddNode(Syntax.VarNode "x", Syntax.VarNode "y"),
                Syntax.VarNode "z"
            ),
            Syntax.VarNode "w"
        )
    }
    {
        s_expr = "(+. x y)"
        expected_syntax = Syntax.FAddNode(Syntax.VarNode "x", Syntax.VarNode "y")
    }
    {
        // Должно работать так же как и у +
        s_expr = "(+. x y z w)"
        expected_syntax = Syntax.FAddNode(
            Syntax.FAddNode(
                Syntax.FAddNode(Syntax.VarNode "x", Syntax.VarNode "y"),
                Syntax.VarNode "z"
            ),
            Syntax.VarNode "w"
        )
    }
    {
        // И само собой это должно работать для вычитаний (следим за тем, чтобы
        // была правильная ассоциативность, иначе (- 2 2 2) может разобраться в
        // 2 - (2 - 2), что приведёт к неверному результату)
        s_expr = "(- x y z w)"
        expected_syntax = Syntax.SubNode(
            Syntax.SubNode(
                Syntax.SubNode(Syntax.VarNode "x", Syntax.VarNode "y"),
                Syntax.VarNode "z"
            ),
            Syntax.VarNode "w"
        )
    }
    {
        s_expr = "(*. x y)"
        expected_syntax = Syntax.FMulNode(Syntax.VarNode "x", Syntax.VarNode "y")
    }
    {
        s_expr = "(*. x y z w)"
        expected_syntax = Syntax.FMulNode(
            Syntax.FMulNode(
                Syntax.FMulNode(Syntax.VarNode "x", Syntax.VarNode "y"),
                Syntax.VarNode "z"
            ),
            Syntax.VarNode "w"
        )
    }
    {
        s_expr = "(/. x y)"
        expected_syntax = Syntax.FDivNode(Syntax.VarNode "x", Syntax.VarNode "y")
    }
    {
        s_expr = "(/. x y z w)"
        expected_syntax = Syntax.FDivNode(
            Syntax.FDivNode(
                Syntax.FDivNode(Syntax.VarNode "x", Syntax.VarNode "y"),
                Syntax.VarNode "z"
            ),
            Syntax.VarNode "w"
        )
    }

    // Операции сравнений:
    {
        s_expr = "(<= a b)"
        expected_syntax = Syntax.LENode(Syntax.VarNode "a", Syntax.VarNode "b")
    }
    {
        s_expr = "(> a b)"
        expected_syntax = Syntax.NotNode(Syntax.LENode(Syntax.VarNode "a", Syntax.VarNode "b"))
    }
    {
        s_expr = "(>= a b)"
        expected_syntax = Syntax.LENode(Syntax.VarNode "b", Syntax.VarNode "a")
    }
    {
        s_expr = "(< a b)"
        expected_syntax = Syntax.NotNode(Syntax.LENode(Syntax.VarNode "b", Syntax.VarNode "a"))
    }
    {
        s_expr = "(= a b)"
        expected_syntax = Syntax.EqNode(Syntax.VarNode "a", Syntax.VarNode "b")
    }
    {
        s_expr = "(<> a b)"
        expected_syntax = Syntax.NotNode(Syntax.EqNode(Syntax.VarNode "a", Syntax.VarNode "b"))
    }

    // Условные выражения:
    {
        s_expr = "(if expr then 1 else 2)"
        expected_syntax = Syntax.IfNode(
            Syntax.VarNode "expr",
            Syntax.IntNode 1,
            Syntax.IntNode 2
        )
    }
    {
        // Это должно распарситься, но при этом упасть на проверке типов позднее
        s_expr = "(if expr then 1 else 2.5)"
        expected_syntax = Syntax.IfNode(
            Syntax.VarNode "expr",
            Syntax.IntNode 1,
            Syntax.FloatNode 2.5
        )
    }
    {
        // and-then это просто сахар для условного выражения
        // цепочки из сравнений для полее чем двух операндов
        // можно было бы организовать, но в данной реализации
        // их решено не делать
        s_expr = "(and-then (<= 2 5) (= (+ 2 2) 6))"
        expected_syntax = Syntax.IfNode(
            Syntax.NotNode(Syntax.LENode(Syntax.IntNode 2, Syntax.IntNode 5)),
            Syntax.BoolNode false,
            Syntax.EqNode(
                Syntax.AddNode(Syntax.IntNode 2, Syntax.IntNode 2),
                Syntax.IntNode 6
            )
        )
    }
    {
        // or-else это просто сахар для условного выражения
        // цепочки из сравнений для полее чем двух операндов
        // можно было бы организовать, но в данной реализации
        // их решено не делать
        s_expr = "(or-else (<= 2 5) (= (+ 2 2) 6))"
        expected_syntax = Syntax.IfNode(
            Syntax.LENode(Syntax.IntNode 2, Syntax.IntNode 5),
            Syntax.BoolNode true,
            Syntax.EqNode(
                Syntax.AddNode(Syntax.IntNode 2, Syntax.IntNode 2),
                Syntax.IntNode 6
            )
        )
    }

    {
        // с вновь связанными переменными мы ассоциируем заглушку для типа,
        // с целью вывести точный тип в дальнейшем
        s_expr = "(let x = 2 in (+ x 2))"
        expected_syntax = Syntax.LetNode(
            ("x", Type.gen_empty ()),
            Syntax.IntNode 2,
            Syntax.AddNode(Syntax.VarNode "x", Syntax.IntNode 2)
        )
    }
    {
        // можно явно проаннотировать тип
        s_expr = "(let x : i = 2 in (+ x 2))"
        expected_syntax = Syntax.LetNode(
            ("x", Type.IntType),
            Syntax.IntNode 2,
            Syntax.AddNode(Syntax.VarNode "x", Syntax.IntNode 2)
        )
    }
    {
        // можно явно проаннотировать тип
        s_expr = "(let x : f = 2.0 in (+. x 2.0))"
        expected_syntax = Syntax.LetNode(
            ("x", Type.FloatType),
            Syntax.FloatNode 2.0,
            Syntax.FAddNode(Syntax.VarNode "x", Syntax.FloatNode 2.0)
        )
    }
    {
        // можно явно проаннотировать тип
        s_expr = "(let x : b = #t in x)"
        expected_syntax = Syntax.LetNode(
            ("x", Type.BoolType),
            Syntax.BoolNode true,
            Syntax.VarNode "x"
        )
    }
    {
        // Можно деструктурировать кортеж, при так же поддерживается
        // аннотирование типами
        s_expr = "(let (, x y) : (, b i) = (, #t 123) in x)"
        expected_syntax = Syntax.LetTuple(
            ["x", Type.BoolType; "y", Type.IntType],
            Syntax.TupleNode [Syntax.BoolNode true; Syntax.IntNode 123],
            Syntax.VarNode "x"
        )
    }
    {
        s_expr = "(let-rec (fac x) = (if (<= x 1.0) then 1.0 else (*. x (fac (-. x 1.0)))) in (fac 6.0))"
        expected_syntax = Syntax.LetRecNode(
            {
                name = "fac", Type.FunType([Type.gen_empty ()], Type.gen_empty ())
                args = [ ("x", Type.gen_empty ()) ]
                body = Syntax.IfNode(
                    Syntax.LENode(Syntax.VarNode "x", Syntax.FloatNode 1.0),
                    Syntax.FloatNode 1.0,
                    Syntax.FMulNode(
                        Syntax.VarNode "x",
                        Syntax.ApplyNode(
                            Syntax.VarNode "fac",
                            [Syntax.FSubNode(Syntax.VarNode "x", Syntax.FloatNode 1.0)]
                        )
                    )
                )
            }, Syntax.ApplyNode(Syntax.VarNode "fac", [Syntax.FloatNode 6.0])
        )
    }
    {
        // объявления функций тоже можно аннотировать
        s_expr = "(let-rec (hello-world _) : (u) -> u = (println-hello-world ()) in ())"
        expected_syntax = Syntax.LetRecNode(
            {
                name = "hello-world", Type.FunType([Type.UnitType], Type.UnitType)
                args = [ ("_", Type.UnitType) ]
                body = Syntax.ApplyNode(Syntax.VarNode "println-hello-world", [ Syntax.UnitNode ])
            }, Syntax.UnitNode
        )
    }
    {
        // функция является первоклассным значением и её можно положить
        // в другую переменную, которую можно проаннотировать типом функции
        s_expr = @"
            (let x : (fn (u) -> u) =
                (let-rec (hello-world _) : (u) -> u = (println-hello-world ()) in ()) in
                (x ()))
        "
        expected_syntax = Syntax.LetNode(
            ("x", Type.FunType([Type.UnitType], Type.UnitType)),
            Syntax.LetRecNode(
                {
                    name = "hello-world", Type.FunType([Type.UnitType], Type.UnitType)
                    args = [ ("_", Type.UnitType) ]
                    body = Syntax.ApplyNode(Syntax.VarNode "println-hello-world", [ Syntax.UnitNode ])
                }, Syntax.UnitNode
            ),
            Syntax.ApplyNode(Syntax.VarNode "x", [ Syntax.UnitNode ])
        )
    }
    {
        // Оператор для выстраивания в последовательность цепочки из "забываний" вычислений,
        // для написания кода в императивном стиле, данный оператор требует, чтобы каждое выражение
        // было типа Unit
        s_expr = @"
            (let arr : ([] i) = (new[] 0 2) in
                (;этот-оператор-так-же-можно-использовать-для-комментариев
                (set[] arr 0 <- 10)
                (set[] arr 1 <- 20)
                (+
                    (get[] arr 0)
                    (get[] arr 1))))"
        expected_syntax = Syntax.LetNode(
            ("arr", Type.ArrayType(Type.IntType)),
            Syntax.ArrayNode(Syntax.IntNode 0, Syntax.IntNode 2),
            Syntax.LetNode(
                ("Tu1", Type.UnitType),
                Syntax.PutNode(Syntax.VarNode "arr", Syntax.IntNode 0, Syntax.IntNode 10),
                Syntax.LetNode(
                    ("Tu2", Type.UnitType),
                    Syntax.PutNode(Syntax.VarNode "arr", Syntax.IntNode 1, Syntax.IntNode 20),
                    Syntax.AddNode(
                        Syntax.GetNode(Syntax.VarNode "arr", Syntax.IntNode 0),
                        Syntax.GetNode(Syntax.VarNode "arr", Syntax.IntNode 1)
                    )
                )
            )
        )
    }
]

[<Test>]
let testParsingSExprToSyntax () =
    for case in parsing_tests do
        Id.reset ()
        let parsed_s_expr = SExpr.parse case.s_expr
        let parsed_syntax = Parsing.f parsed_s_expr
        Assert.AreEqual(case.expected_syntax, parsed_syntax)
