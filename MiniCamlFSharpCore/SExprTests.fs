module mini_caml_fsharp_core.SExprTests

open Microsoft.FSharp.Core
open NUnit.Framework
open mini_caml_fsharp_core.SExpr

[<Test>]
let testSExprParser () =
    let tests: (string * SExpr.SExpr) list =
        [ "123", SExpr.SExprInt 123

          "hello?", SExpr.SExprId(Seq.toList "hello?")

          "(1 2.51 #f)", SExpr.SExprList [ SExpr.SExprInt 1; SExpr.SExprFloat 2.51; SExpr.SExprBool false ]

          "((1 2 3) (1 2 3) (1 2 3))",
          SExpr.SExprList
              [ SExpr.SExprList [ SExpr.SExprInt 1; SExpr.SExprInt 2; SExpr.SExprInt 3 ]
                SExpr.SExprList [ SExpr.SExprInt 1; SExpr.SExprInt 2; SExpr.SExprInt 3 ]
                SExpr.SExprList [ SExpr.SExprInt 1; SExpr.SExprInt 2; SExpr.SExprInt 3 ] ]

          "(a/test 2 1 + 3)",
          SExpr.SExprList
              [ SExpr.SExprId(Seq.toList "a/test")
                SExpr.SExprInt 2
                SExpr.SExprInt 1
                SExpr.SExprId(Seq.toList "+")
                SExpr.SExprInt 3 ] ]

    for source, expected in tests do
        Assert.AreEqual(expected, SExpr.parse source)
