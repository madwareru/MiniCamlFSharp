module mini_caml_fsharp.UnificationTests

open Microsoft.FSharp.Core
open NUnit.Framework
open mini_caml_fsharp.Type
open mini_caml_fsharp.Typing

type private test_case =
    {
        before_unification: Type.t * Type.t
        expected_after: Type.t * Type.t
    }

let private tests = [
    // Простые случаи
    {
        before_unification = Type.UnitType, Type.UnitType
        expected_after = Type.UnitType, Type.UnitType
    }
    {
        before_unification = Type.IntType, Type.VarType(ref None)
        expected_after = Type.IntType, Type.VarType(ref <| Some Type.IntType)
    }
    {
        before_unification = Type.VarType(ref None), Type.FloatType
        expected_after = Type.VarType(ref <| Some Type.FloatType), Type.FloatType
    }

    //Перекрёстные функции
    {
        before_unification = (
            Type.FunType([Type.UnitType], Type.VarType(ref None)),
            Type.FunType([Type.VarType(ref None)], Type.IntType)
        )
        expected_after = (
            Type.FunType([Type.UnitType], Type.VarType(ref <| Some Type.IntType)),
            Type.FunType([Type.VarType(ref <| Some Type.UnitType)], Type.IntType)
        )
    }
    
    // Пустой тип и тип функции
    {
        before_unification = (
            Type.VarType(ref None),
            Type.FunType([Type.IntType], Type.IntType)
        )
        expected_after = (
            Type.VarType(ref(Some <| Type.FunType([Type.IntType], Type.IntType))),
            Type.FunType([Type.IntType], Type.IntType)
        )
    }
    {
        before_unification = (
            Type.FunType([Type.IntType], Type.IntType),
            Type.VarType(ref None)
        )
        expected_after = (
            Type.FunType([Type.IntType], Type.IntType),
            Type.VarType(ref(Some <| Type.FunType([Type.IntType], Type.IntType)))
        )
    }
]

[<Test>]
let testUnification () =
    for {
        before_unification = (t0, t1)
        expected_after = (tx0, tx1)
    } in tests do
        Typing.unify t0 t1
        Assert.AreEqual(tx0, t0)
        Assert.AreEqual(tx1, t1)
