module mini_caml_fsharp.STests

open NUnit.Framework
open mini_caml_fsharp.S

[<Test>]
let testSOperations () =
    let s0 = S.OfList ["2"; "3"]
    let s1 = S.OfList ["1"; "2"; "3"; "4"; "5"]
    let s0excS1 = s0.Exclude(s1)
    let s1excS0 = s1.Exclude(s0)
    let intesection = s0.Intersect(s1)
    let union = s0.Union(s1)
    Assert.AreEqual(S.Empty (), s0excS1)
    Assert.AreEqual(S.OfList ["1"; "4"; "5"], s1excS0)
    Assert.AreEqual(S.OfList ["1"; "2"; "3"; "4"; "5"], union)
    Assert.AreEqual(S.OfList ["2"; "3"], intesection)
    Assert.True(s0.IsSubsetOf(s1))
    Assert.True(s1.IsSupersetOf(s0))
    Assert.AreEqual(S.OfList ["2"; "3"; "8"; "9"; "10"], s0.AddList ["8"; "9"; "10"])