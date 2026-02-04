module mini_caml_fsharp.S

open mini_caml_fsharp.Id

type S = { set: Set<Id.t> } with
    static member Empty () : S = { set = Set.empty }
    static member OfSet (set : Set<Id.t>) = { set = set }
    static member Singleton x = Set.singleton x |> S.OfSet
    static member OfList l = Set.ofList l |> S.OfSet
    member this.Contains k = this.set |> Set.contains k
    member this.Add x = this.set |> Set.add x |> S.OfSet
    member this.Union s = this.set |> Set.union s.set |> S.OfSet
    member this.Exclude s = this.set |> Set.difference s.set |> S.OfSet
    member this.Intersect s = this.set |> Set.intersect s.set |> S.OfSet
    member this.IsSubsetOf s = this.set |> Set.isSubset s.set
    member this.IsSupersetOf s = this.set |> Set.isSuperset s.set
    member this.Remove x = this.set |> Set.remove x |> S.OfSet
    member this.AddList xs = List.fold (fun set x -> set |> Set.add x) this.set xs |> S.OfSet
    member this.IsEmpty () = this.set |> Set.isEmpty