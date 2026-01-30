module mini_caml_fsharp.M

open mini_caml_fsharp.Id

type 't M = { env: Map<Id.t, 't> } with
    static member Empty () : 't M = { env = Map.empty }
    static member OfMap (env : Map<Id.t, 't>)  = { env = env }
    member this.TryFind k = this.env |> Map.tryFind k
    member this.Add k v = this.env |> Map.add k v |> M.OfMap
    member this.Remove k = this.env |> Map.remove k |> M.OfMap
    member this.AddList xys = List.fold (fun env (k, v) -> env |> Map.add k v) this.env xys |> M.OfMap
    member this.AddList2 xs ys = List.fold2 (fun env k v -> env |> Map.add k v) this.env xs ys |> M.OfMap