module mini_caml_fsharp.Inlining

open mini_caml_fsharp.KNorm
open mini_caml_fsharp.M
open mini_caml_fsharp.Id
open mini_caml_fsharp.AlphaConv

module Inlining =
    let rec private size =
        function
        | KNorm.Let(_, e1, e2)
        | KNorm.LetRec({ body = e1 }, e2)
        | KNorm.BranchEq(_, _, e1, e2)
        | KNorm.BranchLE(_, _, e1, e2) -> 1 + size e1 + size e2
        | KNorm.LetTuple(_, _, e) -> 1 + size e
        | _ -> 1

    let rec private do_inline threshold (env: _ M) e =
        let g = do_inline threshold

        match e with
        | KNorm.BranchEq(x, y, e1, e2) -> KNorm.BranchEq(x, y, g env e1, g env e2)
        | KNorm.BranchLE(x, y, e1, e2) -> KNorm.BranchLE(x, y, g env e1, g env e2)
        | KNorm.Let(xt, e1, e2) -> KNorm.Let(xt, g env e1, g env e2)
        | KNorm.LetTuple(xts, y, e) -> KNorm.LetTuple(xts, y, g env e)
        | KNorm.LetRec({ name = x, t; args = yts; body = e1 }, e2) ->
            let env' = if size e1 > threshold then env else env.Add x (yts, e1)

            KNorm.LetRec(
                { name = x, t
                  args = yts
                  body = g env' e1 },
                g env' e2
            )
        | KNorm.Apply(x, ys) ->
            match env.TryFind x with
            | Some(zs, e) ->
                let env': Id.t M =
                    (zs, ys) ||> List.fold2 (fun acc (z, t) y -> acc.Add z y) (M.Empty())

                AlphaConv.alpha_convert env' e
            | _ -> e
        | _ -> e

    let f threshold e = do_inline threshold (M.Empty()) e
