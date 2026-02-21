module mini_caml_fsharp.GenShared

open mini_caml_fsharp.Cmm
open mini_caml_fsharp.M
open mini_caml_fsharp.Id
open mini_caml_fsharp.SExpr
open mini_caml_fsharp.Parsing
open mini_caml_fsharp.Typing
open mini_caml_fsharp.KNormalisation
open mini_caml_fsharp.AlphaConv
open mini_caml_fsharp.BetaReduction
open mini_caml_fsharp.Assoc
open mini_caml_fsharp.Inlining
open mini_caml_fsharp.ConstFolding
open mini_caml_fsharp.Elimination
open mini_caml_fsharp.ClosureRepresentationConv
open mini_caml_fsharp.CmmConv

module GenShared =
    let gen_c_compliant_idents (p: Cmm.program_t) =
        let mutable env_labels = M.Empty()
        let mutable env_l_ts = M.Empty()
        let mutable env_ids = M.Empty()
        let mutable env_id_ts = M.Empty()
        let mutable label_counter = 0
        let mutable id_counter = 0

        let gen_label label t =
            let (Id.L l) = label

            match env_labels.TryFind l with
            | Some _ -> ()
            | None ->
                let (new_label: Id.t) = $"fn_{label_counter}"
                label_counter <- label_counter + 1
                env_labels <- env_labels.Add l new_label
                env_l_ts <- env_l_ts.Add l t
                ()

        let gen_id id t =
            match env_ids.TryFind id with
            | Some _ -> ()
            | None ->
                let (new_id: Id.t) = $"v_{id_counter}"
                id_counter <- id_counter + 1
                env_ids <- env_ids.Add id new_id
                env_id_ts <- env_id_ts.Add id t
                ()

        let rec visit_block (b: Cmm.block_t) =
            match b with
            | Cmm.Seq(Cmm.Assignment((id, t), e), next_block) ->
                gen_id id t

                match e with
                | Cmm.BranchEq(_, _, then_block, else_block)
                | Cmm.BranchLE(_, _, then_block, else_block) ->
                    visit_block then_block
                    visit_block else_block
                    visit_block next_block
                | _ -> visit_block next_block
            | Cmm.Return e ->
                match e with
                | Cmm.BranchEq(_, _, then_block, else_block)
                | Cmm.BranchLE(_, _, then_block, else_block) ->
                    visit_block then_block
                    visit_block else_block
                | _ -> ()

        let visit_fn (fn: Cmm.fn_t) =
            let l, t = fn.name
            gen_label l t

            for arg_id, arg_t in fn.args do
                gen_id arg_id arg_t

            visit_block fn.body

        for fn in p.top_level_functions do
            visit_fn fn

        visit_block p.entry

        env_labels, env_ids, env_id_ts, env_l_ts
        
    type pre_gen_settings_t =
        { mutable inlining_threshold: int
          mutable optimization_loop_limit: int
          mutable typing_rule : Typing.program_output_typing_rule_t
        }
        
    let pre_gen source (settings : pre_gen_settings_t) =
        Id.reset ()
        let k_form =
            source
            |> SExpr.parse
            |> Parsing.f
            |> Typing.f settings.typing_rule
            |> KNormalisation.f
            |> AlphaConv.f
            
        let rec iter n e =
            printfn $"iteration %d{n}."
            match n with
            | 0 -> e
            | _ ->
                let e' = e |> BetaReduction.f
                let e' = e' |> Assoc.f
                let e' = e' |> Inlining.f settings.inlining_threshold
                let e' = e' |> ConstFolding.f
                let e' = e' |> Elimination.f
                if e = e' then e' else iter (n - 1) e'

        (settings.optimization_loop_limit, k_form)
            ||> iter
            |> ClosureRepresentationConv.f
            |> CmmConv.f