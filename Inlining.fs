module mini_caml_fsharp.Inlining

open mini_caml_fsharp.KNorm
open mini_caml_fsharp.M
open mini_caml_fsharp.AlphaConv

module Inlining =
    /// Функция, вычисляющая "размер" выражения в глубину
    /// для дальнейшего решения, делать встраивание или нет.
    let rec private size =
        function
        | KNorm.Let(_, e1, e2)
        | KNorm.LetRec({ body = e1 }, e2)
        | KNorm.BranchEq(_, _, e1, e2)
        | KNorm.BranchLE(_, _, e1, e2) -> 1 + size e1 + size e2
        | KNorm.LetTuple(_, _, e) -> 1 + size e
        | _ -> 1

    /// <summary>
    /// Функция, осуществляющая встраивание.
    /// </summary>
    /// <param name="env">
    /// Окружение, содержащее объявления функций для встраивания.
    /// Ключом является имя функции, значением являются пары из
    /// списка имён аргументов и тела функции.
    /// </param>
    /// <param name="threshold">
    /// Порог размера, до которого происходит встраивание функций.
    /// </param>
    let rec private do_inline threshold (env: _ M) e =
        let g = do_inline threshold

        match e with
        | KNorm.BranchEq(x, y, e1, e2) -> KNorm.BranchEq(x, y, g env e1, g env e2)
        | KNorm.BranchLE(x, y, e1, e2) -> KNorm.BranchLE(x, y, g env e1, g env e2)
        | KNorm.Let(xt, e1, e2) -> KNorm.Let(xt, g env e1, g env e2)
        | KNorm.LetTuple(xts, y, e) -> KNorm.LetTuple(xts, y, g env e)
        | KNorm.LetRec({ name = x, t
                         args = args
                         body = body },
                       cont) ->
            let env' =
                match size body with
                | size when size <= threshold ->
                    // В случае если размер тела функции не превышеает
                    // порог, окружение подменяется копией себя с добавлением
                    // маппинга имени x на пару из имён аргументов (без типов,
                    // они нам не понадобятся далее) и тела функции
                    let args_without_types = args |> List.map fst
                    env.Add x (args_without_types, body)
                | _ -> env

            // В случае если в теле функции был рекурсивный вызов,
            // произойдёт однократное встраивание тела этой функции,
            // таким образом будет получен "частичный" инлайнинг одной
            // итерации такой функции
            let body' = body |> g env'

            // Рекурсивно вызываем инлайнинг для продолжение
            let cont' = cont |> g env'

            // Результатом работы является объявление рекурсивной функции
            // с потенциально встроенным телом и продолжением
            KNorm.LetRec(
                { name = x, t
                  args = args
                  body = body' },
                cont'
            )
        | KNorm.Apply(x, arg_vars) ->
            match env.TryFind x with
            | Some(arg_names, body) ->
                // Если тело функции для встраивания есть в окружении,
                // формируем окружение для альфа-конверсии имён в теле
                // этой функции. Таким образом, для мест в теле функции
                // где встречаются имена аргументов произойдёт подстановка,
                // а все остальные имена пройдут альфа-конверсию, чтобы
                // не возникло конфликта с вызывающей функцией
                let mutable alpha_env = M.Empty()

                for n, v in (arg_names, arg_vars) ||> List.zip do
                    alpha_env <- alpha_env.Add n v

                // Возвращаем альфа-конвертированное тело функции
                body |> AlphaConv.alpha_convert alpha_env
            | _ -> e
        | _ -> e

    let f threshold e = do_inline threshold (M.Empty()) e
