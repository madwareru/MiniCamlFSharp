module mini_caml_fsharp.Assoc

open mini_caml_fsharp.KNorm

module Assoc =
    /// Функция, предназначенная для разворачивания форм по типу
    /// (let x = (let y = (let z = 1 in (+ z 1)) in (+ y y)) in (- x))
    /// к виду
    /// (let z = 1 in
    /// (let y = (+ z 1) in
    /// (let x = (+ y y) in
    /// (-x)))),
    /// т. е. от множества вложенных выражений к линейной последовательности
    /// простых операций связывания имён. Это не делает программу эффективнее,
    /// но облегчает понимание структуры промежуточного представления при отладке
    /// и экспериментах. Так же это преобразование помогает делать другие оптимизации,
    /// например сворачивание констант, и приближает структуру кода к машинному коду
    let rec f e =
        match e with
        | KNorm.BranchEq(l, r, e1, e2) ->
            let e1' = f e1
            let e2' = f e2
            KNorm.BranchEq(l, r, e1', e2')
        | KNorm.BranchLE(l, r, e1, e2) ->
            let e1' = f e1
            let e2' = f e2
            KNorm.BranchLE(l, r, e1', e2')
        | KNorm.LetTuple(bound_names, binding, cont) ->
            let cont' = f cont
            KNorm.LetTuple(bound_names, binding, cont')
        | KNorm.LetRec({ name = name
                         args = args
                         body = body },
                       cont) ->
            let body' = f body
            let cont' = f cont

            KNorm.LetRec(
                { name = name
                  args = args
                  body = body' },
                cont'
            )
        | KNorm.Let(root_name, binding, root_cont) ->
            let binding' = f binding
            let root_cont' = f root_cont

            /// Функция для "утапливания" корневого (let root_name = ... in root_cont)
            /// внутрь связанного выражения. Пока в аргументе находится связывание
            /// имён с продолжением, происходит рекурсивный заход дальше внутрь
            let rec ins =
                function
                | KNorm.Let(name, binding, cont) -> KNorm.Let(name, binding, ins cont)
                | KNorm.LetTuple(bound_names, binding, cont) -> KNorm.LetTuple(bound_names, binding, ins cont)
                | KNorm.LetRec(fundef, cont) -> KNorm.LetRec(fundef, ins cont)
                | e -> KNorm.Let(root_name, e, root_cont')

            ins binding'
        | _ -> e
