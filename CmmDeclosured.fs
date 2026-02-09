module mini_caml_fsharp.CmmDeclosured

open mini_caml_fsharp.Id
open mini_caml_fsharp.Type

(*
    Данное представление практически аналогично представлению Cmm
    Отличия:
      - Отсутствует операция вызова замыкания;
      - Отсутствует список свободных переменных в toplevel функции;

    Основная идея такова, что программу в представлении Cmm можно
    преобразовать к такому виду, где toplevel функция, ожидавшая
    ранее, что её будут вызывать особенным образом теперь имеет
    специальный первый параметр, где лежит кортеж с замыканием,
    и она самостоятельно в теле вызова распаковывает это замыкание
    в локальные переменные с именами соответствующими таковым в
    свободных. Вызов замыкания же превращается в обычный прямой
    вызов с передачей этого замыкания в первый аргумент.

*)
module CmmDeclosured =
    type atom_expr_t =
        | Unit
        | Int of int64
        | Float of double
        | FunctionPtr of Id.l

    type block_t =
        | Seq of statement_t * block_t
        | Return of expr_t

    and statement_t = Assignment of (Id.t * Type.t) * expr_t

    and expr_t =
        // Атомы и ссылки на переменные
        | Atom of atom_expr_t
        | Var of Id.t

        // Операции над целыми числами
        | Neg of Id.t
        | Add of Id.t * Id.t
        | Sub of Id.t * Id.t

        // Операции над числами с плавающей точкой
        | FNeg of Id.t
        | FAdd of Id.t * Id.t
        | FSub of Id.t * Id.t
        | FMul of Id.t * Id.t
        | FDiv of Id.t * Id.t

        // Операции над массивами, кортежами и замыканиями
        | MemoryGet of Id.t * Id.t
        | MemoryPut of Id.t * Id.t * Id.t
        | ExternalMemory of Id.l

        // Прямой вызов функции по метке из toplevel
        | Apply of Id.l * Id.t list

        // Ветвления
        | BranchEq of Id.t * Id.t * block_t * block_t
        | BranchLE of Id.t * Id.t * block_t * block_t

    type fn_t =
        { name: Id.l * Type.t
          args: (Id.t * Type.t) list
          body: block_t }

    type program_t =
        { top_level_functions: fn_t list
          entry: block_t }
