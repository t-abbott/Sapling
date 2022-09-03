namespace Incremental

open Incremental.Syntax


module Interp =
    type Environment = Map<Name, Expr>

    exception EvalError of string

    let rec eval expr env =
        match expr with
        | Number n -> n
        | Binop (l, op, r) ->
            let op_fn = 
                match op with
                | "+" -> ( + )
                | "-" -> ( - )
                | _ ->
                    let msg = sprintf "invalid operator '%s'" op in
                    raise (EvalError msg)
            in 
                op_fn (eval l env) (eval r env)

