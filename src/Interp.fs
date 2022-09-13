namespace Incremental.Interp

open Incremental
open Incremental.Cell
open Incremental.Expr
open Incremental.Utils

exception EvalError of string


module Eval =
    /// Evaluate an expression `expr` with respect to an environment `env`
    let rec eval expr (Env env) =
        match expr with
        | Number _
        | Bool _
        | Unit -> ref (Val expr)
        | Var v ->
            (match (Map.tryFind v env) with
             | Some e -> e
             | None -> let msg = sprintf "reference to unknown variable '%s'" v in raise (EvalError msg))
        | Dyn cell -> failwith "not implemented"
        | LetIn (name, e, body) ->
            // bind the name `name` to the value `e` in the expression `body`
            // since `e` can reference itself via `name` we need to evaluate it to a closure
            // first, and then add `name` to point to
            let placeholder = ref EnvVal.Uninitialised in
            let env' = Map.add name placeholder env in
            let boundVal = (eval e (Env env')).Value in // TODO prevent recursive variable definitions
            let () = placeholder.Value <- boundVal in
            eval body (Env env')
        | LetDynIn (name, e, body) -> raise (EvalError "dyn expressions not implemented")
        | Binop (l, op, r) -> evalBinop (l, op, r) (Env env)
        | IfThen (cond, e1, e2) ->
            match eval cond (Env env) with
            | { contents = EnvVal.Val (Bool b) } -> if b then (eval e1 (Env env)) else (eval e2 (Env env))
            | _ ->
                let msg =
                    sprintf "expected condition to reduce to a bool in '(%s)'" (expr.ToString()) in

                raise (EvalError msg)
        | Fun _ -> ref (Env.close expr (Env env))
        | Apply (e1, e2) ->
            // TODO refactor to match on `(eval expr exv).Value`
            let arg = eval e2 (Env env) in

            match eval e1 (Env env) with
            | { contents = Closure ((Fun (name, body)), (Env closedEnv)) } ->
                eval (body) (Env(Map.add name arg closedEnv))
            | { contents = BuiltIn b } ->
                let arg' =
                    match arg with
                    | { contents = EnvVal.Val v } -> v
                    | _ ->
                        let msg = sprintf "can only apply values to builtin, got '%s'" (e2.ToString()) in

                        raise (EvalError msg)

                let res = b.body arg' in
                ref res
            | _ ->
                let msg =
                    sprintf
                        "tried to apply a value to something that isn't a function in expression '%s'"
                        (expr.ToString()) in raise (EvalError msg)

    // TODO catch evalNumber/evalBool exceptions and display a better message
    and evalBinop (l, op, r) env =
        if op.hasNumericArgs then
            let x, y = (evalNumber l env), (evalNumber r env) in

            match op with
            | LessThan -> Val(Bool(x < y))
            | GreaterThan -> Val(Bool(x > y))
            | Equals -> Val(Bool(x = y))
            | Add -> Val(Number(x + y))
            | Sub -> Val(Number(x - y))
            | Mult -> Val(Number(x * y))
            | Div -> Val(Number(x / y))
            | Mod -> Val(Number(x % y))
            | _ -> failwith "unreachable"
        else
            let p, q = (evalBool l env), (evalBool r env) in

            match op with
            | And -> Val(Bool(p && q))
            | Or -> Val(Bool(p || q))
            | _ -> failwith "unreachable"
        |> ref

    and evalNumber expr (Env env) =
        match eval expr (Env env) with
        | { contents = Val (Number n) } -> n
        | _ ->
            let msg =
                sprintf "expected expression to reduce to a number: '%s'" (expr.ToString()) in raise (EvalError msg)

    and evalBool expr (Env env) =
        match eval expr (Env env) with
        | { contents = Val (Bool b) } -> b
        | _ ->
            let msg =
                sprintf "expected expression to reduce to a boolean: '%s'" (expr.ToString()) in raise (EvalError msg)

    ///
    and getCell (cell: Cell.T<Expr>) : Expr =
        match cell.value with
        | Some v -> v
        | None ->
            // eval the cell
            // does it need an env? can a cell capture names? yes; fuck.
            failwith "no"

    ///
    let run expr = eval expr Env.initial


module Repl =
    open System
    open Incremental.Parse

    let eval input =
        let ast = parse input in

        try
            Ok(Eval.run ast)
        with EvalError (msg) ->
            Error(msg)

    let writePrompt () = Console.Write "> "

    let rec run () =
        writePrompt ()
        let input = Console.ReadLine() in

        if input = ".exit" then
            ()
        else
            match eval input with
            | Ok (res) -> Console.WriteLine(res.Value.ToString())
            | Error (msg) -> Console.WriteLine msg

            run ()
