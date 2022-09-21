namespace Incremental.Interp

open System
open Incremental
open Incremental.Cell
open Incremental.Expr
open Incremental.Utils

exception EvalError of string


module Eval =
    /// Evaluate an expression `expr` with respect to an environment `env`
    /// TODO make this return a list of the dynamic cells touched
    let rec eval expr (Env env) =
        log (sprintf "eval %s" (expr.ToString()))

        match expr with
        | Number _
        | Bool _
        | Unit -> ref (Val expr)
        | Var v ->
            // evaluate a dynamic cell when referenced
            match env.TryFind v with
            | Some ({ contents = Dyn (cell, env') }) -> evalCell cell env'
            | Some e -> e
            | None -> let msg = sprintf "reference to unbound variable: '%s'" v in raise (EvalError msg)

        | LetIn (name, e, body) ->
            // bind the name `name` to the value `e` in the expression `body`
            // since `e` can reference itself via `name` we need to evaluate it to a closure
            // first, and then add `name` to point to
            let placeholder = ref Uninitialised in
            let env' = Map.add name placeholder env in
            let boundVal = (eval e (Env env')).Value in // TODO prevent recursive non-function definitions
            let () = placeholder.Value <- boundVal in
            eval body (Env env')

        | LetDynIn (name, e, body) ->
            // create a thunk from the dynamic expression `e` to be
            // evaluated later
            let cell = Cell.from (Val e)
            let value = Dyn(cell, (Env env))
            let env' = Map.add name (ref value) env
            eval body (Env env')

        | Binop (l, op, r) -> evalBinop (l, op, r) (Env env)
        | IfThen (cond, e1, e2) ->
            match eval cond (Env env) with
            | { contents = Val (Bool b) } -> if b then (eval e1 (Env env)) else (eval e2 (Env env))
            | _ ->
                let msg =
                    sprintf "expected condition to reduce to a bool in '(%s)'" (expr.ToString()) in

                raise (EvalError msg)

        | Fun _ -> ref (Env.close expr (Env env))
        | Apply (e1, e2) ->
            let arg = eval e2 (Env env)

            // I think this is the problem:
            // in eagerly evaluating the argument we get the current value of the cell,
            // but if we're calling `set` we need to pass the reference to the cell itself
            // so do we pass the argument dn then evaluate it?

            match (eval e1 (Env env)).Value with
            | Closure ((Fun (name, body)), (Env closedEnv)) -> eval (body) (Env(Map.add name arg closedEnv))
            | BuiltIn b ->
                log (sprintf "calling builtin %s" (arg.Value.ToString()))
                log (sprintf "%s -> %s" (e2.ToString()) (arg.Value.ToString()))

                try
                    if b.name = "set" then
                        match (e2) with
                        | Var x ->
                            match env.TryFind x with
                            | Some { contents = Dyn (cell, env') } -> b.body (ref (Dyn(cell, env'))) (Env env)
                            | _ -> failwith "no"
                        | _ -> failwith "no"
                    else
                        b.body arg (Env env)
                with BuiltinError err ->
                    let msg = sprintf "error calling '%s': %s" err.name err.message
                    raise (EvalError msg)
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

    /// Get the current value of a cell, updating its value if it has been invalidated
    and evalCell (cell: Cell.T<EnvVal>) (Env env) : EnvVal ref =
        log (sprintf "evaluating cell %s" (string cell.id))
        log (cell.ToString())

        match cell.value, cell.body with
        | Some v, _ -> ref v
        | None, (Val body) ->
            let e = eval body (Env env)
            cell.value <- (Some e.Value)
            log (sprintf "evaluated cell, got %s" (e.Value.ToString()))
            e
        | _ -> failwith "unreachable"


    ///
    let run expr = eval expr Env.initial


module Repl =
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
