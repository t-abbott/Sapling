namespace Incremental.Interp

open System
open Incremental
open Incremental.Cell
open Incremental.Expr
open Incremental.Utils

exception EvalError of string


module Eval =
    /// Evaluate an expression `expr` with respect to an environment `env`.
    /// Returns the resulting value and a list of all dynamic cells touched
    /// while evaluating `expr`
    let rec eval expr (Env env) =
        log (sprintf "eval %s" (expr.ToString()))

        match expr with
        | Number _
        | Bool _
        | Unit -> ref (Val expr), []
        | Var v ->
            // evaluate a dynamic cell when referenced
            match env.TryFind v with
            | Some ({ contents = Dyn (cell, env') }) ->
                let res, touchedCells = evalCell cell env'
                res, cell :: touchedCells
            | Some e -> e, []
            | None -> let msg = sprintf "reference to unbound variable: '%s'" v in raise (EvalError msg)

        | LetIn (name, e, body) ->
            // bind the name `name` to the value `e` in the expression `body`
            // since `e` can reference itself via `name` we need to evaluate it to a closure
            // first, and then add `name` to point to
            let placeholder = ref Uninitialised
            let env' = Map.add name placeholder env
            let boundVal, boundCells = eval e (Env env') // TODO prevent recursive non-function definitions
            let () = placeholder.Value <- boundVal.Value

            let res, resCells = eval body (Env env')
            res, resCells @ boundCells

        | LetDynIn (name, e, body) ->
            // create a thunk from the dynamic expression `e` to be
            // evaluated later
            // We don't return the dynamic value `e` as a dependency since
            // `body` doesn't necessarily use it

            let cell = Cell.from (Val e)
            let value = Dyn(cell, (Env env))
            let env' = Map.add name (ref value) env
            eval body (Env env')

        | Binop (l, op, r) -> evalBinop (l, op, r) (Env env)

        | IfThen (cond, e1, e2) ->
            let condVal, condCells = eval cond (Env env)

            match condVal.Value with
            | Val (Bool b) ->
                //
                let res, resCells =
                    match b with
                    | true -> eval e1 (Env env)
                    | false -> eval e2 (Env env)

                res, resCells @ condCells

            | _ ->
                let msg =
                    sprintf "expected condition to reduce to a bool in '(%s)'" (expr.ToString()) in

                raise (EvalError msg)

        | Fun _ -> ref (Env.close expr (Env env)), []

        | Apply (e1, e2) ->
            let arg, argCells = eval e2 (Env env)
            let func, _ = eval e1 (Env env)

            match func.Value with
            | Closure ((Fun (name, body)), (Env closedEnv)) ->
                //
                eval (body) (Env(Map.add name arg closedEnv))
            | BuiltIn b ->
                log (sprintf "calling builtin %s" (arg.Value.ToString()))
                log (sprintf "%s -> %s" (e2.ToString()) (arg.Value.ToString()))

                try
                    // Special case - pass the `Cell.T` object directly to `set`,
                    // rather than passing its value
                    if b.name = "set" then
                        match (e2) with
                        | Var x ->
                            match env.TryFind x with
                            | Some { contents = Dyn (cell, env') } ->
                                // `set` only returns unit, so one could argue that
                                // an expression calling `set cell` doesn't necessarily
                                // mean `cell` is a dependency. Hence we return an empty
                                // list
                                b.body (ref (Dyn(cell, env'))) (Env env), []
                            | _ -> failwith "no"
                        | _ -> failwith "no"
                    else
                        b.body arg (Env env), argCells
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
            let (x, xCells), (y, yCells) = (evalNumber l env), (evalNumber r env) in

            let res =
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

            ref res, xCells @ yCells
        else
            let (p, pCells), (q, qCells) = (evalBool l env), (evalBool r env) in

            let res =
                match op with
                | And -> Val(Bool(p && q))
                | Or -> Val(Bool(p || q))
                | _ -> failwith "unreachable"

            ref res, pCells @ qCells

    /// Evaluate an expression expected to reduce to a float
    and evalNumber expr (Env env) : float * Cell.T<EnvVal> list =
        let num, numCells = eval expr (Env env)

        match num.Value with
        | Val (Number n) -> n, numCells
        | _ ->
            let msg =
                sprintf "expected expression to reduce to a number: '%s'" (expr.ToString()) in raise (EvalError msg)

    /// Evaluate an expression expected to reduce to a bool
    and evalBool expr (Env env) : bool * Cell.T<EnvVal> list =
        let bool, boolCells = eval expr (Env env)

        match bool.Value with
        | Val (Bool b) -> b, boolCells
        | _ ->
            let msg =
                sprintf "expected expression to reduce to a boolean: '%s'" (expr.ToString()) in raise (EvalError msg)

    /// Get the current value of a cell, updating its value if it has been invalidated
    and evalCell (cell: Cell.T<EnvVal>) (Env env) : EnvVal ref * Cell.T<EnvVal> list =
        log (sprintf "evaluating cell %s" (cell.ToString()))

        match cell.value, cell.body with
        | Some v, _ -> ref v, cell.reads
        | None, (Val body) ->
            let e, touchedCells = eval body (Env env)
            cell.value <- (Some e.Value)

            // upadate `cell`s dependencies
            cell.reads <- touchedCells
            cell.RegisterObservers touchedCells

            e, touchedCells
        | _ -> failwith "unreachable"

    ///
    let run expr =
        let res, _ = eval expr Env.initial
        res


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
