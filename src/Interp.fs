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
        Log.debug (sprintf "evaluating '%s'" (expr.ToString()))

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
            // Bind the `name` to the value `e` in the expression `body`.
            // Since `e` can reference itself via `name` (recursion), we evaluate
            // `e` to a closure first and then point `name` to it in the env
            let placeholder = ref Uninitialised
            let env' = Map.add name placeholder env
            let boundVal, boundCells = eval e (Env env') // TODO prevent recursive non-function definitions
            let () = placeholder.Value <- boundVal.Value

            let res, resCells = eval body (Env env')
            res, resCells @ boundCells

        | LetDynIn (name, e, body) ->
            // Here we create a cell to wrap the dynamic expression `e`, but don't
            // compute the value `e` reduces to (it's basically a thunk).
            // `e` isn't returned as a dependency of this expression since `body`
            // doesn't necessarily reference it, and the value of `body` is what this
            // expression will evaluate to

            let cell = Cell.from (Val e)
            let value = Dyn(cell, (Env env))
            let env' = Map.add name (ref value) env
            eval body (Env env')

        | Binop (l, op, r) -> evalBinop (l, op, r) (Env env)

        | IfThen (cond, e1, e2) ->
            let condVal, condCells = eval cond (Env env)

            match condVal.Value with
            | Val (Bool b) ->
                let res, resCells =
                    match b with
                    | true -> eval e1 (Env env)
                    | false -> eval e2 (Env env)

                res, resCells @ condCells

            | _ ->
                let msg =
                    sprintf "expected the condition to reduce to a bool in '(%s)'" (expr.ToString())

                raise (EvalError msg)

        | Fun _ -> ref (Env.close expr (Env env)), []

        | Apply (e1, e2) ->
            // FIX: look at this code again. Can `func` be dependent on anything?
            // e.g. if it's a dynamically created closure

            let arg, argCells = eval e2 (Env env)
            let func, _ = eval e1 (Env env)

            match func.Value with
            | Closure ((Fun (name, body)), (Env closedEnv)) -> eval (body) (Env(Map.add name arg closedEnv))
            | BuiltIn b ->
                Log.info (sprintf "calling builtin %s" b.name)

                try
                    // `set` is a special case - it takes the `Cell.T` object directly,
                    // rather than the value the cell represents
                    if b.name = "set" then
                        match (e2) with
                        | Var x ->
                            match env.TryFind x with
                            | Some { contents = Dyn (cell, env') } ->
                                // `set` returns unit, so one could argue that an expression
                                // calling `set cell` doesn't necessarily mean `cell` is a
                                // dependency. Hence we return an empty list.
                                b.body (ref (Dyn(cell, env'))) (Env env), []
                            | _ ->
                                let msg = sprintf "`set` expected a dynamic value, got '%s'" (e2.ToString())
                                raise (EvalError msg)
                        | _ ->
                            let msg = sprintf "`set` expected a dynamic value, got '%s'" (e2.ToString())
                            raise (EvalError msg)
                    else
                        b.body arg (Env env), argCells
                with BuiltinError err ->
                    let msg = sprintf "error calling '%s': %s" err.name err.message
                    raise (EvalError msg)
            | _ ->
                let msg =
                    sprintf
                        "tried to apply a value to something that isn't a function in expression '%s'"
                        (expr.ToString())

                raise (EvalError msg)

    // TODO: catch evalNumber/evalBool exceptions and display a better message
    /// Evaluate a binary operator
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
        let id = string cell.id

        match cell.value, cell.body with
        | Some v, _ ->
            Log.debug (sprintf "found cached value for cell '%s'" id)
            ref v, cell.reads
        | None, (Val body) ->
            let e, touchedCells = eval body (Env env)
            cell.value <- (Some e.Value)

            // upadate `cell`s dependencies
            cell.reads <- touchedCells
            cell.RegisterObservers touchedCells

            Log.debug (sprintf "computed cell '%s', touched %d other cells" id touchedCells.Length)
            e, touchedCells
        | _ -> failwith "unreachable"

    /// Evaluate an expression `expr`
    let run expr =
        let res, _ = eval expr Env.initial
        res.Value


module Repl =
    open Incremental.Parse

    let eval input =
        let ast = parse input
        Log.debug "parsed input"

        try
            Ok(Eval.run ast)
        with EvalError (msg) ->
            Error(msg)

    let writePrompt () = Console.Write "> "

    let rec run () =
        writePrompt ()
        let input = Console.ReadLine() in

        if input = ".exit" then
            Log.debug "exiting repl"
            ()
        else
            match eval input with
            | Ok (res) -> Console.WriteLine(res.ToString())
            | Error (msg) -> Console.WriteLine msg

            run ()
