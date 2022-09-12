namespace Incremental.Interp

open Incremental
open Incremental.Expr
open Incremental.Utils
open Incremental.Utils.Extensions

exception EvalError of string

module Env =
    /// <summary>Represents an environment mapping names to their values</summary>
    /// <remark>The actual values are mutable because of how we implement recursion</remark>
    type T = Map<Name, EnvVal ref>

    /// Expression values
    and EnvVal =
        | Uninitialised
        | Val of Expr.T
        | Closure of Expr.T * T
        | BuiltIn of Builtin.T

        override this.ToString() =
            match this with
            | Uninitialised -> "[uninitialised]"
            | Val e -> e.ToString()
            | Closure (expr, env) ->
                // TODO fix closure pretty printing. `ToString` doesn't let
                // us pass the names of the parent variables the closure is
                // defined in so you can't avoid expanding recursive definitions
                "[closure]"
            | BuiltIn b -> b.ToString()

        /// <summary>
        /// Substitute variables in `expr` for the expressions they map to in `env` without
        /// expanding the definitions of variables in `parentNames` (to avoid expanding
        /// recursive definitions).
        /// </summary>
        /// TODO: why is this in envval? move it out
        static member SubWithName (expr: Expr.T) (env: T) (parentNames: Set<Expr.Name>) =
            System.Console.WriteLine(sprintf "expr: %s \tparent names: %s" (expr.ToString()) (parentNames.ToString()))

            match expr with
            | Unit
            | Bool _
            | Number _ -> expr
            | Var name when parentNames.Contains name ->
                // don't expand recursive definitions (`name` is defined within `parentName`)
                Var name
            | Var name ->
                (match env.TryFind name with
                 | None -> Var name
                 | Some ({ contents = Val expr' }) -> expr'
                 | Some ({ contents = Closure (expr', env') }) -> EnvVal.SubWithName expr' env' parentNames
                 | Some ({ contents = envval }) -> Var(envval.ToString()))
            | LetIn (name, e, body) ->
                let body' = EnvVal.SubWithName body (env.Without name) (parentNames.Add name) in LetIn(name, e, body')
            | Binop (l, op, r) ->
                let l', r' = proj2 (fun e -> EnvVal.SubWithName e env parentNames) (l, r) in Binop(l', op, r')
            | IfThen (cond, e1, e2) ->
                let cond', e1', e2' =
                    proj3 (fun e -> EnvVal.SubWithName e env parentNames) (cond, e1, e2) in IfThen(cond', e1', e2')
            | Fun (name, body) -> let body' = EnvVal.SubWithName body (env.Without name) parentNames in Fun(name, body')
            | Apply (f, x) -> let f', x' = proj2 (fun e -> EnvVal.SubWithName e env parentNames) (f, x) in Apply(f', x')

        /// <summary>
        /// Substitute variables in `expr` for the expressions they map to in `env`
        /// </summary>
        /// <remark>
        /// Used for pretty-printing, not evaluation. This method converts references
        /// to built-in functions to prettiefied `Var` names
        /// </remark>
        static member Substitute expr env =
            let _ = System.Console.WriteLine("substituting " + (expr.ToString())) in

            EnvVal.SubWithName expr env Set.empty

    let substitute = EnvVal.Substitute

    let empty = Map.empty

    /// Initial environment containing all built-in functions and variables
    let initial =
        Builtin.functions |> List.map (fun b -> b.name, ref (BuiltIn b)) |> Map.ofList

    /// Create a closure from an expression and environment
    let close (expr: Expr.T) env = Closure(expr, env)

module Eval =
    /// Evaluate an expression `expr` with respect to an environment `env`
    let rec eval expr (env: Env.T) =
        match expr with
        | Number _
        | Bool _
        | Unit -> ref (Env.Val expr)
        | Var v ->
            (match (Map.tryFind v env) with
             | Some e -> e
             | None -> let msg = sprintf "reference to unknown variable '%s'" v in raise (EvalError msg))
        | LetIn (name, e, body) ->
            // bind the name `name` to the value `e` in the expression `body`
            // since `e` can reference itself via `name` we need to evaluate it to a closure
            // first, and then add `name` to point to
            let placeholder = ref Env.Uninitialised in
            let env' = Map.add name placeholder env in
            let boundVal = (eval e env').Value in // TODO prevent recursive variable definitions
            let () = placeholder.Value <- boundVal in
            eval body env'
        | Binop (l, op, r) -> evalBinop (l, op, r) env
        | IfThen (cond, e1, e2) ->
            match eval cond env with
            | { contents = Env.Val (Bool b) } -> if b then (eval e1 env) else (eval e2 env)
            | _ ->
                let msg =
                    sprintf "expected condition to reduce to a bool in '(%s)'" (expr.ToString()) in

                raise (EvalError msg)
        | Fun _ -> ref (Env.close expr env)
        | Apply (e1, e2) ->
            // TODO refactor to match on `(eval expr exv).Value`
            let arg = eval e2 env in

            match eval e1 env with
            | { contents = Env.Closure ((Fun (name, body)), closedEnv) } -> eval (body) (Map.add name arg closedEnv)
            | { contents = Env.BuiltIn b } ->
                let arg' =
                    match arg with
                    | { contents = Env.Val v } -> v
                    | _ ->
                        let msg = sprintf "can only apply values to builtin, got '%s'" (e2.ToString()) in

                        raise (EvalError msg)

                let res = b.body arg' in
                ref (Env.Val res)
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
            | LessThan -> Env.Val(Bool(x < y))
            | GreaterThan -> Env.Val(Bool(x > y))
            | Equals -> Env.Val(Bool(x = y))
            | Add -> Env.Val(Number(x + y))
            | Sub -> Env.Val(Number(x - y))
            | Mult -> Env.Val(Number(x * y))
            | Div -> Env.Val(Number(x / y))
            | Mod -> Env.Val(Number(x % y))
            | _ -> failwith "unreachable"
        else
            let p, q = (evalBool l env), (evalBool r env) in

            match op with
            | And -> Env.Val(Bool(p && q))
            | Or -> Env.Val(Bool(p || q))
            | _ -> failwith "unreachable"
        |> ref

    and evalNumber expr env =
        match eval expr env with
        | { contents = Env.Val (Number n) } -> n
        | _ ->
            let msg =
                sprintf "expected expression to reduce to a number: '%s'" (expr.ToString()) in raise (EvalError msg)

    and evalBool expr env =
        match eval expr env with
        | { contents = Env.Val (Bool b) } -> b
        | _ ->
            let msg =
                sprintf "expected expression to reduce to a boolean: '%s'" (expr.ToString()) in raise (EvalError msg)

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
