namespace Incremental.Interp

open Incremental
open Incremental.Expr
open Incremental.Utils
open Incremental.Utils.Extensions

exception EvalError of string

module Env = 
    /// Represents an environment mapping names to their values
    type T = Map<Name, EnvVal>
    /// Expression values
    and EnvVal =
        | Val of Expr.T
        | Closure of Expr.T * T
        | BuiltIn of Builtin.T
        with 
            override this.ToString () =
                    match this with
                    | Val e -> e.ToString ()
                    | Closure (expr, env) ->
                        // this works for now since we're not changing the ast at all.
                        // Will break if we fix name capture or start optimising things
                        (EnvVal.Substitute expr env).ToString ()
                    | BuiltIn b ->
                        b.ToString ()


            /// <summary>
            /// Substitute variables in `expr` for the expressions they map to in `env`
            /// </summary>
            /// <remark>
            /// Used for pretty-printing, not evaluation. This method converts references
            /// to built-in functions to prettiefied `Var` names</remark>
            static member Substitute (expr : Expr.T) (env : T) =
                match expr with
                | Unit | Bool _ | Number _ -> expr 
                | Var v ->
                    match env.TryFind v with
                    | None -> Var v
                    | Some (Val expr') ->
                        expr'
                    | Some (Closure (expr', env')) ->
                        EnvVal.Substitute expr' env'
                    | Some (BuiltIn _ as b) ->
                        Var (b.ToString ())
                | LetIn (name, e, body) ->
                    let body' = EnvVal.Substitute body (env.Without name) in
                    LetIn(name, e, body')
                | Binop (l, op, r) ->
                    let l', r' = proj2 (fun e -> EnvVal.Substitute e env) (l, r) in
                    Binop (l', op, r')
                | IfThen (cond, e1, e2) ->
                    let cond', e1', e2' = proj3 (fun e -> EnvVal.Substitute e env) (cond, e1, e2) in
                    IfThen (cond', e1', e2')
                | Fun (name, body) ->
                    let body' = EnvVal.Substitute body (env.Without name) in
                    Fun (name, body')
                | Apply (f, x) ->
                    let f', x' = proj2 (fun e -> EnvVal.Substitute e env) (f, x) in
                    Apply (f', x')

    let substitute = EnvVal.Substitute
 
    let empty = Map.empty
    
    /// Initial environment containing all built-in functions and variables
    let initial = 
        Builtin.functions 
        |> List.map (fun b -> b.name, BuiltIn b) 
        |> Map.ofList

    /// Create a closure from an expression and environment
    let close (expr : Expr.T) env = Closure (expr, env)

module Eval =
    /// Evaluate an expression `expr` with respect to an environment `env`
    let rec eval expr (env : Env.T) =
        match expr with
        | Number _  | Bool _ | Unit -> Env.Val expr
        | Var v ->
            (match (Map.tryFind v env) with
            | Some e -> e
            | None ->
                let msg = 
                    sprintf "reference to unknown variable '%s'" v
                in
                raise (EvalError msg))
        | LetIn (name, value, body) ->
            eval body (Map.add name (eval value env) env) 
        | Binop (l, op, r) ->
            evalBinop (l, op, r) env
        | IfThen (cond, e1, e2) ->
            match eval cond env with
            | Env.Val (Bool b) ->
                if b then (eval e1 env) else (eval e2 env)
            | _ ->
                let msg =
                    sprintf "expected condition to reduce to a bool in '(%s)'" (expr.ToString ())
                in raise (EvalError msg)
        | Fun _ ->
            Env.close expr env
        | Apply (e1, e2) ->
            let arg = eval e2 env in
            match eval e1 env with
            | Env.Closure ((Fun (name, body)), closedEnv) ->
                eval (body) (Map.add name arg closedEnv)
            | Env.BuiltIn b ->
                let arg' = 
                    match arg with
                    | Env.Val v -> v 
                    | _ ->
                        let msg = 
                            sprintf "can only apply values to builtin, got '%s'" (e2.ToString ()) 
                        in raise (EvalError msg)
                let res = b.body arg' in 
                Env.Val res
            | _ ->
                let msg = 
                    sprintf "tried to apply a value to something that isn't a function in expression '%s'" (expr.ToString ())
                in
                raise (EvalError msg)

    // TODO catch evalNumber/evalBool exceptions and display a better message
    and evalBinop (l, op, r) env =
        if op.hasNumericArgs then
            let x, y = (evalNumber l env), (evalNumber r env) in
            match op with
            | LessThan -> Env.Val (Bool (x < y))
            | GreaterThan -> Env.Val (Bool (x > y))
            | Equals -> Env.Val (Bool (x = y))
            | Add -> Env.Val (Number (x + y))
            | Sub -> Env.Val (Number (x - y))
            | Mult -> Env.Val (Number (x * y))
            | Div -> Env.Val (Number (x / y))
            | Mod -> Env.Val (Number (x % y))
            | _ -> failwith "unreachable"
        else
            let p, q = (evalBool l env), (evalBool r env) in
            match op with
            | And -> Env.Val (Bool (p && q))
            | Or -> Env.Val (Bool (p || q))
            | _ -> failwith "unreachable"

    and evalNumber expr env =
        match eval expr env with
        | Env.Val (Number n) -> n
        | _ -> 
            let msg = 
                sprintf "expected expression to reduce to a number: '%s'" (expr.ToString ()) in
            raise (EvalError msg)

    and evalBool expr env =
        match eval expr env with
        | Env.Val (Bool b) -> b
        | _ ->
            let msg =
                sprintf "expected expression to reduce to a boolean: '%s'" (expr.ToString ()) in
            raise (EvalError msg)

    let run expr = 
        eval expr Env.initial

module Repl =

    open System
    open Incremental.Parse

    let eval input = 
        let ast = parse input in
        try
            Ok (Eval.run ast)
        with 
        | EvalError(msg) -> Error (msg)

    let writePrompt () =
        Console.Write "> "

    let rec run () =
        writePrompt ()
        let input = Console.ReadLine () in
        let () = Console.Error.WriteLine input in
        if input = ":q" then
            ()
        else
            match eval input with
            | Ok(res) -> 
                Console.WriteLine (res.ToString ());
            | Error(msg) ->
                Console.WriteLine msg 
            run ()
