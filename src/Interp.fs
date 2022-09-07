module Incremental.Interp

open Incremental.Expr
open Incremental.Utils

exception EvalError of string

module Env = 
    /// Represents an environment mapping names to their values
    type T = Map<Name, EnvVal>
    /// Expression values
    and EnvVal =
        | Val of Expr.T
        | Closure of Expr.T * T
    
    let close (expr : Expr.T) env = Closure (expr, env)

/// Return a list of the free variables in an expression
let rec freeVars = function
    | Unit -> []
    | Bool _ -> []
    | Number _ -> []
    | Var v -> [v]
    | LetIn (name, _, body) ->
        without (freeVars body) name
    | Binop (l, _, r) ->
        union (freeVars l) (freeVars r)
    | IfThen (cond, e1, e2) ->
        union (freeVars e1) (freeVars e2) |> union (freeVars cond)
    | Fun (name, body) ->
        without (freeVars body) name
    | Apply (e1, e2) ->
        union (freeVars e1) (freeVars e2)

/// Return a list of the bound variables in an expression
let rec boundVars = function
    | LetIn (name, _, body) | Fun (name, body)->
        name :: (boundVars body)
    | _ -> []

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
                sprintf "expected condition to be a boolean in '(%s)'" (expr.ToString ())
            in raise (EvalError msg)
    | Fun _ ->
        Env.close expr env
    | Apply (e1, e2) ->
        let arg = eval e2 env in
        match eval e1 env with
        | Env.Closure ((Fun (name, body)), closedEnv) ->
            eval (body) (Map.add name arg closedEnv)
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