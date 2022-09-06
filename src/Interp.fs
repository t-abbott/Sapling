module Incremental.Interp

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
        union (freeVars e1) (freeVars e1) |> union (freeVars cond)
    | Fun (name, body) ->
        without (freeVars body) name
    | Apply (e1, e2) ->
        union (freeVars e1) (freeVars e2)

/// Return a list of the bound variables in an expression
let rec boundVars = function
    | LetIn (name, _, body) | Fun (name, body)->
        name :: (boundVars body)
    | _ -> []

open Env

let ops = ([ 
    "+", ( + ); 
    "-", ( - );
    "*", ( * );
    "/", ( / )
] |> Map.ofList)

/// Evaluate an expression `expr` with respect to an environment `env`
let rec eval expr env =
    match expr with
    | Number _  | Bool _ | Unit -> Val expr
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
        | Val (Bool b) ->
            if b then (eval e1 env) else (eval e2 env)
        | _ ->
            let msg =
                sprintf "expected condition to be a boolean in '(%s)'" (expr.ToString ())
            in raise (EvalError msg)
    | Fun _ ->
        close expr env
    | Apply (e1, e2) ->
        let arg = eval e2 env in
        match eval e1 env with
        | Closure ((Fun (name, body)), closedEnv) ->
            eval (body) (Map.add name arg closedEnv)
        | _ ->
            let msg = 
                sprintf "tried to apply a value to something that isn't a function in expression '%s'" (expr.ToString ())
            in
            raise (EvalError msg)            

and evalBinop (l, op, r) env = 
    let op_fn =
        (match (ops.TryFind op) with
        | Some f -> f 
        | None -> 
            let msg = sprintf "unrecognised operator '%s'" op in
            raise (EvalError msg))
    in
    match (evalNumber l env), (evalNumber r env) with
    | (Number x), (Number y) -> Val (Number (op_fn x y))
    | _ ->
        failwith "unreachable"

and evalNumber expr env =
    match eval expr env with
    | Val (Number n) -> Number n
    | _ -> 
        let msg = 
            sprintf "expected expression to reduce to a number: '%s'" (expr.ToString ()) in
        raise (EvalError msg)