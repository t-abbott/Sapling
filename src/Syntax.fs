namespace Incremental

module Expr = 
    type Name = string

    type T =
        | Unit 
        | Bool of bool
        | Number of float
        | Var of Name
        | LetIn of Name * T * T
        | Binop of T * Name * T
        | IfThen of T * T * T
        | Fun of Name * T
        | Apply of T * T

        with
            override this.ToString () = 
                match this with
                | Unit -> "()"
                | Bool b -> string b
                | Number n -> string n
                | Var v -> v
                | LetIn (name, e, body) ->
                    sprintf "let %s = %s in %s" name (e.ToString ()) (body.ToString ())
                | Binop (l, op, r) ->
                    sprintf "(%s %s %s)" (l.ToString ()) op (r.ToString ())
                | IfThen (cond, e1, e2) ->
                    sprintf "(if %s then %s else %s)" (cond.ToString ()) (e1.ToString ()) (e2.ToString ())
                | Fun (name, body) -> 
                    sprintf "(fun %s -> %s)" name (body.ToString ())
                | Apply (e1, e2) ->
                    sprintf "(%s %s)" (e1.ToString ()) (e2.ToString ())

    let is_value = function 
        | Number _ | Bool _ | Unit -> true
        | _ -> false
