namespace Incremental

module Expr = 
    type Name = string

    type T = 
        | Number of float
        | Var of Name
        | Binop of T * Name * T
        | LetIn of Name * T * T
        | Fun of Name * T
        | Apply of T * T

        with
            override this.ToString () = 
                match this with
                | Number n -> string n
                | Var v -> v
                | Binop (l, op, r) ->
                    sprintf "(%s %s %s)" (l.ToString ()) op (r.ToString ())
                | LetIn (name, e, body) ->
                    sprintf "let %s = %s in %s" name (e.ToString ()) (body.ToString ())
                | Fun (name, body) ->
                    sprintf "(fun %s -> %s)" name (body.ToString ())
                | Apply (e1, e2) ->
                    sprintf "(%s %s)" (e1.ToString ()) (e2.ToString ())

    let is_value = function 
        | Number _ -> true
        | _ -> false
