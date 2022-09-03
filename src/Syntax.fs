namespace Incremental

module Syntax = 
    type Name = string

    type Expr = 
        | Number of float
        | Binop of Expr * Name * Expr
        
        with
            override this.ToString () = 
                match this with
                | Number n -> string n
                | Binop (l, op, r) ->
                    sprintf "(%s %s %s)" (l.ToString ()) op (r.ToString ())

