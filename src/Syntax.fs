namespace Incremental

module Expr =
    type Name = string

    type Op =
        | LessThan
        | GreaterThan
        | Equals
        | And
        | Or
        | Add
        | Sub
        | Mult
        | Div
        | Mod

        override this.ToString() =
            match this with
            | LessThan -> "<"
            | GreaterThan -> ">"
            | Equals -> "="
            | And -> "and"
            | Or -> "or"
            | Add -> "+"
            | Sub -> "-"
            | Mult -> "*"
            | Div -> "/"
            | Mod -> "%"

        member this.hasNumericArgs =
            match this with
            | And
            | Or -> false
            | _ -> true

        member this.asBooleanArgs =
            match this with
            | And
            | Or -> true
            | _ -> false


    type T =
        | Unit
        | Bool of bool
        | Number of float
        | Var of Name
        | LetIn of Name * T * T
        | Binop of T * Op * T
        | IfThen of T * T * T
        | Fun of Name * T
        | Apply of T * T

        override this.ToString() =
            match this with
            | Unit -> "()"
            | Bool b -> string b
            | Number n -> string n
            | Var v -> v
            | LetIn (name, e, body) -> sprintf "let %s = %s in %s" name (e.ToString()) (body.ToString())
            | Binop (l, op, r) -> sprintf "(%s %s %s)" (l.ToString()) (op.ToString()) (r.ToString())
            | IfThen (cond, e1, e2) ->
                sprintf "(if %s then %s else %s)" (cond.ToString()) (e1.ToString()) (e2.ToString())
            | Fun (name, body) -> sprintf "(fun %s -> %s)" name (body.ToString())
            | Apply (e1, e2) -> sprintf "(%s %s)" (e1.ToString()) (e2.ToString())

        member this.is_value =
            match this with
            | Unit
            | Bool _
            | Number _ -> true
            | _ -> false
