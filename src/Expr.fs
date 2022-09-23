namespace Incremental.Expr

open System
open Incremental.Cell
open Incremental.Utils


/// Represents the name of a value
type Name = string

type BuiltinErrMsg = { name: Name; message: string }
exception BuiltinError of BuiltinErrMsg

/// Binary operators
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

    member this.hasBooleanArgs =
        match this with
        | And
        | Or
        | Equals -> true
        | _ -> false

///
type Expr =
    | Unit
    | Bool of bool
    | Number of float
    | Var of Name
    | LetIn of Name * Expr * Expr
    | LetDynIn of Name * Expr * Expr
    | Binop of Expr * Op * Expr
    | IfThen of Expr * Expr * Expr
    | Fun of Name * Expr
    | Apply of Expr * Expr

    override this.ToString() =
        match this with
        | Unit -> "()"
        | Bool b -> string b
        | Number n -> string n
        | Var v -> v
        | LetIn (name, e, body) -> sprintf "let %s = %s in %s" name (e.ToString()) (body.ToString())
        | LetDynIn (name, e, body) -> sprintf "let dyn %s = %s in %s" name (e.ToString()) (body.ToString())
        | Binop (l, op, r) -> sprintf "(%s %s %s)" (l.ToString()) (op.ToString()) (r.ToString())
        | IfThen (cond, e1, e2) -> sprintf "(if %s then %s else %s)" (cond.ToString()) (e1.ToString()) (e2.ToString())
        | Fun (name, body) -> sprintf "(fun %s -> %s)" name (body.ToString())
        | Apply (e1, e2) -> sprintf "(%s %s)" (e1.ToString()) (e2.ToString())

    member this.is_value =
        match this with
        | Unit
        | Bool _
        | Number _ -> true
        | _ -> false

/// <summary>Represents an environment mapping names to their values</summary>
/// <remark>The actual values are mutable because of how we implement recursion</remark>
and Env =
    | Env of Map<Name, EnvVal ref>

    static member empty = Map.empty

    /// Initial environment containing all built-in functions and variables
    static member initial =
        Builtin.functions
        |> List.map (fun b -> b.name, ref (BuiltIn b))
        |> Map.ofList
        |> Env

    /// Create a closure from an expression and environment
    static member close expr env = Closure(expr, env)

/// Expression values
and EnvVal =
    | Uninitialised
    | Val of Expr
    | Closure of Expr * Env
    | Dyn of Cell.T<EnvVal> * Env
    | BuiltIn of Builtin

    override this.ToString() =
        match this with
        | Uninitialised -> "[uninitialised]"
        | Val e -> e.ToString()
        | Closure (_, _) -> "[closure]"
        | Dyn (_, _) -> "[dynamic]"
        | BuiltIn b -> b.ToString()

and Builtin =
    { name: string
      body: EnvVal ref -> Env -> EnvVal ref }

    override this.ToString() = sprintf "[builtin %s]" this.name

    /// Read a number from stdin and return it as a `Number`
    static member getNum _ _ =
        let input = Console.ReadLine()

        try
            let n = Double.Parse input
            ref (Val(Number n))
        with :? FormatException ->
            raise (
                BuiltinError
                    { name = "getNum"
                      message = "couldn't parse input '" + input + "'" }
            )

    /// Prints a value to the console
    static member print (expr: EnvVal ref) _ =
        Console.WriteLine(expr.Value.ToString())
        ref (Val Unit)

    /// Returns a new builtin function that modifies the dynamic cell `cell`
    static member set (value: EnvVal ref) _ =
        let cell =
            match value.Value with
            | Dyn (c, _) -> c
            | _ ->
                raise (
                    BuiltinError
                        { name = "set"
                          message = "can't update a non-dynamic value" }
                )

        let fname = "_doSet_" + string cell.id

        let doSet (newVal: EnvVal ref) (env: Env) : EnvVal ref =
            Log.debug (sprintf "updated cell '%s'" (string cell.id))

            cell.Set(newVal.Value)
            value.Value <- Dyn(cell, env)
            ref (Val Unit)

        ref (BuiltIn { name = fname; body = doSet })

    static member functions =
        [ { name = "getNum"
            body = Builtin.getNum }
          { name = "print"; body = Builtin.print }
          { name = "set"; body = Builtin.set } ]
