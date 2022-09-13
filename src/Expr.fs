namespace Incremental.Expr

open System
open Incremental.Cell

/// Represents the name of a value
type Name = string

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
    | Dyn of Cell.T<Expr>
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
        | Dyn c -> c.body.ToString()
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
    | BuiltIn of Builtin

    override this.ToString() =
        match this with
        | Uninitialised -> "[uninitialised]"
        | Val e -> e.ToString()
        | Closure (_, _) ->
            // TODO fix closure pretty printing. `ToString` doesn't let
            // us pass the names of the parent variables the closure is
            // defined in so you can't avoid expanding recursive definitions
            "[closure]"
        | BuiltIn b -> b.ToString()

and Builtin =
    // TODO: make builtins take envvals as arguments
    { name: string
      body: Expr -> EnvVal }

    override this.ToString() = sprintf "[builtin %s]" this.name

    /// Read a number from stdin and return it as a `Number`
    static member getNum(_: Expr) : EnvVal =
        let input = Console.ReadLine()

        try
            let n = Double.Parse input
            Val(Number n)
        with :? FormatException ->
            raise (Exception("couldn't parse number " + input))

    /// Prints a value to the console
    static member print(e: Expr) : EnvVal =
        Console.WriteLine(e.ToString())
        Val Unit

    /// Returns a new builtin function that updates the dynamic cell `cell`
    static member set(e: Expr) : EnvVal =
        let cell =
            match e with
            | Dyn c -> c
            | _ -> raise (Exception "can't update a non-dynamic value")

        let name = "_doSet_" + string cell.id

        let f expr =
            cell.Set expr
            Val Unit in

        BuiltIn { name = name; body = f }

    static member functions =
        [ { name = "getNum"
            body = Builtin.getNum }
          { name = "print"; body = Builtin.print }
          { name = "set"; body = Builtin.set } ]
