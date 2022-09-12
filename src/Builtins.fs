namespace Incremental.Interp

open System
open Incremental
open Incremental.Expr

module Builtin =
    /// Represents a built-in function
    type T =
        { name: string
          body: Expr.T -> Expr.T }

        override this.ToString() = sprintf "[builtin %s]" this.name

    /// Read a number from stdin and return it as a `Number`
    let getNum (_: Expr.T) : Expr.T =
        let input = Console.ReadLine()

        try
            let n = Double.Parse input
            Number n
        with :? FormatException ->
            raise (Exception("couldn't parse number " + input))

    /// Print a value to the console
    let print (e: Expr.T) =
        Console.WriteLine(e.ToString())
        Unit

    let functions =
        [ { name = "getNum"; body = getNum }; { name = "print"; body = print } ]
