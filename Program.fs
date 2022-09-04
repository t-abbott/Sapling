open System
open System.IO
open FSharp.Text.Lexing

open Incremental
open Incremental.Interp
open Incremental.Utils

let usage = "dotnet run -- <file>"

let parse (input : string) : Expr.T =
    let lexbuf = LexBuffer<char>.FromString input in
    Parser.parse Lexer.tokenize lexbuf

let run file = 
    let ast = parse (File.ReadAllText file) in
    let res = eval ast Map.empty in        
    Console.WriteLine (ast.ToString ());
    Console.WriteLine (res.ToString ());
 
[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        Console.Error.WriteLine usage;
        1
    else
        try
            run argv[0];
            0
        with
        | EvalError(message, expr) ->
            let estring = 
                match expr with
                | Some(e) -> "in expression " + e.ToString ()
                | None -> ""
            in
            printfn "error: %s %s\n" message estring;
            1
        | :? Exception as ex ->
            printfn "error: %s" (ex.Message); 
            1
        
