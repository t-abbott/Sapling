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

let printError (message : string) =
    Console.Error.WriteLine ("error: " + message)
    
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
        | EvalError(message) ->
            printError message;
            1
        | ex ->
            printError ex.Message; 
            1
        
