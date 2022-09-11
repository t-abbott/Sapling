open System
open System.IO

open Incremental.Parse
open Incremental.Interp


let usage = "dotnet run -- <file>"

let run file = 
    let ast = parse (File.ReadAllText file) in
    let res = (Eval.run ast).Value in        
    // Console.WriteLine (ast.ToString ());
    Console.WriteLine (res.ToString ())

let printError (message : string) =
    Console.Error.WriteLine ("error: " + message)
    
[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        Repl.run ();
        0
    elif argv.Length > 1 || argv[0] = "--help" then
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
        
