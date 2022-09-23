open System
open System.IO

open Incremental.Utils
open Incremental.Parse
open Incremental.Interp


let usage = "dotnet run -- <file>"

let run file =
    let ast = parse (File.ReadAllText file) in
    let res = (Eval.run ast).Value in
    // Console.WriteLine (ast.ToString ());
    Console.WriteLine(res.ToString())

let printError (message: string) =
    Console.Error.WriteLine("error: " + message)

[<EntryPoint>]
let main argv =
    Log.setOutput Log.nullSink

    match argv.Length with
    | 0 ->
        Repl.run ()
        0
    | 1 when argv[0] = "--help" ->
        Console.Error.WriteLine usage
        1
    | 1 ->
        try
            run argv[0]
            0
        with
        | EvalError (message) ->
            printError message
            1
        | ex ->
            printError ex.Message
            1
    | _ ->
        Console.Error.WriteLine usage
        1
