open System
open System.IO

open Argu

open Incremental.Utils
open Incremental.Parse
open Incremental.Interp

///
type Args =
    | [<MainCommand>] File of string
    | [<AltCommandLine("-v")>] Verbose

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | File _ -> "specify a file to execute"
            | Verbose -> "toggle verbose output (logging)"

let argParser = ArgumentParser.Create<Args>()
let usage = argParser.PrintUsage()

let run file =
    let ast = parse (File.ReadAllText file)
    let res = Eval.run ast
    Console.WriteLine(res.ToString())

let printError (message: string) =
    Console.Error.WriteLine("error: " + message)

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        Repl.run ()
        0
    else
        try
            let args = argParser.Parse argv

            match args.Contains Verbose with
            | true -> Log.setOutput Log.stderr
            | false -> Log.setOutput Log.nullSink

            run (args.GetResult File)
            0
        with
        | :? ArguParseException as ex ->
            if ex.ErrorCode = ErrorCode.HelpText then
                Console.WriteLine usage
                0
            else
                printError ex.Message
                1

        | EvalError (message) ->
            printError message
            1
        | ex ->
            printError ex.Message
            1
