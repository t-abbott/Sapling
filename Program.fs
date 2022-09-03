open System
open System.IO
open FSharp.Text.Lexing

open Incremental.Syntax
open Incremental.Interp

let usage = "dotnet run -- <file>"

let parse (input : string) : Expr =
    let lexbuf = LexBuffer<char>.FromString input in
    Parser.parse Lexer.tokenize lexbuf

[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        Console.Error.WriteLine usage;
        1
    else 
        let ast = parse (File.ReadAllText argv[0]) in
        let res = eval ast Map.empty in
        Console.WriteLine (res.ToString ());
        0