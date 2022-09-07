module Incremental.Parse

open Incremental
open FSharp.Text.Lexing

let parse (input : string) : Expr.T =
    let lexbuf = LexBuffer<char>.FromString input in
    Parser.parse Lexer.tokenize lexbuf