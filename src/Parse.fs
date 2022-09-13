module Incremental.Parse

open FSharp.Text.Lexing
open Incremental.Expr

let parse (input: string) : Expr =
    let lexbuf = LexBuffer<char>.FromString input in Parser.parse Lexer.tokenize lexbuf
