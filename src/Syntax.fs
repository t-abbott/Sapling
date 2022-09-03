module Incremental.Syntax

type Name = string

type Expr = 
    | Number of float
    | Binop of Expr * Name * Expr

