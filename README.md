# Incremental

An interpreter for a toy self-adjusting ml-style programming languge.
Inspired by [mini-ml](https://dl.acm.org/doi/10.1145/319838.319847)
and Neel Krishnaswami's [blog post](https://semantic-domain.blogspot.com/2015/07/how-to-implement-spreadsheet.html).

## Dependencies

- `FsLexYacc`
- `Argu`

## Use

`dotnet run -- <file>`

https://github.com/fsprojects/FSharp.Data.Adaptive/discussions/102

# thoughts:

- the recursive definitions are handled means lots of `refs` everywhere, which isn't very nice
- algebraic effects could be a nice way of handling cell invalidation at the language level

## TODO

- make parser and lexer errors nice
- add multi-argument functions (sugared/core AST?)
- fix parser ambiguity errors
- write a test rig for the example files
- compile to C? -> lambda lifting
