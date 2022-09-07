# Incremental

An interpreter for a toy self-adjusting ml-style programming languge.
Inspired by [mini-ml](https://dl.acm.org/doi/10.1145/319838.319847)
and Neel Krishnaswami's [blog post](https://semantic-domain.blogspot.com/2015/07/how-to-implement-spreadsheet.html).

## Dependencies

- `FsLexYacc`

## Use

`dotnet run -- <file>`

## TODO

- add multi-argument functions (sugared/core AST?)
- fix parser ambiguity errors
- make application have higher precedence than addition (and other things of that sort)
- make closure ToString nice
- write a test rig for the example files
- compile to C? -> lambda lifting
