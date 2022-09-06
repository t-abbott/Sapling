# Incremental

An interpreter for a toy self-adjusting ml-style programming languge.
Inspired by [mini-ml](https://dl.acm.org/doi/10.1145/319838.319847)
and Neel Krishnaswami's [blog post](https://semantic-domain.blogspot.com/2015/07/how-to-implement-spreadsheet.html).

## Dependencies

- `FsLexYacc`

## Use

`dotnet run -- <file>`

## TODO

- fix parser ambiguity errors
- make application have higher precedence than addition etc
- make closure ToString nice
- add a test rig on the example files
