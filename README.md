# Sapling

An interpreter for a toy self-adjusting programming languge
inspired by Neel Krishnaswami's [blog post](https://semantic-domain.blogspot.com/2015/07/how-to-implement-spreadsheet.html).

## Dependencies

- `FsLexYacc`
- `Argu`

## Use

Start the repl[^1] with

```
dotnet run
```

or execute a file by running

```
dotnet run -- <file>
```

~~In either case ignore the shift/reduce error vomit.~~

Some example programs are listed in `examples/`.
Run with the `--help` flag to see usage information.
Alternatively use `dotnet build` to create a dll in `bin/`.

# thoughts:

- the recursive definitions are handled means lots of `refs` everywhere, which isn't very nice
- algebraic effects could be a nice way of handling cell invalidation at the language level

---

[^1]: example:

    ```
    $ dotnet run
    > let f = (fun x -> x + 1) in f -4.01
    -3.01
    > .exit
    ```
