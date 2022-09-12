namespace Incremental

module Expr =
    ///
    type Name = string

    ///
    type Op =
        | LessThan
        | GreaterThan
        | Equals
        | And
        | Or
        | Add
        | Sub
        | Mult
        | Div
        | Mod

        override this.ToString() =
            match this with
            | LessThan -> "<"
            | GreaterThan -> ">"
            | Equals -> "="
            | And -> "and"
            | Or -> "or"
            | Add -> "+"
            | Sub -> "-"
            | Mult -> "*"
            | Div -> "/"
            | Mod -> "%"

        member this.hasNumericArgs =
            match this with
            | And
            | Or -> false
            | _ -> true

        member this.asBooleanArgs =
            match this with
            | And
            | Or -> true
            | _ -> false

    ///
    type T =
        | Unit
        | Bool of bool
        | Number of float
        | Var of Name
        | LetIn of Name * T * T
        | Binop of T * Op * T
        | IfThen of T * T * T
        | Fun of Name * T
        | Apply of T * T

        override this.ToString() =
            match this with
            | Unit -> "()"
            | Bool b -> string b
            | Number n -> string n
            | Var v -> v
            | LetIn (name, e, body) -> sprintf "let %s = %s in %s" name (e.ToString()) (body.ToString())
            | Binop (l, op, r) -> sprintf "(%s %s %s)" (l.ToString()) (op.ToString()) (r.ToString())
            | IfThen (cond, e1, e2) ->
                sprintf "(if %s then %s else %s)" (cond.ToString()) (e1.ToString()) (e2.ToString())
            | Fun (name, body) -> sprintf "(fun %s -> %s)" name (body.ToString())
            | Apply (e1, e2) -> sprintf "(%s %s)" (e1.ToString()) (e2.ToString())

        member this.is_value =
            match this with
            | Unit
            | Bool _
            | Number _ -> true
            | _ -> false

    ///
    and Cell =
        {
            id: System.Guid

            /// The expression
            mutable body: T

            /// The cached value of this `Cell`
            mutable value: T option

            /// A list of `Cells` that `this` depends on
            mutable reads: Cell list

            /// A list of `Cells` that depend on `this`
            mutable observers: Cell list
        }

        static member fromExpr e =
            { id = System.Guid.NewGuid()
              body = e
              value = None
              reads = []
              observers = [] }

        /// Remove `cell` as an observer of `observedCell`
        static member removeObserver cell observedCell =
            observedCell.observers <- List.filter (fun o -> o.id <> cell.id) observedCell.observers

        /// Invalidate `this` cell and it's dependencies
        member this.Invalidate() =
            List.iter (fun (o: Cell) -> o.Invalidate()) this.observers
            List.iter (fun (o: Cell) -> Cell.removeObserver this o) this.reads
            this.value <- None
            this.observers <- []
            this.reads <- []

        /// Get the current value of `this`
        member this.Get =
            match this.value with
            | Some e -> e
            | None -> failwith "not implemented"

        /// Change the body of `this`
        member this.Set expr =
            this.Invalidate()
            this.body <- expr
