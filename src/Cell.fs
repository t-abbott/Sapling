namespace Incremental.Cell

open Incremental.Utils

module Cell =
    type 'a T =
        {
            id: System.Guid

            /// A dynamic expression
            mutable body: 'a

            /// The cached value of this `Cell`
            mutable value: 'a option

            /// A list of `Cells` that `this` depends on
            mutable reads: 'a T list

            /// A list of `Cells` that depend on `this`
            mutable observers: 'a T list
        }

        /// Remove `cell` as an observer of `observedCell`
        static member removeObserver cell observedCell =
            observedCell.observers <- List.filter (fun o -> o.id <> cell.id) observedCell.observers

        /// Set a cell `observer` as an observer of `this`
        member this.AddObserver observer =
            this.observers <- observer :: this.observers

        /// Register the cell `this` as an observer of the cells `observedCells`
        member this.RegisterObservers observedCells =
            List.iter (fun (c: T<'a>) -> c.AddObserver this) observedCells

        /// Invalidate `this` cell and it's dependencies
        member this.Invalidate() =
            Log.debug (sprintf "invalidated cell '%s'" (string this.id))

            // invalidate cells that depend on `this`
            List.iter (fun (o: 'a T) -> o.Invalidate()) this.observers

            // remove `this` as an observer from other cells
            List.iter (fun (o: 'a T) -> T<'a>.removeObserver this o) this.reads

            this.value <- None
            this.observers <- []
            this.reads <- []

        /// Change the body of `this`
        member this.Set expr =
            this.Invalidate()
            this.body <- expr

    /// Create a new `Cell.T` from a value `x`
    let from x =
        { id = System.Guid.NewGuid()
          body = x
          value = None
          reads = []
          observers = [] }

    let equals c1 c2 = c1.id = c2.id
