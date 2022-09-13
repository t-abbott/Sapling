namespace Incremental.Cell

module Cell =
    type 'a T =
        {
            id: System.Guid

            /// The expression
            mutable body: 'a

            /// The cached value of this `Cell`
            mutable value: 'a option

            /// A list of `Cells` that `this` depends on
            mutable reads: 'a T list

            /// A list of `Cells` that depend on `this`
            mutable observers: 'a T list
        }

        ///
        static member fromValue x =
            { id = System.Guid.NewGuid()
              body = x
              value = None
              reads = []
              observers = [] }

        /// Remove `cell` as an observer of `observedCell`
        static member removeObserver cell observedCell =
            observedCell.observers <- List.filter (fun o -> o.id <> cell.id) observedCell.observers

        /// Invalidate `this` cell and it's dependencies
        member this.Invalidate() =
            List.iter (fun (o: 'a T) -> o.Invalidate()) this.observers
            List.iter (fun (o: 'a T) -> T<'a>.removeObserver this o) this.reads
            this.value <- None
            this.observers <- []
            this.reads <- []

        /// Change the body of `this`
        member this.Set expr =
            this.Invalidate()
            this.body <- expr
