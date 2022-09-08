module Incremental.Utils

open System.Linq

type Result<'a, 'b> = 
    | Ok of 'a
    | Error of 'b

let proj2 f (x, y) = f x, f y
let proj3 f (x, y, z) = f x, f y, f z

let without xs y = 
    List.filter (fun x -> x <> y) xs

let dedupe xs = 
    Set.toList (Set.ofList xs)

let union xs ys = 
    dedupe (xs @ ys)

module Extensions =
    type Map<'a, 'b when 'a : comparison> with
        member this.Pairs = Seq.zip (this.Keys) (this.Values) |> Seq.toList

        member this.Without key = 
            this.Remove key

        /// Take the union of two maps `this` and `other`, with elements in
        /// `other` taking precedence
        member this.Union other =
            Map.foldBack Map.add other this

        member this.Difference (other : Map<'a, 'b>) =
            let xs = List.filter (fun (key, _) -> not (other.ContainsKey key)) this.Pairs in
            let ys = List.filter (fun (key, _) -> not (this.ContainsKey key)) other.Pairs in
            Map.ofList (xs @ ys) 

        /// Return the subset of `this` containing only the keys in `other` 
        member this.Intersection (keys : 'a list) = 
            List.filter (fun (key, _) -> keys.Contains key) this.Pairs |> Map.ofList
