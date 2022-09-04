module Incremental.Utils

open System.Linq

let without xs y = 
    List.filter (fun x -> x <> y) xs

let dedupe xs = 
    Set.toList (Set.ofList xs)

let union xs ys = 
    dedupe (xs @ ys)

module Extensions = 
    type Map<'a, 'b when 'a : comparison> with
        member this.Pairs = Seq.zip (this.Keys) (this.Values) |> Seq.toList

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
