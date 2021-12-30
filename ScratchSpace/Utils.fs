module ScratchUtils

open System
open Geometry

let printList l = 
    l |> Seq.iter (printf "%A\n")

let readListFromInput converter = 
    let rec readHelper entries = 
        match Console.ReadLine() with 
        | "" | null -> List.rev entries
        | e -> readHelper ((converter e) :: entries)
    readHelper []

let asInts = fun e -> e |> int
let asDoubles = fun e -> e |> double
let asIntList = fun (e:string) -> e.Split(' ') |> Array.toList |> List.map Int32.Parse 
let asStrings = id
let asOrderedPairs = fun e -> e |> asIntList |> makeOrderedPair

