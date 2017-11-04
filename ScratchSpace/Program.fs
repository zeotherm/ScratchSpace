open System

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let rec printHelloWorld n = 
    match n with 
    | 0 -> "\n"
    | i when i > 0 -> "Hello World\n" + printHelloWorld (i-1)
    | _ -> failwith "Number must be positive"

let printList l = 
    l |> Seq.iter (printf "%A\n")

let readListFromInput() = 
    let rec readHelper entries = 
        match Console.ReadLine() with 
        | "" | null -> List.rev entries
        | e -> readHelper ((e |> int) :: entries)
    readHelper []

[<EntryPoint>]
let main argv = 
    let N = System.Console.ReadLine() |> int
    readListFromInput() |> List.collect (fun x -> List.replicate N x) |> printList
    0 // return an integer exit code
