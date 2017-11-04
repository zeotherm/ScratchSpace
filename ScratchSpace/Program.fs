open System

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let printList l = 
    l |> Seq.iter (printf "%A\n")

let readListFromInput() = 
    let rec readHelper entries = 
        match System.Console.ReadLine() with 
        | "" | null -> List.rev entries
        | e -> readHelper ((e |> int) :: entries)
    readHelper []

let rec printHelloWorld n = 
    match n with 
    | 0 -> "\n"
    | i when i > 0 -> "Hello World\n" + printHelloWorld (i-1)
    | _ -> failwith "Number must be positive"

let filter_ f xs =
    let rec filterhelper acc list =
        match list with
        | [] -> List.rev acc 
        | h :: t when f h -> filterhelper (h::acc) t 
        | _ :: t -> filterhelper acc t
    filterhelper [] xs

let reverse_ xs =
    let rec reverseaux acc list = 
        match list with
        | [] -> acc
        | h :: t -> reverseaux (h::acc) t
    reverseaux [] xs

let sumOdds list = 
    list |> List.filter (fun x -> not (x % 2 = 0)) |> List.sum

let length() =
    List.fold (fun acc _ -> acc + 1) 0
[<EntryPoint>]
let main argv = 
    //let N = System.Console.ReadLine() |> int
    //readListFromInput() |> filter_ (fun x -> x < N) |> printList
    printfn "%d" (readListFromInput() |> length())
    0 // return an integer exit code
