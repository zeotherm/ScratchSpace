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

let removeOddIndicies list =
    List.mapi (fun i x -> (i+1, x)) list |> List.filter (fun x -> fst x % 2 = 0) |> List.map snd

let removeOddIndicies2 list =
    List.mapi (fun i x -> (i+1, x)) list |> List.fold (fun acc elem -> if fst elem % 2 = 0 then (snd elem)::acc else acc) [] |> List.rev

let rec removeOddIndicies3 list =
    match list with
    | _::x::xs -> x::(removeOddIndicies3 xs)
    | _ -> []

let createListOfLengthN N = 
    List.init N (fun i -> i)

[<EntryPoint>]
let main argv = 
    let N = System.Console.ReadLine() |> int
    //readListFromInput() |> filter_ (fun x -> x < N) |> printList
    printfn "%A" (createListOfLengthN N)
    0 // return an integer exit code
