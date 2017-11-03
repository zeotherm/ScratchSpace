// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let rec printHelloWorld n = 
    match n with 
    | 0 -> "\n"
    | i when i > 0 -> "Hello World\n" + printHelloWorld (i-1)
    | _ -> failwith "Number must be positive"

[<EntryPoint>]
let main argv = 
    let a = System.Console.ReadLine() |> int
    printfn "%s" (printHelloWorld a)
    0 // return an integer exit code
