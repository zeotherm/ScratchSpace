﻿open System.Security.AccessControl
open System
// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let printList l = 
    l |> Seq.iter (printf "%A\n")

let readListFromInput converter = 
    let rec readHelper entries = 
        match System.Console.ReadLine() with 
        | "" | null -> List.rev entries
        | e -> readHelper ((converter e) :: entries)
    readHelper []

let asInts = fun e -> e |> int
let asDoubles = fun e -> e |> double
let asIntList = fun (e:string) -> e.Split(' ') |> Array.toList |> List.map System.Int32.Parse 
let asStrings = id

let rec printHelloWorld n = 
    match n with 
    | 0 -> "\n"
    | i when i > 0 -> "Hello World\n" + printHelloWorld (i-1)
    | _ -> failwith "Number must be positive"

let filter_ pred xs =
    let rec filterhelper acc list =
        match list with
        | [] -> List.rev acc 
        | h :: t when pred h -> filterhelper (h::acc) t 
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
    //List.init N (fun i -> i)
    [0 .. N]

let factorial n =
    let rec factaux i acc = 
        match i with 
        | 0 | 1 -> acc
        | _ -> factaux (i-1) (acc*i)
    factaux n 1

let exp x : double =
    [0 .. 9] |> List.map (fun n -> (pown x n) / ((factorial n) |> double)) |> List.sum 

type Polynomial = 
    { coeff_exp_pair : (int*int) list}
    member private this.printPolyElement pe n = 
        let sign = if (fst pe) < 0 then "-" else (if n <> 0 then "+" else "")
        match pe with 
        | (0, _) -> ""
        | (c, 0) -> sprintf "%s%d" sign (abs c)
        | (c, 1) when (abs c) <> 1 -> sprintf "%s%dx" sign (abs c)
        | (1, 1) -> "+x"
        | (-1, 1) -> "-x"
        | (1, e) when e <> 0 -> sprintf "%sx^%d" sign e
        | (c, e) -> sprintf "%s%dx^%d" sign (abs c) e
    override m.ToString() = 
        let rec printHelper e n = 
            match e with
            | t::[] -> m.printPolyElement t n
            | h::t -> m.printPolyElement h n + printHelper t (n+1)
            | _ -> "\n"
        printHelper m.coeff_exp_pair 0

type Bounds = { lower : double; upper: double}
let makePoly coeffs exps = {coeff_exp_pair = List.zip coeffs exps |> List.sortByDescending (fun ce -> snd ce)}
let makeBounds (b:int list) = {lower = double b.[0]; upper = double b.[1]}
let eval (p:Polynomial) (x:double) = p.coeff_exp_pair |> List.map (fun ce -> double (fst ce) * (pown x (snd ce))) |> List.sum

let integrate_aux (f: double -> double) (w:double->double) (bounds:Bounds) dx = 
    let N = ((int bounds.upper) - (int bounds.lower)) * (int (1.0/dx))
    let x_is = List.init N (fun i -> bounds.lower + dx*(double i + 1.0))
    x_is |> List.map (f >> w) |> List.sum

let integrate f bounds dx = 
    integrate_aux f (fun f_x -> f_x*dx) bounds dx

let volumeOfRevolution f bounds dx =
    integrate_aux f (fun f_x -> System.Math.PI*f_x**2.0*dx) bounds dx

let integrationProblem() = 
    let inp = readListFromInput asIntList
    let p = makePoly inp.[0] inp.[1]
    let bounds = makeBounds inp.[2]
    printfn "%O" p
    printfn "%f @ lower" (eval p bounds.lower)
    printfn "%f @ upper" (eval p bounds.upper)
    let deltax = 0.001
    let f = (eval p)
    printfn "%f" (integrate f bounds deltax)
    printfn "%f" (volumeOfRevolution f bounds deltax)

type OrderedPair = 
    { x : int; y : int}
    override m.ToString() = sprintf "(%d,%d)" m.x m.y
type TestCase = OrderedPair list
type Polygon = OrderedPair list
let makeOrderedPair elem = 
    match elem with 
    | i::j::[] -> { x = i; y = j}
    | _ -> failwith "Not a valid pair entry"

let makeTestCase inp = 
    inp |> List.fold (fun acc elem -> (makeOrderedPair elem)::acc) []

let rec processTestCaseInput tci = 
    match tci with
    | h :: t -> let num_elem = List.head h
                makeTestCase(List.take num_elem t) :: processTestCaseInput (List.skip num_elem t)
    | _ -> []

let isValidFunction tc = 
    let testOneX tc = 
        let ys = List.fold (fun (acc: int list) (elem:OrderedPair) -> elem.x :: acc) [] tc
        let h = List.head ys
        if List.forall ((<>) h) (List.tail ys) then true else false
    let xs = List.groupBy (fun e -> e.x) tc |> List.map snd
    if (List.map testOneX xs |> List.fold (fun acc elem -> acc && elem) true ) then "YES" else "NO"

let distance (p:OrderedPair) (q:OrderedPair) = 
    System.Math.Sqrt(pown (double p.x - double q.x) 2 + pown (double p.y - double q.y) 2)

let PolygonCloser (p:Polygon) =
    if List.head p <> List.last p then 
        (List.head p)::(List.rev p)
    else 
        p

let perimeter (p:Polygon) = 
    let rec perimeter_helper p_aug = 
        match p_aug with 
        | p::q::t -> distance p q + perimeter_helper (q::t)
        | _ -> 0.0
    perimeter_helper (PolygonCloser p)

let area (p:Polygon) =
    let rec area_helper (pgon:Polygon) =
        match pgon with
        | p::q::t -> double (p.x*q.y - q.x*p.y) + area_helper (q::t)
        | _ -> 0.0
    0.5 * System.Math.Abs (area_helper (PolygonCloser p))

let rec gcd a b =
    match b with 
    | 0 -> a
    | _ -> gcd b (a % b)

let modvalBI = System.Numerics.BigInteger (pown 10 8 + 7)
let modval = pown 10 8 + 7

let memoize f =
    let cache = ref Map.empty
    fun x ->
        match (!cache).TryFind(x) with
        | Some res -> res
        | None ->
             let res = f x
             cache := (!cache).Add(x,res)
             res

let fibonacci N =
    let rec fibhelper n a b =
        match n with 
        | 0 -> a
        | 1 -> b
        | _ -> fibhelper (n-1) (b%modval) ((a%modval+b%modval)%modval)
    fibhelper (N-1) 0 1

let rec Pascal n r =
    match (n, r) with 
    | (_,0) -> 1
    | (x, y) when x = y -> 1
    | (row, col) -> Pascal (row-1) col + Pascal (row-1) (col-1)

let PascalM = memoize Pascal

let PascalRow r =
    [0..r] |> List.map (PascalM r) 

let printListOfLists() =
    List.map (fun l -> List.iter ( fun i -> printf "%d " i) l 
                       printfn "") 
                        
let collate p q = 
    let rec collate_aux acc ps qs =
        match ps, qs with
        | [], [] -> acc
        | ph::pt, qh::qt -> collate_aux (qh::ph::acc) pt qt
        | _ -> failwith "different length strings"
    collate_aux [] p q |> List.rev |> List.toArray |> System.String

let swapadjacent xs =
    let rec swapadj_aux acc ys =
        match ys with
        | p::q::t -> swapadj_aux (p::q::acc) t
        | _ -> acc
    swapadj_aux [] (List.ofSeq xs) |> List.rev |> List.toArray |> System.String

let pack xs =
    let rec pack_aux acc x = 
        match x with 
        | [] -> acc 
        | h::t ->
            match acc with
            | [] -> pack_aux [(h, 1)] t
            | (i, count) :: ta ->
                if h = i then
                    pack_aux ((i, count+1) :: ta) t
                else
                    pack_aux ((h, 1) :: (i, count) :: ta) t
    pack_aux [] xs |> List.rev

type PackStruct = (char*int) list

let printPS (ps:PackStruct) =
    let print_aux ci =
        if (snd ci) <> 1 then
            printf "%c%i" (fst ci) (snd ci)
        else
            printf "%c" (fst ci)

    ps |> ((List.iter print_aux) >> (fun x -> printfn "") )
                    
let rotations (S:string) =
    let N = S.Length
    let sa = [for c in S -> c] |> List.toArray
    let indicies = [0..N-1]
    let rotations = [for i in indicies do yield [for j in indicies -> (i+j+1)%N]]
    let f = fun r -> [for idx in r -> sa.[idx]] |> List.toArray |> System.String
    List.map f rotations |> List.iter (printf "%s ") |> ignore
    printfn ""

let higherFibs = Seq.unfold (fun (a,b) -> 
                                        let bmv = b%modval
                                        let next = (a + bmv)%modval
                                        Some(next, (bmv, next))) (0,1)
let fib01 = seq {0 .. 1} 
let fib01I = [0I; 1I] |> List.toSeq
let fibSeq = Seq.append fib01 higherFibs

// Serpinski Triangle
type Tri = int * int * int

let step (ts : Tri list) =
  [ for t in ts do
      let x, y, n = t
      yield! [ x, y, n-1
               x-(pown 2 (n-2)), y + (pown 2 (n-2)), n-1
               x+(pown 2 (n-2)), y + (pown 2 (n-2)), n-1 ] ]

// drawing canvas of width * height * rows
type Canvas = int * int * string list

// point on the canvas
type Point = int * int

// draws horisontal line from x of length n in row
let drawhlr (row : string) (x : int) (n : int) =
  let len = row.Length
  if x < len then
    let s1 = row.Substring(0, x)
    let p = x + n
    if p < len then
      let s2 = String('1', n)
      let s3 = row.Substring(p)
      sprintf "%s%s%s" s1 s2 s3
    else
      let s2 = String('1', len - x)
      sprintf "%s%s" s1 s2
  else row

// draws horisontal line from p of length n on the canvas
let drawhl (c : Canvas) (p : Point) (n : int) =
  let w, h, rows = c
  let x, y = p
  let newr = rows
             |> List.mapi (fun i row ->
                             if y = i then drawhlr row x n  
                             else row)
  w, h, newr

// draws triangle on canvas
let draw (c : Canvas) (t : Tri) =
  let x, y, n = t
  let lines = [ for i in 1..(pown 2 (n-1)) do
                  yield (x-(i-1), y+(i-1)), i*2-1 ]
  lines
  |> List.fold (fun c v -> drawhl c (fst v) (snd v)) c

let render (c : Canvas) =
  let _, _, rows = c
  String.Join("\n", rows)



[<EntryPoint>]
let main argv =
    let N = System.Console.ReadLine() |> int
    [1..N]
    |> List.fold (fun ts _ -> step ts) [ (31, 0, 6) ]
    |> List.fold (fun c t -> draw c t) (63, 32, [ for i in 0..31 do yield String('_', 63) ])
    |> render
    |> printf "%s"
    0 // return an integer exit code