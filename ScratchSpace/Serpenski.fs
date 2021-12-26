
module Serpenski
open System

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
