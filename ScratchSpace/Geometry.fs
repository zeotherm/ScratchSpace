module Geometry

type OrderedPair = 
    { x : int; y : int}
    override m.ToString() = sprintf "(%d,%d)" m.x m.y
type Polygon = OrderedPair list

let makeOrderedPair elem = 
    match elem with 
    | i::j::[] -> { x = i; y = j}
    | _ -> failwith "Not a valid pair entry"

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

