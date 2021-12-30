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

let findP0 (p: Polygon) = 
    let compareFunc (a: OrderedPair) (b: OrderedPair): int = 
        if a.y < b.y then -1 else
        if a.y > b.y then 1 else
        if a.x < b.x then -1 else
        if a.x > b.x then 1 else
        0
    let sorted = List.sortWith compareFunc p

    sorted.Head

let cosPolarAngle (P0: OrderedPair) (p: OrderedPair): double = 
    // cos Theta = |u . v|/(||u||*||v||)
    // u = [1, 0]
    // v = [p.x - P0.x, p.y - P0.y]
    // u.v = 1 * (p.x - P0.x) + 0 * (p.y - P0.y) = (p.x - P0.x)
    // ||u|| = 1
    // ||v|| = distance(P0, p)
    // cos(Theta) = (p.x-P0.x)/(distance(P0,p))
    double(p.x - P0.x)/(distance P0 p)


let sortByPolarAngle (p:Polygon) :Polygon =     
    
    let tagPolarAngleDist (p0: OrderedPair) : OrderedPair -> OrderedPair * double * double =
        fun p1 -> (p1, cosPolarAngle p0 p1, distance p0 p1) 

    let P0 = findP0 p
    let rest = List.filter (fun p1 -> p1 <> P0) p
    
    let temp1 = List.map (tagPolarAngleDist P0) rest
    let temp2 = List.groupBy (fun p -> let (_, cos_theta, _) = p    
                                       cos_theta) temp1
    let temp2a = List.sortBy (fun l -> fst l) temp2
    let temp3 = List.map (fun t -> List.maxBy (fun elem -> let (_, _, dist) = elem
                                                           dist) 
                                              (snd t)) temp2a
    let temp4 = List.map (fun e -> let (v, _, _) = e
                                   v) temp3
    let temp5 = List.rev temp4


    let ps = List.map (tagPolarAngleDist P0) rest                // tag with polar angle and distance from P0
             |> List.groupBy (fun p -> let (_, cos_theta, _) = p    
                                       cos_theta)                    // groupBy polar angle
             |> List.sortBy (fun l -> fst l)                         // sort by Polar angle
             |> List.map (fun t -> List.maxBy (fun elem -> let (_, _, dist) = elem
                                                           dist) 
                                              (snd t))           // Take only the ones that are the maximum distance at a given cos(t)
             |> List.map (fun e -> let (v, _, _) = e
                                   v)                            // extract the values you actually want
             |> List.rev    // need to reverse since we are measuring cos(t) which goes from 1->0 as t goes from 0->90
    
    P0::ps //(List.rev ps) 

type Rotation = CCW | CW | COLINEAR

let rotationDirection (a: OrderedPair) (b: OrderedPair) (c: OrderedPair): Rotation = 
    let cpMag = (b.x - a.x)*(c.y - a.y) - (b.y - a.y)*(c.x - a.x)
    if cpMag < 0 then
        CW
    else
        if cpMag = 0 then
            COLINEAR
        else
            CCW

let findConvexHull (p:Polygon) : Polygon =
    let rec hullHelper (points:Polygon) (stack:Polygon) =
        let rec stackClearer p ss =
            match ss with
            | top::next_to_top::rs -> let rotDir = rotationDirection next_to_top top p
                                      if rotDir <> CCW then
                                        stackClearer p ss.Tail
                                      else
                                        p::ss
            | _ -> ss
            
        
        match points with
        | p::ps -> match stack with
                   | top::next_to_top::rs -> hullHelper ps (stackClearer p stack)
                   | _ -> hullHelper ps (p::stack)
                                                
        | _ -> stack
    let ps = sortByPolarAngle p
    let stack: Polygon = []
    hullHelper ps []

        
        

