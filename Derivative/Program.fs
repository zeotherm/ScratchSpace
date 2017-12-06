open System
open System.Numerics

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let printn N =
   let IsNearlyInt x =
    let epsilon = 1e-10
    abs (x - round(x)) < epsilon

   if IsNearlyInt N then 
    sprintf "%d" (System.Convert.ToInt32(N))
   else 
    sprintf "%f" N

type Expression = 
    | X
    | Const of float
    | Neg of Expression
    | Add of Expression * Expression
    | Sub of Expression * Expression
    | Mul of Expression * Expression
    | Div of Expression * Expression
    | Pow of Expression * Expression
    | Sin of Expression
    | Cos of Expression
    | Tan of Expression
    | Exp of Expression
    | Ln of Expression

let (|Op|_|) (x : Expression) = 
    match x with
    | Add( e1, e2) -> Some( Add, e1, e2)
    | Sub( e1, e2) -> Some( Sub, e1, e2)
    | Mul( e1, e2) -> Some( Mul, e1, e2)
    | Div( e1, e2) -> Some( Div, e1, e2)
    | Pow( e1, e2) -> Some( Pow, e1, e2)
    | _ -> None

let (|Func|_|) (x : Expression) =
    match x with
    | Exp(e) -> Some( Exp, e)
    | Ln(e) -> Some(Ln, e)
    | Sin(e) -> Some(Sin, e)
    | Cos(e) -> Some(Cos, e)
    | Tan(e) -> Some(Tan, e)
    | _ -> None

let OpName op =
    match op with
    | Add(_, _) -> "+"
    | Sub(_, _) -> "-"
    | Mul(_, _) -> "*"
    | Div(_, _) -> "/"
    | Pow(_, _) -> "^"
    | _ -> failwith(sprintf "Unknown operator [%A]" op)

let FuncName f a =
    match f with 
    | Exp(_) -> sprintf "e^(%s)" a
    | Ln(_) -> sprintf "ln(%s)" a
    | Sin(_) -> sprintf "sin(%s)" a
    | Cos(_) -> sprintf "cos(%s)" a
    | Tan(_) -> sprintf "tan(%s)" a
    | _ -> failwith( sprintf "Unrecognized function [%A]" f)

let FormatExpression e =
    let addParens o s =
        match o with
        | None -> s
        | _ -> "(" + s + ")"
    // NB: Only need to be wrapped in parens if there is an order of operation mismatch... (^) >> (*/) >> (+-)
    let rec FormatSubExpression (outer : Expression option, inner : Expression) : string =
        match inner with
        | X -> "x"
        | Const(n) -> printn n
        | Neg x -> sprintf "-%s" (FormatSubExpression(Some(inner), x))
        | Mul( left, right) ->
            addParens outer (FormatSubExpression(Some(inner), left) + FormatSubExpression(Some(inner), right))
        | Op(_, left, right) -> 
            let s = FormatSubExpression(Some(inner), left) + " " + OpName(inner) + " " + FormatSubExpression(Some(inner), right) 
            addParens outer s 
        | Func(_, arg) -> FuncName(inner) (FormatSubExpression(None, arg))
        | _ -> failwith(sprintf "Unknown expression type to format [%A]" e)
    FormatSubExpression( None, e)

let rec Simplify x : Expression =
    match x with
    | Add( Const(n1), Const(n2)) -> Const(n1+n2)
    | Sub( Const(n1), Const(n2)) -> Const(n1-n2)
    | Mul( Const(n1), Const(n2)) -> Const(n1*n2)
    | Div( Const(n1), Const(n2)) -> Const(n1/n2)
    | Neg( Const(0.)) -> Const(0.)
    | Neg(Neg(e)) -> e |> Simplify
    | Neg( e) -> Neg( Simplify e)
    | Add( e, Const(0.)) | Add( Const(0.), e) -> e |> Simplify
    | Add( Const(n), e) -> Add(e, Const(n)) |> Simplify
    | Add( e1, Neg(e2)) -> Sub(e1, e2) |> Simplify
    | Add( Neg(e1), e2) -> Sub(e2, e1) |> Simplify
    | Sub( e, Const(0.)) -> e |> Simplify
    | Sub( Const(0.), e) -> Neg(e) |> Simplify
    | Mul( Const(1.), e) -> e |> Simplify
    | Mul( e, Const(1.)) -> e |> Simplify
    | Mul( _, Const(0.)) -> Const(0.)
    | Mul( Const(0.), _) -> Const(0.)
    | Mul( e, Const(n)) -> Mul(Const(n), e) |> Simplify
    | Mul( Const(n), Add(e, f)) -> Add(Mul(Const(n), e), Mul(Const(n), f)) |> Simplify
    | Mul( Const(n), Sub(l, r)) -> Sub(Mul(Const(n), l), Mul(Const(n), r)) |> Simplify
    | Mul( Const(n), Mul(l, r)) -> Mul(Mul(Const(n), l), r) |> Simplify
    | Mul( Div( Const(n), e1), e2) -> Mul(Const(n), Div(e1, e2)) |> Simplify
    | Mul( e1, Div( Const(n), e2)) -> Mul(Const(n), Div(e1, e2)) |> Simplify
    | Mul( Neg(e1), e2) -> Neg(Mul(e1, e2)) |> Simplify
    | Mul( e1, Neg(e2)) -> Neg(Mul(e1, e2)) |> Simplify
    | Div( Const(0.), e) -> Const(0.)
    | Div( e, Const(1.)) -> e |> Simplify
    | Div( Neg(e1), e2) -> Neg( Div(e1, e2)) |> Simplify
    | Div( e1, Neg(e2)) -> Neg( Div(e1, e2)) |> Simplify
    | Pow( Const(0.), _) -> Const(0.)
    | Pow( Const(1.), _) | Pow( _, Const(0.)) -> Const(1.)
    | Pow( e, Const(1.)) -> e |> Simplify
    | Op( op, e1, e2) ->
        let e1s = Simplify e1
        let e2s = Simplify e2
        if e1s <> e1 || e2s <> e2 then
            op(e1s, e2s) |> Simplify
        else
            op( e1, e2)
    | Func(f, e) -> 
        let es = Simplify e
        if es <> e then
            f( es) |> Simplify
        else 
            f(e)
    | _ -> x

let rec Derivative y : Expression = 
    let y' = 
        match y with
        | X -> Const( 1.)
        | Const(n) -> Const( 0.)
        | Neg(e) -> Neg(Derivative(e))
        | Add(e1, e2) -> Add( Derivative(e1), Derivative(e2))
        | Sub(e1, e2) -> Sub( Derivative(e1), Derivative(e2))
        | Mul(e1, e2) -> Add( Mul(Derivative(e1), e2), Mul(e1, Derivative(e2)))
        | Div(e1, e2) -> Div( Sub(Mul(Derivative(e1), e2), Mul(e1, Derivative(e2))), Pow(e2, Const(2.0)))
        | Pow(e, Const(n)) -> Mul( Const(n), Pow(e, Const(n-1.0)))
        | Pow(Const(n), e) -> Mul(Mul(Ln(Const(n)), Pow(Const(n), e)), Derivative(e))
        | Exp(X) -> Exp(X)
        | Ln(X) -> Div(Const( 1.), X)
        | Sin(X) -> Cos(X)
        | Cos(X) -> Neg(Sin(X))
        | Tan(X) -> Div(Const( 1.), Pow(Cos(X), Const( 2.)))
        | Func( g, f) ->
            let g' = Derivative(g(X))
            let f' = Derivative(f)
            match g' with
            | Func( dgf, _) -> Mul(dgf(f), f')
            | Op( op, e1, e2) -> Mul(op(e1, e2), f')
            | Neg( e ) -> Mul(Neg(e), f')
            | _ -> failwith(sprintf "Unable to match compound function [%A]" g')
        | _ -> failwith(sprintf "Unable to match expression [%A]" y)
    Simplify y'

[<EntryPoint>]
let main argv = 
    let f = Cos(Pow(X, Const(1.0)))

    let f' = Derivative(f)

    printfn "%A" (Simplify f)
    printfn "%s" (FormatExpression (Simplify f))
    printfn "%A" (Simplify f')
    printfn "%s" (FormatExpression (Simplify f'))


    let g = Add(Mul(Const(0.), X), Mul(Const(5.), Const(1.)))
    printfn "%A" (Simplify g)
    printfn "%s" (FormatExpression (Simplify g))

    let h = Mul(Mul(Const(2.), X), Const(3.))
    printfn "%s" (FormatExpression h)
    printfn "%s" (FormatExpression (Simplify h))

    let e4 = Mul(Add(X, Const(2.)), Const(3.))
    let t4 = Simplify e4

    printfn "============="

    printfn "%A" e4
    printfn "%A" t4
    printfn "%s" (FormatExpression e4)
    printfn "%s" (FormatExpression t4)
    
    0 // return an integer exit code
