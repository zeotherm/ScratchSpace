﻿open System
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

let ZERO = Const( 0.)
let ONE = Const( 1.)
let TWO = Const( 2.)
let rec Derivative x : Expression = 
    match x with
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
        let gx = g(X)
        let g' = Derivative(gx)
        let f' = Derivative(f)
        match g' with
        | Func( dgf, _) -> Mul(dgf(f), f')
        | Op( op, e1, e2) -> Mul(op(e1, e2), f')
        | Neg( e ) -> Mul(Neg(e), f')
        | _ -> failwith(sprintf "Unable to match compound function [%A]" g')
    | _ -> failwith(sprintf "Unable to match expression [%A]" x)

[<EntryPoint>]
let main argv = 
    let f = Cos(Pow(X, ONE))

    let f' = Derivative(f)

    printfn "%A" f'
    0 // return an integer exit code
