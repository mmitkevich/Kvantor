module Kvant.BlackScholes

open Kvant

// source: http://www.fssnip.net/3I
open DiffSharp.AD.Reverse
open Fmat.Numerics
open System.Reactive.Subjects

// Cumulative Normal Distribution Function - attempt to write a generic version
let inline CDF(x:Adj) : Adj = 
    let (b1,b2,b3)  = (0.319381530, -0.356563782, 1.781477937)
    let (b4,b5)     = (-1.821255978, 1.330274429)
    let (p , c )    = (0.2316419  ,  0.39894228)
    let (zero, one) = (LanguagePrimitives.GenericZero, LanguagePrimitives.GenericOne)
    if x > zero then
        let t:Adj = one / (one + p * x) 
        (one - c * exp( -x * x / 2.0)* t * (t*(t*(t*(t*b5+b4)+b3)+b2)+b1)) 
    else
        let t:Adj = 1.0 / (one - p * x) 
        (c * exp( -x * x / 2.0)* t * (t*(t*(t*(t*b5+b4)+b3)+b2)+b1))
    // - See more at: http://www.voyce.com/index.php/2009/06/26/black-scholes-option-pricing-using-fsharp-and-wpf/#sthash.i9UUX0Yh.dpuf
  
 
// call_put_flag: 'c' if call option; otherwise put option
// s: stock price
// x: strike price of option
// t: time to expiration in years
// r: risk free interest rate
// v: volatility

let blackScholes (vanillaKind:VanillaKind) (strike:float) (args:Adj[]) =
    let (spot, t, r, vol) = (args.[0],args.[1],args.[2],args.[3])
    let d1=(log(spot / strike) + (r+vol*vol/2.0)*t)/(vol*sqrt(t))
    let d2=d1-vol*sqrt(t)

    let result = 
        match vanillaKind  with
        | Call -> spot*CDF(d1) - strike*exp(-r*t)*CDF(d2)
        | Put -> -spot*CDF(-d1) + strike*exp(-r*t)*CDF(-d2)
        | Future ->  spot*exp(r*t)

    result

let future (args:Adj[]) = 
    let (spot,t,r) = (args.[0],args.[1],args.[2])
    spot*exp(r*t)

type Greeks = Greeks of Matrix
 
let matrixAsArray (m:Matrix) = 
    match m.Data with
    | Managed d->d

let valuesArray (bs:BehaviorSubject<'a> array):('a array)  =  Array.map (fun (b:BehaviorSubject<'a>) -> b.Value) bs

let price (asset:Asset):Matrix  = 
    match asset.kind with
    | Vanilla -> 
        match asset.vanillaKind with
        | None -> invalidOp "vanillaKind should be specified"
        | Some vk -> 
            match vk with
            | Call | Put ->
                let (price,greeks) = grad' (blackScholes vk asset.strike) [|asset.spot.Value; max asset.maturity.Value 1e-5; Option.get(asset.underlying).rate.Value; asset.volatility.Value|]
                Matrix (Array.append [|price|] greeks)
            | Future -> 
                let (price,greeks) = grad' future (valuesArray [|asset.spot; asset.maturity; Option.get(asset.underlying).rate|])
                Matrix (Array.append ((Array.append [|price|] greeks)) [|0.|])
    | _ -> invalidOp (sprintf "%A pricing not supported" asset.kind)

 
let value (arr:Matrix) = arr.[0]
let delta (arr:Matrix) = arr.[1]
let theta (arr:Matrix) = arr.[2]
let rho (arr:Matrix)   = arr.[3]
let vega (arr:Matrix)  = arr.[4]

let printGreeks m = 
    sprintf "value = %12.2f | delta = %12.3f | theta = %12.2f | vega = %12.2f" (value m) (delta m) (theta m) (vega m)

