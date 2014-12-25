namespace Kvant

open FSharp.Control.Reactive
open System.Reactive.Subjects
open Fmat.Numerics
open Fmat.Numerics.Conversion
open Fmat.Numerics.MatrixFunctions

type AssetKind = Spot | Future | VanillaOption

type VanillaKind = Call | Put

type ExcerciseKind = European | American

type Asset(?symbol:string, ?underlying:Asset, ?kind:AssetKind, ?lot:float, ?price:float, ?vanillaKind:VanillaKind, ?strike: float, ?excercise:ExcerciseKind, ?maturity:float, ?rate:float, ?volatility:float) = 
    member val price        = Behavior.create (defaultArg price 1.)
    member val lot          = Behavior.create (defaultArg lot 1.)

    member val symbol       = defaultArg symbol "n/a"
    member val kind         = defaultArg kind Spot
    member val excercise    = defaultArg excercise American
    member val vanillaKind  = defaultArg vanillaKind Call

    member val underlying   = underlying

    member this.spot        = Option.get(this.underlying).price

    member val maturity     = Behavior.create( defaultArg maturity 0.)
    member val rate         = Behavior.create (defaultArg rate 0.)
    member val volatility   = Behavior.create (defaultArg volatility 0.)
    member val strike       = defaultArg strike 1.

    static member VanillaOption (underlying:Asset, vanillaKind:VanillaKind, strike:float, ?symbol:string, ?lot:float, ?price:float, ?excercise:ExcerciseKind, ?maturity:float, ?rate:float, ?volatility:float ) = 
        Asset(  symbol = (defaultArg symbol "OPT"),
                underlying = underlying, kind = VanillaOption, vanillaKind = vanillaKind, strike = strike,
                ?lot = lot, ?price = price, 
                ?excercise = excercise,
                ?maturity = maturity, ?rate = rate, ?volatility = volatility)
               
    static member Future (underlying:Asset, ?symbol:string, ?lot:float, ?price:float, ?maturity:float, ?rate:float, ?volatility:float) = 
        Asset(  symbol = (defaultArg symbol "FUT"),
                underlying = underlying, kind = Future, 
                ?lot = lot, ?price = price, 
                ?maturity = maturity, ?rate = rate, ?volatility = volatility)


    static member Spot (?currency:Asset, ?symbol:string, ?lot:float, ?price:float, ?maturity:float, ?rate:float, ?volatility:float) = 
        Asset(?symbol = symbol, ?underlying = currency, kind = Spot, ?lot = lot, ?price = price,
            ?maturity = maturity, ?rate = rate, ?volatility = volatility)

    static member Currency (symbol:string) = 
        Asset(symbol = symbol)
       

