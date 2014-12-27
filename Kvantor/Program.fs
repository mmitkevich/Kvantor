open System
open Kvant
open Fmat.Numerics

type Position = float*Asset
let rate = 0.1
let debug = false //true

let future und ex now = Asset.Vanilla(und, Future, 1., maturity = Calendar.Russia.maturity now ex, rate=rate)

let call und strike ex volatility now = Asset.Vanilla(und, Call, strike, maturity = Calendar.Russia.maturity now ex, rate=rate, volatility=volatility)
let put und strike ex volatility now = Asset.Vanilla(und, Put, strike, maturity = Calendar.Russia.maturity now ex, rate=rate, volatility=volatility)
let valueAndGreeks m = 
    sprintf "%12g | %12.3f | %12.2f | %12.2f" (BlackScholes.value m) (BlackScholes.delta m) (-(BlackScholes.theta m)/365.) ((BlackScholes.vega m)/100.)

let price portfolio (asset:float->Asset) spot volatility now = 
    let pf  = portfolio (asset spot) volatility now
    if debug then printfn "%12s | %12s | %12s | %12s | %12s | %12s | %12s | %12s" "pos" "kind" "strike" "time" "value" "delta" "theta" "vega"
    let folder = fun sum pos -> 
        let (amount,asset) = pos
        let m = BlackScholes.price asset
        if debug then printfn "%12g | %12s | %12g | %12g | %s" amount (sprintf "%A" asset.vanillaKind) asset.strike (365.*asset.maturity.Value) (valueAndGreeks m)
        sum + m
    pf |> List.fold folder (Matrix.zeros [1;5] )


let expn n  = 
    match n with
    | 1 -> DateTime(2015,1,15)
    | 2 -> DateTime(2015,2,15)
    | 3 -> DateTime(2015,3,16)
    | _ -> invalidOp "unknown expiration index"

let rub = Asset.Currency("RUB")
let usdrub spot = Asset.Spot(rub,"USD_RUB", price=spot)


[<EntryPoint>]
let main argv = 
    let portfolio (s:Asset) volatility now  = 
        [   (5.,    call    s  60000.     (expn 3)    volatility   now);
            (10.,   put     s  60000.     (expn 1)    volatility   now);
            (10.,   call    s  70000.     (expn 3)    volatility   now);
            (12.,   put     s  67000.     (expn 1)    volatility   now);
            (4.,    put     s  70000.     (expn 3)    volatility   now);
            (10.,   call    s  60000.     (expn 1)    volatility   now);
            (5.,    call    s  65000.     (expn 1)    volatility   now);
            (1.,    call    s  55000.     (expn 3)    volatility   now);
            (-5.,   put     s  50000.     (expn 3)    volatility   now);
            (16.,   put     s  55000.     (expn 1)    volatility   now);
            (8.,    put     s  70000.     (expn 1)    volatility   now);
            (15.,   future  s             (expn 3)                 now);
        ]

    let report spot volatility (now:DateTime) = 
        printfn "\ndate = %12s | spot = %12g | volatility = %12g" (now.Date.ToShortDateString()) spot volatility
        let m = price portfolio usdrub spot volatility now 
        printfn "%12s | %12s | %12s | %12s" "value" "delta" "theta" "vega"
        printfn "%s" (valueAndGreeks m)

    let iterate spots volatilities now = 
        volatilities |> List.iter(fun volatility->
            spots |> List.iter(fun spot -> report spot volatility now)|>ignore
            )

    //let vols = [0.2; 0.5; 0.8]
    let vols = [0.4]
    let spots = [30000.; 50000.; 58000.; 60000.; 63000.; 100000.]
    //let spots = [60000.]

    iterate  spots vols (DateTime(2015,3,15))
    0
