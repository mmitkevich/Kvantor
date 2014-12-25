module Kvant.Tests.BlackScholes

open Kvant

open Xunit
open DiffSharp.AD.Reverse

let black_scholes_price (kind:VanillaKind) (k:float) (check:Kvant.BlackScholes.Greeks->bool) = 
    let rub = Asset.Currency("RUB")

    let strike = 60000.
    let spot = k*strike

    let usd_rub = Asset.Spot(rub, "USD/RUB", price = spot)

    let otm_call = Asset.VanillaOption(usd_rub, Call, strike, maturity=0.2, rate=0.01, volatility=0.1)

    let (value, greeks) = Kvant.BlackScholes.price otm_call  
    printfn "spot/strike = %A price = %A greeks = %A" (spot/strike) value greeks

let (=~) x y = abs(x-y)<1e-3

[<Fact>]
let ``black scholes itm call delta should equal to 1.0``() = black_scholes_price Call 2. (fun greeks-> greeks.delta =~ 1.)

[<Fact>]
let ``black scholes atm call delta should equal to 0.5``() = black_scholes_price Call  1. (fun greeks-> greeks.delta =~ 0.5)

[<Fact>]
let ``black scholes otm call delta should equal to 0.0``() = black_scholes_price Call 0.5 (fun greeks-> greeks.delta =~ 0.)

[<Fact>]
let ``black scholes itm put delta should equal to -1.0``() = black_scholes_price Put 0.5 (fun greeks-> greeks.delta =~ -1.)

[<Fact>]
let ``black scholes atm put delta should equal to -0.5``() = black_scholes_price Put  1. (fun greeks-> greeks.delta =~ -0.5)

[<Fact>]
let ``black scholes otm put delta should equal to 0.0``() = black_scholes_price Put 2. (fun greeks-> greeks.delta =~ 0.)



