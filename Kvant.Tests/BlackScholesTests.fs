module Kvant.Tests.BlackScholes

open Kvant
open Fmat.Numerics
open Xunit
open DiffSharp.AD.Reverse

let black_scholes_price (kind:VanillaKind) (k:float) (matur:float) (check:Matrix->bool) = 
    let rub = Asset.Currency("RUB")

    let strike = 1.
    let spot = k*strike

    let asset = Asset.Spot(rub, "USD/RUB", price = spot)

    let otm_call = Asset.Vanilla(asset, Call, strike, maturity=matur, rate=0.01, volatility=0.1)

    let m = Kvant.BlackScholes.price otm_call  
    printfn "strike = %12g | spot = %12g | %s" strike  spot (BlackScholes.printGreeks m)

let (=~) x y = abs(x-y)<1e-3

let maturity = 0.2

[<Fact>]
let ``black scholes itm call delta should equal to 1.0``() = black_scholes_price Call 2. maturity (fun m-> BlackScholes.delta m =~ 1.)

[<Fact>]
let ``black scholes atm call delta should equal to 0.5``() = black_scholes_price Call  1. maturity (fun m-> BlackScholes.delta m =~ 0.5)

[<Fact>]
let ``black scholes otm call delta should equal to 0.0``() = black_scholes_price Call 0.5 maturity (fun m-> BlackScholes.delta m =~ 0.)

[<Fact>]
let ``black scholes itm put delta should equal to -1.0``() = black_scholes_price Put 0.5 maturity (fun m-> BlackScholes.delta m =~ -1.)

[<Fact>]
let ``black scholes atm put delta should equal to -0.5``() = black_scholes_price Put  1. maturity (fun m-> BlackScholes.delta m=~ -0.5)

[<Fact>]
let ``black scholes otm put delta should equal to 0.0``() = black_scholes_price Put 2. maturity (fun m-> BlackScholes.delta m=~ 0.)


[<Fact>]
let ``black scholes itm call price on expiration is (spot - strike)``() = black_scholes_price Call 10. 0.01 (fun m-> BlackScholes.value m =~ 10. - 1.)

[<Fact>]
let ``black scholes otm call price on expiration is 0``() = black_scholes_price Call 0.9 0.01 (fun m-> BlackScholes.value m =~ 0.)

