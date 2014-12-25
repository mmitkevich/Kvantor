open Kvant

let rub = Asset.Currency("RUB")
let usd_rub = Asset.Spot(rub, "USD_RUB", price=60000.)
let usd_rub_fut = Asset.Future(usd_rub)

let portfolio = [
    (1., Asset.VanillaOption(usd_rub, Call, 60000., maturity=0.2, rate=0.1, volatility=0.5));
    (1., Asset.VanillaOption(usd_rub, Call, 65000., maturity=0.2, rate=0.1, volatility=0.5))
    (1., Asset.VanillaOption(usd_rub, Call, 65000., maturity=0.2, rate=0.1, volatility=0.5))
    ]

[<EntryPoint>]
let main argv = 
    0   
