open System
open Kvant

type Position = float*Asset
let now = DateTime.Now
let rate = 0.1
let volatility = 0.5
let expirations = [|DateTime(2015,1,15); DateTime(2015,3,16)|]

let futpos =  15.
let call und strike iexp now = Asset.VanillaOption(und, Call, strike, maturity = Calendar.Russia.maturity now expirations.[iexp], rate=rate, volatility=volatility)
let put und strike iexp now = Asset.VanillaOption(und, Put, strike, maturity = Calendar.Russia.maturity now expirations.[iexp], rate=rate, volatility=volatility)

let calc spot = 
    let rub = Asset.Currency("RUB")
    let usd_rub = Asset.Spot(rub, "USD_RUB", price=spot)
    let usd_rub_fut = Asset.Future(usd_rub)
    let portfolio now = [
        (5.,    call    usd_rub 60000.     1    now);
        (10.,   put     usd_rub 60000.     0    now);
        (10.,   call    usd_rub 70000.     1    now);
        (12.,   put     usd_rub 67000.     0    now);
        (4.,    put     usd_rub 70000.     1    now);
        (10.,   call    usd_rub 60000.     0    now);
        (5.,    call    usd_rub 65000.     0    now);
        (1.,    call    usd_rub 55000.     1    now);
        (-5.,   put     usd_rub 50000.     1    now);
        (16.,   put     usd_rub 55000.     0    now);
        (8.,    put     usd_rub 70000.     0    now);
        ]

    let delta (pf:Position list) = 
        let total = pf |> List.sumBy (fun pos -> 
            let (amount,asset) = pos
            let (price,greeks) = BlackScholes.price asset
            //printfn "%A * %A\t %A\t %A\t %A\t %A" amount asset.vanillaKind asset.strike asset.maturity.Value price greeks
            amount*greeks.delta)
        total
    
    delta (portfolio now)


[<EntryPoint>]
let main argv = 
    [45000.; 50000.; 53000.; 55000.; 57000.; 58000.; 59000.; 60000.; 65000.; 70000.; 75000.] |> List.iter(fun s ->
        let deltaOpt = calc s 
        printfn "%A \t\t delta (options) = %A\t\t  (futures) = %A\t\t  (total) %A" s deltaOpt futpos (deltaOpt+futpos)
        )
    
    0
