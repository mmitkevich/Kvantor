module Kvant.Calendar.Russia

open System

let holidays = [
        (1,1); (1,2); (1,3); (1,4); (1,5); (1,6); (1,7); (1,8); (1,9)   // new year 
        ; (1,5)
    ]

let expirationDate year month =
     match month with
     | 1-> DateTime(year,month,15)
     | 2-> DateTime(year,month,15)
     | _-> DateTime(year,month,16)


let maturity (now:DateTime) (expirationDate:DateTime) = expirationDate.Subtract(now).TotalDays / 365.
