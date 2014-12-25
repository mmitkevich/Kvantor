module Kvant.Numerics

open Fmat.Numerics

let data (m:Matrix) = 
    match m.Data with
    | Managed d->d
