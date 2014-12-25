namespace Kvant.Transaq
open Kvant
open System.Text.RegularExpressions
open System.Collections.Generic
(*
type AssetsMap = Map<string,Asset>
type Position  = float*Asset

type TransaqUtil = 
    //7600s5g   5   Si-3.15M160315CA 60000  5                                   -2 800.00       
    static member parseDerivativePositionsFile(path:string):Dictionary<string,Asset>*seq<Position> =
        let assets = Dictionary<string,Asset>()
        let pos_sym_underl = System.IO.File.ReadLines(path) |> Seq.skip(2) |> Seq.map(fun (line:string) -> 
                let items = Regex.Split(line, @"\t+")

                let (account, position, symbol) = (items.[0], float items.[1], items.[2])
                let parts = Regex.Split(symbol,@"[- ]")
                let undsym = parts.[0]
                let strike = if parts.Length >=2 then float parts.[2] else 1.
                (position, symbol, undsym)
-            )

        

        pos_sym_underl |> Seq.iter(fun psu ->
            let pos, sym, und = psu

            )


*)