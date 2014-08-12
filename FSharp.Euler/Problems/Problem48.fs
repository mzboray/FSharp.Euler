namespace FSharp.Euler.Problems

open System
open FSharp.Euler.Common.NumberTheory

module Problem48 =

  let rec private modPow (x : int64) (y : int64) (modulus : int64) =
    match y with
    | 0L -> 1L
    | 1L -> x
    | _ -> (x * (modPow x (y - 1L) modulus)) % modulus

  let solve = 
    let modulus = 1000000000000L
    let pows = seq { for i in 1L .. 1000L do yield modPow i i modulus }
    printfn "%d" (Seq.sum pows)
    ()