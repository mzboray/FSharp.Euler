namespace FSharp.Euler.Problems

open System
open FSharp.Euler.Common.NumberTheory

module Problem44 =

  let private findMinD j initMinD =
    let pj = pentagonal j
    let rec check k minD =
      let pk = pentagonal k
      let d = pj - pk
      match k with
      | 0L -> minD
      | _ when d > minD -> minD
      | _ when isPentagonal (pj + pk) && isPentagonal d ->
        printfn "j:%d k:%d D:%d" j k d
        check (k - 1L) (min d minD)
      | _ -> check (k - 1L) minD
    check (j-1L) initMinD

  let solve = 
    let mutable minD = Int64.MaxValue
    for j in 1L .. 1827555L do
      minD <- findMinD j minD
    printfn "minimum D:%d" minD
