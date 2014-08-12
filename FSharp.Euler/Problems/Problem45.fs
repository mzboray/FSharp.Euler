namespace FSharp.Euler.Problems

open System
open FSharp.Euler.Common.NumberTheory

module Problem45 =

  let rec private find j =
    let tj = triangular j
    if (isHexagonal tj && isPentagonal tj) then (j, tj) else find (j + 1L)

  let solve =
    printfn "%A" (find (286L))
    ()