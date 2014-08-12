namespace FSharp.Euler.Problems

open System
open FSharp.Euler.Common.NumberTheory

module Problem94 =

  let rec iterate (x, y) = seq { 
    yield x, y    
    printfn "%A" (x,y)
    yield! iterate (3I *x + 4I * y, 2I * x + 3I * y)
  }
  let perimeter (x, y) =
    4I * (x * x)
    
  let perimeter2 (x, y) =
    2I * (x + y) * (x + y)

  let takeUntil p s = s |> Seq.map p |> Seq.takeWhile (fun p -> p <= 1000000000I)

  let solve =
    let values = iterate (2I, 1I)
    let sum1 = values |> takeUntil perimeter |> Seq.sum
    let sum2 = values |> takeUntil perimeter2 |> Seq.sum
    printfn "%A" (sum1 + sum2)
