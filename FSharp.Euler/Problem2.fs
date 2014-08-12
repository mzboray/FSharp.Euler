namespace FSharp.Euler.Problems

open FSharp.Euler.Common.NumberTheory

module Problem2 =

  let solve =
    let result = fibs |> Seq.takeWhile (fun i -> i <= 4000000I) |> Seq.filter (fun i -> i % 2I = 0I) |> Seq.sum
    printfn "%A" result
    
