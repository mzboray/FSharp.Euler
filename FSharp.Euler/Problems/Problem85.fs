namespace FSharp.Euler.Problems

open System
open FSharp.Euler.Common.NumberTheory
open System.Numerics

module Problem85 =
  let g n m =
    n * (n + 1) * m * (m + 1) / 4
  
  let fst (a, _, _) = a
  let snd (_, b, _) = b
  let thr (_, _, c) = c
  
  let solve =
    let mutable minItems = (Int32.MaxValue, 0, 0)
    for i in 1 .. 1000 do
      for j in 1 .. 1000 do
        let value = (g i j)
        let diff = abs (value - 2000000)
        if diff < fst minItems
        then
          minItems <- (diff, i, j)
    printfn "result: %A %A" minItems ((snd minItems) * (thr minItems))
    