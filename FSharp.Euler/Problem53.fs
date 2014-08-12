namespace FSharp.Euler.Problems

open System
open FSharp.Euler.Common.Combinatorics

module Problem53 = 

  let solve = 
    let mutable sum = 0
    for n in 23 .. 100 do
      for r in 1 .. n-1 do
        if (choose n r) > 1000000I then sum <- sum + 1
    printfn "%d" sum
