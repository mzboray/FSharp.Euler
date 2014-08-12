namespace FSharp.Euler.Problems

open System
open FSharp.Euler.Common.NumberTheory

module Problem65 =

  let continuedFractionSeq =
    let rec generateSequence k = 
      seq {
        yield 1I
        yield 1I
        yield 2I * k
        yield! generateSequence (k + 1I)
      }
    seq {
      yield 2I
      yield 1I
      yield 2I
      yield! generateSequence 2I
    }

  // NOTE: Zero-based
  let fractions = continuedFractionSeq |> Seq.cache

  let numerator =
    let cache = ref (Map.ofArray [| (-2,0I); (-1,1I) |])
    let rec loop n = 
      match (!cache).TryFind n with 
      | Some value -> value
      | _ -> 
        let result = (fractions |> Seq.nth n) * loop (n - 1) + loop (n - 2)
        cache := (!cache).Add (n, result)
        result
    loop
    
  let solve =
      printfn "%A" (digitSum (numerator 99))
    
