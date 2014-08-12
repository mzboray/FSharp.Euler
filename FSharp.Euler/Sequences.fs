namespace FSharp.Euler.Common

open FSharp.Euler.Common.NumberTheory
open Checked

module Sequences = 

  let integersFrom start = 
    let x = ref start
    seq { while true do yield !x; x := !x + 1L }
    
  let naturalNumbers = 
    integersFrom 1L
    
  let primes = integersFrom 2L |> Seq.filter (fun i -> isPrime i) |> Seq.cache
