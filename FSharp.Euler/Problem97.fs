namespace FSharp.Euler.Problems

open System
open FSharp.Euler.Common.Combinatorics
open Checked

module Problem97 = 

  let solve = 
    let modulus = 10000000000L
    let rec loop n acc =
      match n with
      | 0 -> acc
      | _ -> loop (n-1) ((2L * acc) % modulus)
    let result = ((28433L * loop 7830457 1L) + 1L) % modulus
    printfn "%d" result

