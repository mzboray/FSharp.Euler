namespace FSharp.Euler.Common

open System
open System.Linq
open Checked
open System.Numerics
open FSharp.Euler.Common.Seq
open Microsoft.FSharp.Core

  module NumberTheory =

    let inline gcd a b =
      let m, n = (max a b), (min a b)
      let rec loop m n =
        if (n = LanguagePrimitives.GenericZero) then m else loop n (m % n)
      loop m n
    

    let isqrt (a : int64) = 
      if (a < 0L) then invalidArg "a" "Value cannot be less than 0"
      if (a = 0L) then 
        0
      else
        let fa = (float a)
        let rec computeNext x =
          let next = (x + fa / x) / 2.0
          if abs (next - x) < 1.0 then next else computeNext next
        int (computeNext (fa / 2.0))
      
    let isPrime (n : int64) =
      let primesLessThan30 = [|2L;3L;5L;7L;11L;13L;17L;19L;23L;29L|]
      if n < 30L && primesLessThan30.Contains n 
      then 
        true 
      else
        let nsqrt = int64 (isqrt n)
        let candidateSequence = 
                Seq.repeati primesLessThan30 |> 
                Seq.map (fun (a, b) -> int64 (30 * a) + b) |> 
                Seq.takeWhile (fun i -> i <= nsqrt) |>
                List.ofSeq
        let rec check n s =
          match s with
          | i :: rest ->
            match n with
            | _ when n % i = 0L -> false
            | _ -> check n rest
          | _ -> true
        check n candidateSequence
        
    let primes n =
      let ps = new System.Collections.BitArray(n, true)
      ps.Set(0, false)
      ps.Set(1, false)
      let mutable i = 0
      while (i * i < n) do
        if (ps.Get i) then
          for j in i * i .. i .. n do
            ps.Set(j, false)
      ps
      
        
    let isPerfectSquare (n : int64) =
      let sqrt = int64 (isqrt n)
      sqrt * sqrt = n

    let pentagonal n : int64 =
      n * (3L * n - 1L) / 2L
    
    let triangular n : int64 =
      n * (n + 1L) / 2L
    
    let hexagonal n : int64 =
      n * (2L * n - 1L)

    let isPentagonal (n : int64) = 
      let x = 24L * n + 1L
      let xsqrt = int64 (isqrt x)
      (xsqrt * xsqrt = x) && xsqrt % 6L = 5L
    
    let isHexagonal (n : int64) =
      let x = 8L * n + 1L
      let xsqrt = int64 (isqrt x)
      (xsqrt * xsqrt = x) && xsqrt % 4L = 3L
    
    let inline digitCount (n : bigint) =
      let rec loop (n : bigint) acc =
        match n with
        | _ when n < (bigint 10) -> acc
        | _ -> loop (n / (bigint 10)) (1 + acc)
      loop n 1
      
    let digitSum (n : bigint) =
      let rec loop n acc =
        match n with
        | _ when n < 10I -> (int n + acc)
        | _ -> loop (n / 10I) (int (n % 10I) + acc)
      loop n 0
      
    let fib =
      let cache = ref (Map.ofArray [|(1,1I); (2,2I)|])
      let rec loop n =
        match (!cache).TryFind n with
        | Some value -> value
        | _ ->
          let result = loop (n - 1) + loop (n - 2)
          cache := (!cache).Add (n, result)
          result
      loop
    
    
    let private fibSeq =
      let rec loop n =
        seq { 
          yield fib n
          yield! loop (n + 1)
        }
      
      loop 1
    
    /// An infinite sequence of fibonacci numbers  
    let fibs = Seq.cache fibSeq
