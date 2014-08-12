module FSharp.Euler.Main

open System
open System.Collections.Generic
open System.Linq
open System.Diagnostics
open FSharp.Euler.Problems
open FSharp.Euler.Common
open FSharp.Euler.Benchmark

[<EntryPoint>]
let main args =  
  let stopwatch = Stopwatch.StartNew()
  Problem70.solve
  printfn "Done. Elapsed: %A" stopwatch.Elapsed
  0

