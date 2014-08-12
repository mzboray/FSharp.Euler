namespace FSharp.Euler

open System
open System.Diagnostics

module Benchmark =

  let time f n =
    let s = Stopwatch.StartNew()
    ignore (f n)
    s.Stop()
    s.Elapsed
    
  let bench f =
    let b n = 
      let mutable i = 0
      let mutable result = 0
      while i < n do
        result <- f()
        i <- i + 1
      result
    b
    
  let statistics f =
    let mutable i = 0
    let mutable values = List.empty
    while i < 1000 do
      values <- List.Cons ((time f 1), values)
    let average = values |> List.averageBy (fun i -> float i.Ticks)
    let stddev = (values |> List.sumBy (fun i -> float (pown (float i.Ticks - average) 2))) / (float values.Length)
    (average, stddev)

  let run (f : int -> int)  =
    let frequency = Stopwatch.Frequency
    printfn "Timer resolution: %f us" (1.0e6 / float frequency)
    ignore (f 1) // jit the method so it does not interefere with our timings
    let empty = bench (fun () -> 0)
    ignore (time empty 1)
    printfn "Empty method timing: %A" (time empty 1)
    time f 25
    
