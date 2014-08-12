namespace FSharp.Euler.Problems

open System
open System.IO
open System.Collections.Generic
open Euler.Common.NumberTheory

module Problem42 =
  let private readData =
    let data = File.ReadAllText("Data/words.txt").Split(',')
    seq { for word in data do yield word.Trim('\"') }
  
  let solve =
    let triangulars = new HashSet<int>( seq { for i in 1L .. 200L do yield int (triangular i) } )
    let count = ref 0
    for word in readData do
      let sum = ref 0
      for c in word do
        sum := !sum + (int c - int 'A' + 1)
      if triangulars.Contains !sum then count := !count + 1
    printfn "%d" !count
