namespace FSharp.Euler.Problems

open System
open System.Collections.Generic
open FSharp.Euler.Common.NumberTheory


module Problem148 =  
  (* 1000000000 = 33531600616 in base 7 *)
  let rec private countRow row : int64 =
    match row with
    | _ when row < 7 -> int64 (row + 1)
    | _ -> (int64 (row % 7 + 1)) * (countRow (row / 7))
  
  let rec private sumRows row : int64 =
    match row with
    | 0 | 1 -> int64 row
    | _ -> 
      let q = row / 7
      let r = row % 7
      28L * (sumRows q) + (countRow q) * int64 (r * (r + 1) / 2)
    
  let solve =
    let n = 1000000000
    printfn "Sum = %d" (sumRows n)
