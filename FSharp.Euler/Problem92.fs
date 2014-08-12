namespace FSharp.Euler.Problems

open System
open FSharp.Euler.Common.NumberTheory

module Problem92 =

  let digitSumSq n =
    let sq n = n * n
    let rec loop n acc =
      match n with
      | _ when n < 10 -> (sq n + acc)
      | _ -> loop (n / 10) (sq (n % 10) + acc)
    loop n 0
    
  let terminatesWith =
    let cache = ref ( Map.ofArray [|(1, 1); (89, 89); (44, 1); (85, 89); (32, 1);|] )
    let rec loop n =
      match (!cache).TryFind n with
      | Some value -> value
      | None -> 
        let result = loop (digitSumSq n)
        cache := (!cache).Add (n, result)
        result
    loop
    
  let solve =
    let count = seq { for i in 1 .. 9999999 do yield terminatesWith i } |> Seq.filter (fun i -> i = 89) |> Seq.length
    printfn "%d" count