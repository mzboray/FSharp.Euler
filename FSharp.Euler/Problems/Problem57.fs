namespace FSharp.Euler.Problems

open System
open FSharp.Euler.Common.NumberTheory
open System.Numerics

module Problem57 =

  let private f =
    let cache = ref (Map.ofArray [| (0, (bigint 1, bigint 1)); (1, (bigint 3, bigint 2)) |])
    let rec loop n =
      match (!cache).TryFind n with
      | Some value -> value
      | None ->
        let a_n1, b_n1 = loop (n - 1)
        let a_n2, b_n2 = loop (n - 2) 
        let func a b = (bigint 2) * a + b
        let value = (func a_n1 a_n2 , func b_n1 b_n2 )
        cache := (!cache).Add (n, value)
        value
    loop
  
  let solve =
    let mutable count = 0
    for i in 1 .. 1000 do
      let (a, b) = f i
      count <- count + if (digitCount a) > (digitCount b) then 1 else 0
    printfn "%d" count