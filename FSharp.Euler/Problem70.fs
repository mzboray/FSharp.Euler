namespace FSharp.Euler.Problems

open FSharp.Euler.Common
open FSharp.Euler.Common.NumberTheory

module Problem70 =  

  let totient =
    let rec loop (state : Map<_, _>) n =
      if (isPrime (int64 n)) then 
        n + 1
      else
        let k =  
          match state.TryFind 2 with
          | Some value -> value
          | None -> 0
      
        if (n % 2 = 0) then 
          loop (state.Add(2, k + 1)) (n / 2) 
        else
          let result = seq { for i in 1 .. n do yield gcd n i } |> Seq.filter (fun i -> i = 1) |> Seq.length
          let factor = if k >= 1 then pown 2 k - pown 2 (k - 1) else 1
          result * factor
    loop Map.empty
    
    
  let solve =
    let sum = ref 0I
    for i in 1 .. 20000 do
      printfn "%d %d" i (totient i)
    //  sum := (!sum) + bigint (totient i)
    //printfn "%A" !sum