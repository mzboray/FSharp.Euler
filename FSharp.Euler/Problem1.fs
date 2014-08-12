namespace FSharp.Euler.Problems

module Problem1 =

  let solve =
    let result = seq { 1 .. 999 } |> Seq.filter (fun i -> i % 3 = 0 || i % 5 = 0) |> Seq.sum
    printfn "%d" result
