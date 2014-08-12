namespace FSharp.Euler.Problems

open FSharp.Euler.Common.NumberTheory

module Problem140 =

  let private A (x, y) =
    let cache = ref Map.empty
    let rec f n =
      let key = (x, y), n
      match (!cache).TryFind key, n with
      | (Some value, _) -> value
      | (None, 1) -> int64 x
      | (None, 2) -> int64 y
      | (None, _) -> 
        let value = f (n - 1) + f (n - 2)
        cache := (!cache).Add (key, value)
        value
    f
    
  let private g = A (1, 4)
    
  let solve = 
    for i in 1 .. 30 do
      printfn "%10d %10d" i (g i)
    for i in 1L .. 20000L do
      if (isPerfectSquare (5L * i * i + 14L * i + 1L)) 
      then 
        let a = -(i + 1L) + int64 (isqrt (5L * i * i + 14L * i + 1L))
        let b = 2L * (i + 3L)
        let d = gcd a b
        let num, denom = a / d, b / d
        printfn "%d, %d / %d" i a b