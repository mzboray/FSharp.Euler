namespace FSharp.Euler.Common

open System
open System.Collections.Generic
open Checked

module Combinatorics =

  
  let choose =
    let cache = ref Map.empty
    let rec loop n r : bigint =
      match n, r with
      | _, 0 -> 1I
      | _, _ when r >= n -> 1I
      | _, _ when r > n / 2 -> loop n (n - r)
      | _, _ ->
        match (!cache).TryFind (n, r) with
        | Some value -> value
        | None ->
          let value = (loop (n - 1) r) + (loop (n - 1) (r - 1))
          cache := (!cache).Add ((n, r), value)
          value
    loop
