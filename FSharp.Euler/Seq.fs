namespace FSharp.Euler.Common


module Seq =
  
  let repeat items = 
    seq { while true do yield! items }
  
  let repeati source =
    let x = ref 0
    let rec loop s = seq { yield! Seq.map (fun n -> (!x, n)) s; x := !x + 1; yield! loop s }
    loop source