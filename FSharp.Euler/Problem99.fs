namespace FSharp.Euler.Problems

open System
open System.IO

module Problem99 =

  let private parseLine (line : string) = 
    match line.Split(',') |> List.ofArray with
    | [b; exp] -> (Int32.Parse(b), Int32.Parse(exp))
    | _ -> failwith "Invalid format"

  let private readData = 
    let lines = File.ReadAllLines("base_exp.txt")
    seq { for line in lines do yield parseLine line }

  let solve = 
    let maxLine = ref -1
    let maxValue = ref -1.0
    for (index, (b, exp)) in readData |> Seq.mapi (fun i x -> (i, x)) do
      let value = (float exp) * log (float b)
      if (!maxValue < value) then 
        maxValue := value
        maxLine := index
    printfn "%d" (!maxLine + 1)
