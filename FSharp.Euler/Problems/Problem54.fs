namespace FSharp.Euler.Problems

open System

module Problem54 =

  type Suit = 
    | Hearts | Clubs | Diamonds | Spades
  
  type Value =
    | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    
  type Card =
    | Card of Value * Suit
    
  type HandPower =
    | HighCard of Value
    | OnePair of Value * HandPower
    | TwoPair of Value * Value * HandPower
    | ThreeOfAKind of Value * HandPower
    | Straight of Value
    | Flush of Suit * Value * HandPower
    | FullHouse of Value * Value * HandPower
    | FourOfAKind of Value * HandPower
    | StraightFlush of Value
    | RoyalFlush
    
  type Result =
    | Player1
    | Player2
    | Tie
    
  let getValue card =
    match card with
    | Card(value, _) -> value
  
  let getSuit card = 
    match card with
    | Card(_, suit) -> suit
    
  let getDistance value1 value2 =
    let getInt value =
      match value with
      | Two -> 2
      | Three -> 3
      | Four -> 4
      | Five -> 5
      | Six -> 6
      | Seven -> 7
      | Eight -> 8
      | Nine -> 9
      | Ten -> 10
      | Jack -> 11
      | Queen -> 12
      | King -> 13
      | Ace -> 14
    abs (getInt value1 - getInt value2)
    
  let parseCard (input : string) = 
    if input.Length <> 2 then invalidArg "input" "Value must be a card value and a suit"
    let value, suit = input.[0], input.[1]
    let parseValue value =
      match value with
      | '2' -> Two
      | '3' -> Three
      | '4' -> Four
      | '5' -> Five
      | '6' -> Six
      | '7' -> Seven
      | '8' -> Eight
      | '9' -> Nine
      | 'T' -> Ten
      | 'J' -> Jack
      | 'Q' -> Queen
      | 'K' -> King
      | 'A' -> Ace
      | _ -> failwith (sprintf "Unkown value '%c'" value)
    let parseSuit suit =
      match suit with
      | 'H' -> Hearts
      | 'C' -> Clubs
      | 'D' -> Diamonds
      | 'S' -> Spades
      | _ -> failwith (sprintf "Unkown suit '%c'" suit)
    Card (parseValue value, parseSuit suit)
    
  let parseCards (inputs : string[]) =
    inputs |> Seq.map (fun input -> parseCard input)
    
  let parseHands (input : string) =
    let cards = input.Split()
    parseCards cards.[0..4],parseCards cards.[5..9]
    
  let isStraight hand =
    let sorted = hand |> Seq.map getValue |> Seq.sort
    let firstValue = Seq.head sorted
    let projected = sorted |> Seq.map (getDistance firstValue) |> Array.ofSeq
    [|0;1;2;3;4|] = projected
    
  let isFlush hand =
    let groupCount = hand |> Seq.groupBy getSuit |> Seq.length
    groupCount = 1
    
  let getValues hand =
    let grouped = hand |> Seq.groupBy getValue
    failwith "not implemented"
    
  let solve =
    let input = "2C 3C 4C 5C 6C 7D 2S 5D 3S AC"
    let hand = List.ofSeq (fst (parseHands input))
    printfn "%A" (isStraight hand, isFlush hand)
