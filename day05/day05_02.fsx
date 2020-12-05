open System
open System.IO

let getFile = File.ReadAllLines "day05_input.txt"

let toBinary char =
  match char with
  | 'F' -> '0'
  | 'B' -> '1'
  | 'L' -> '0'
  | 'R' -> '1'
  | _ -> 'X'

let toTuple (str: string) =
  (str.[0..6], str.[7..9])

let tupleToSeatId (b, c) =
 (Convert.ToInt32(b, 2) * 8 + Convert.ToInt32(c, 2))

let convertToSeatId = String.map(toBinary) >> toTuple >> tupleToSeatId

let (|FoundNext|NoNext|) (a, b) =
    if a + 1 = b then FoundNext else NoNext

let rec findFirstMissing (nums: int list) =
    match (nums.[0], nums.[1]) with
    | FoundNext -> findFirstMissing nums.Tail
    | NoNext -> nums.[0] + 1

getFile
  |> Array.map convertToSeatId
  |> Array.sort
  |> Array.toList
  |> findFirstMissing
  |> Console.WriteLine // 661
