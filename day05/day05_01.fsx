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

getFile
  |> Array.map convertToSeatId
  |> Array.max 
  |> Console.WriteLine // 901
