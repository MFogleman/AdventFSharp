#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"

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

let convertToSeatId = Array.map (String.map(toBinary) >> toTuple >> tupleToSeatId)

let rec findFirstMissing (nums: int list) =
    match nums with
    | row::rows -> if row + 1 = rows.Head then findFirstMissing rows else row+1
    | _ -> -999

getFile
  |> convertToSeatId
  |> Array.sort
  |> Array.toList
  |> findFirstMissing
  |> Console.WriteLine // 661
