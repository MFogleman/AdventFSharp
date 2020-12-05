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

let binToRow char =
  match char with
  | '0' -> 'F'
  | '1' -> 'B'
  | _ -> 'X'

let binToCol char =
  match char with
  | '1' -> 'R'
  | '0' -> 'L'
  | _ -> 'X'

// Convert.ToInt32(input, 2);

let toTuple (str: string) =
  (str, str.[0..6], str.[7..9])

let tupleToInt (a, b, c) =
 (a, Convert.ToInt32(b, 2) * 8 + Convert.ToInt32(c, 2))


let convertToBinaryTuples = Array.map (String.map(toBinary) >> toTuple >> tupleToInt)
// bbbffffrlr
// BBBFFFFRLR

let DEBUG = [|"BFFFBBFRRR"; "FFFBBBFRRR"; "BBFFBBFRLL"|];

let stringTuples (bin: string, _) =
  (bin.[0..6] |> String.map(binToRow) , bin.[7..9] |> String.map(binToCol))

// DEBUG
getFile
  |> convertToBinaryTuples
  |> Array.sort
  |> Array.last
  |> printfn "DEBUG:: %A" // 901
//     |> Array.map int
//     |> findDigitsToMatch2020
//     |> (fun arr -> arr.Head)
//     |> unpackAndMultiply
//     |> Console.WriteLine // 1003971

