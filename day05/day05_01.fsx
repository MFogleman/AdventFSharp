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
  | 'L' -> '1'
  | 'R' -> '0'
  | _ -> 'X'

// Convert.ToInt32(input, 2);

let toTuple (str: string) =
  (str.[0..6], str.[7..9])

let tupleToInt (a, b) =
 (Convert.ToInt32(a, 2), Convert.ToInt32(b, 2))


let convertToBinaryTuples = Array.map (String.map(toBinary) >> toTuple >> tupleToInt)

getFile
  |> convertToBinaryTuples
  |> printfn "DEBUG:: %A"
//     |> Array.map int
//     |> findDigitsToMatch2020
//     |> (fun arr -> arr.Head)
//     |> unpackAndMultiply
//     |> Console.WriteLine // 1003971

