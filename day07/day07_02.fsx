open System
open System.IO
open System.Text.RegularExpressions

let getFile = File.ReadAllLines "day07_input.txt"

let parseBagLine (line: string) =
  Regex.Matches(line, "\d \w+ \w+")
  |> Seq.cast
  |> Seq.map(fun (m: Match) -> m.Value)
  |> Seq.toList

let matchingBag color line =
  let re = sprintf "%s bags contain" color
  Regex.Match(line, re).Success

let charToInt c = int c - int '0'

let toTuple (str: string) =
 (charToInt str.[0], str.[2..str.Length-1])

let rec countBagsIn allBags (bagCount, (color: string)) =
  let childBags =
    allBags
    |> List.find(matchingBag color)
    |> parseBagLine
    |> List.map toTuple

  match List.isEmpty childBags with
    | true -> bagCount
    | false ->
      childBags
      |> List.fold(
        fun acc tup ->acc + countBagsIn allBags tup
      ) 0
      |>(fun s -> s * bagCount + bagCount)

getFile
  |> Array.toList
  |> (fun bags -> countBagsIn bags (1, "shiny gold"))
  |> (fun n -> n - 1)
  |> printfn "FINAL::%A"
