open System
open System.IO
open System.Text.RegularExpressions

let getFile = File.ReadAllLines "day07_input.txt"

let bag color =
  sprintf "\d %s bags?" color

let colorIn line color: bool =
  let m = Regex.Match(line, bag color)
  m.Success

let checkFor colorsToCheckFor colorsFound line =
  match List.tryFind(colorIn line) colorsToCheckFor with
    | None -> colorsFound
    | Some _ -> line.Split([|" bags contain"|], StringSplitOptions.None).[0]::colorsFound

let rec findIn (results: string list) (colorsToCheckFor: string list) allBags =
  let justFound = List.fold (checkFor colorsToCheckFor) [] allBags
  let totalFound = List.distinct(justFound@results)
  match totalFound = results with
    | true -> totalFound
    | false -> findIn totalFound justFound allBags

getFile
  |> Array.toList
  |> findIn [] ["shiny gold"]
  |> List.length
  |> Console.WriteLine
