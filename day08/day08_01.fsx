open System
open System.IO
open System.Text.RegularExpressions

let getFile = File.ReadAllLines "day08_input.txt"

let ReMatches re str =
  let m = Regex.Match(str, re)
  m.Success

let parseAcc cmd = 4
let parseJmp cmd idx = 2



let (|Nop|Acc|Jmp|Err|) cmd =
  if ReMatches cmd "^nop"
    then Nop
  if ReMatches cmd "^acc"
    then Acc
  if ReMatches cmd "^jmp"
    then Jmp

  else Err
  // else None

let rec execCmd idx (program: string list) (history: int list) acc =
  if List.contains idx history
    then acc

  match program.[idx] with
    | ReMatches
  if program.[idx] = "nop"
    then execCmd (idx+1) program history acc

  if program.[idx] = "acc"
    then
      let newIdx = parseAcc program.[idx]
      execCmd newIdx program history acc

  if program.[idx] = "jmp"
    then
      let newIdx = parseJmp program.[idx] idx
      execCmd newIdx program history acc

getFile
  |> Array.toList
  |> printfn "DEBUG:: %A"
  // |> findIn [] ["shiny gold"]
  // |> List.length
  // |> Console.WriteLine
