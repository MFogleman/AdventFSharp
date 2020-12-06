open System
open System.IO

let getFile = File.ReadAllText "day06_input.txt"

let normalizeInput (str: string) =
  str.Split([|"\n\n"|], StringSplitOptions.None)

let foldr (neededLen: int) total (_, num) =
  match num with
  | num when num = neededLen -> total + 1
  | _ -> total

let joinStr (str: string []) = String.Join("", str)
let lenOf (x: string []) = x.Length


let countTotals (str: string) =
  let people = str.Split('\n')
  let allResponses = joinStr people

  Seq.countBy id allResponses
  |> Seq.fold (foldr (lenOf people)) 0

getFile
  |> normalizeInput
  |> Array.sumBy countTotals
  |> printfn "DEBUG %A"
