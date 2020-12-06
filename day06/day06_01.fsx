open System
open System.IO
open System.Text.RegularExpressions

let getFile = File.ReadAllText "day06_input.txt"

let normalizeInput (str: string) =
  str.Split([|"\n\n"|], StringSplitOptions.None)

let distinctChars (str: string) =
  Regex.Replace(str, "\n", "")
  |> Seq.distinct
  |> Seq.length

getFile
  |> normalizeInput
  |> Array.sumBy distinctChars
  |> printfn "DEBUG %A"
