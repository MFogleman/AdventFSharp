#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"

open System
open System.IO
open System.Text.RegularExpressions

let getFile = File.ReadAllText "day04_input.txt"

let validRange min max (str: string) =
  match System.Int32.TryParse str with
  | true,int -> int >= min && int <= max
  | _ ->  false

let matches re str =
  let m = Regex.Match(str, re)
  m.Success

let validEcl = matches "^amb|blu|brn|gry|grn|hzl|oth$"
let validHcl= matches "^#[0-9a-f]{6}$"
let validPid = matches "^\d{9}$"
let validByr = validRange 1920 2002
let validIyr = validRange 2010 2020
let validEyr = validRange 2020 2030
let validIn = validRange 59 76
let validCm = validRange 150 193
let validHgt str =
  match str with
  | str when matches "^\d\d\dcm$" str -> str.[0..2] |> validCm
  | str when matches "^\d\din$" str -> str.[0..1] |> validIn
  | _ -> false

let validateFields (acc: int) (k, v) =
  match k with
  |"byr" -> if validByr v then acc+1 else acc
  |"iyr" -> if validIyr v then acc+1 else acc
  |"eyr" -> if validEyr v then acc+1 else acc
  |"hgt" -> if validHgt v then acc+1 else acc
  |"hcl" -> if validHcl v then acc+1 else acc
  |"ecl" -> if validEcl v then acc+1 else acc
  |"pid" -> if validPid v then acc+1 else acc
  | _ -> acc

let validatePassports totalValidPassports (passport: (string * string) []) =
  match Array.fold validateFields 0 passport with
  | 7 -> totalValidPassports + 1
  | _ -> totalValidPassports

let kvTupleFromField (field: string) =
  let arr = field.Split(':')
  (arr.[0], arr.[1])

let normalizePassport (passport: string) =
  passport
    .Replace('\n', ' ')
    .Split(' ')
    |> Array.map(kvTupleFromField)

let replaceNewlineWithWhitespace (str: string) =
  str.Replace('\n', ' ' )

let splitOnEmptyLine (str: string) =
  str.Split([|"\n\n"|], StringSplitOptions.None)

getFile
  |> splitOnEmptyLine
  |> Array.map(normalizePassport)
  |> Array.fold validatePassports 0
  |> Console.WriteLine
  // 127
