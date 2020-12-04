#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"

open System
open System.IO
open System.Text.RegularExpressions

let debug (msg: string) input =
  printfn "%A %A" msg input
  input

// let getFile = File.ReadAllLines "day04_input.txt"
let getFile = File.ReadAllText "day04_input.txt"

let req = ["byr";"iyr";"eyr";"hgt";"hcl";"ecl";"pid"]

let stringify acc curr =
  match curr with
  | "" -> acc + "&"
  | _ -> acc + curr


let blankToNl str =
  match str with
  | "" -> "&"
  | _ -> str


let splitEmpty (str: string) =
  str.Split ' '

let joinEmpty (arr: string []) =
  String.Join(" ", arr)

let ecls = ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]

let validEcl (ecl: string) =
  List.exists(fun validColor -> validColor = ecl) ecls


let validRange min max (str: string) =
  match System.Int32.TryParse str with
  | true,int -> int >= min && int <= max
  | _ ->  false

let validByr = validRange 1920 2002
let validIyr = validRange 2010 2020
let validEyr = validRange 2020 2030

let validIn = validRange 59 76
let validCm = validRange 150 193

let normalizePassport (passport: string) =
  passport.Split(' ') |> Array.map(fun field ->
    let arr = field.Split(':')
    let k = arr.[0]
    let v = arr.[1]
    (k, v)
  )

let validHcl str =
  let m = Regex.Match(str, "^#[0-9a-f]{6}$")
  m.Success

let validPid str =
  let m = Regex.Match(str, "^\d{9}$")
  m.Success

let matches re str =
  let m = Regex.Match(str, re)
  m.Success

let validHgt str =
  match str with
  | str when matches "^\d\d\dcm$" str -> str.[0..2] |> validCm
  | str when matches "^\d\din$" str -> str.[0..1] |> validIn
  | _ -> false

let validateField (acc: int) (k, v) =
  match k with
  |"byr" -> if validByr v then acc+1 else acc
  |"iyr" -> if validIyr v then acc+1 else acc
  |"eyr" -> if validEyr v then acc+1 else acc
  |"hgt" -> if validHgt v then acc+1 else acc
  |"hcl" -> if validHcl v then acc+1 else acc
  |"ecl" -> if validEcl v then acc+1 else acc
  |"pid" -> if validPid v then acc+1 else acc
  | _ -> acc

let validatePassports acc (passport: (string * string) []) =
  let validFields = Array.fold validateField 0 passport
  if validFields = 7 then acc + 1 else acc

getFile
  |> (fun str -> str.Split ([|"\n\n"|], StringSplitOptions.None) )
  |> Array.map(fun str -> str.Replace('\n', ' '))
  |> Array.map(normalizePassport)
  // |> debug "afterNomralize"
  |> Array.fold validatePassports 0
  |> debug "answer::"



  // |> Array.fold stringify ""
  // |> (fun str -> str.Split '&')
  // |> Array.fold countValid 0

//274 ecls
