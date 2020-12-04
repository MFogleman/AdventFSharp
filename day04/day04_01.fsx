#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"

open System
open System.IO

let getFile = File.ReadAllLines "day04_input.txt"


let req = ["byr";"iyr";"eyr";"hgt";"hcl";"ecl";"pid"]

let stringify acc curr =
  match curr with
  | "" -> acc + "&"
  | _ -> acc + curr

let isIn (passport: string) acc reqStr =
  if passport.Contains reqStr
    then acc + 1
    else acc


let countValid acc passport =
  if List.fold (isIn passport) 0 req = 7
    then acc + 1
    else acc


getFile
  |> Array.fold stringify ""
  |> (fun str -> str.Split '&')
  |> Array.fold countValid 0
  |> printfn "debug:: %A" // 219

