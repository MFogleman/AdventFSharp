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
  match passport.Contains reqStr with
  | true -> acc + 1
  | false -> acc

// check over all requirements in passport.  If all are valid (7)
// increment acc for 1 more valid passport
let countValid acc passport =
  match List.fold (isIn passport) 0 req with
  | 7 -> acc + 1
  | _ -> acc


getFile
  |> Array.fold stringify ""
  |> (fun str -> str.Split '&')
  |> Array.fold countValid 0
  |> printfn "debug:: %A" // 219

