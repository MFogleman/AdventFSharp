#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"

open System
open System.IO


type Passport = {
  Byr: string option
  Iyr: string option
  Eyr: string option
  Hgt: string option
  Hcl: string option
  Ecl: string option
  Pid: string option
}
let defaultPassport = {
  Byr = None
  Iyr = None
  Eyr = None
  Hgt = None
  Hcl = None
  Ecl = None
  Pid = None
}

let getFile = File.ReadAllText "day04_input.txt"

let toTups (str: string) =
  let arr = str.Split(':')
  (arr.[0], arr.[1])

let buildPassport (passport: Passport) (k: string, v: string) =
  match k with
    | "byr" -> { passport with Byr = Some v}
    | "iyr" -> { passport with Iyr = Some v}
    | "eyr" -> { passport with Eyr = Some v}
    | "hgt" -> { passport with Hgt = Some v}
    | "hcl" -> { passport with Hcl = Some v}
    | "ecl" -> { passport with Ecl = Some v}
    | "pid" -> { passport with Pid = Some v}
    | _ -> passport

let countValid acc (passport: string) =
  let sameLine = passport.Replace('\n', ' ').Split(' ')
  let tups = Array.map toTups sameLine
  let newPassport = Array.fold buildPassport defaultPassport tups

  let change = match newPassport with
                | {Byr=None } -> 0
                | {Iyr=None } -> 0
                | {Eyr=None } -> 0
                | {Hgt=None } -> 0
                | {Hcl=None } -> 0
                | {Ecl=None } -> 0
                | {Pid=None } -> 0
                | _ -> 1
  acc + change

let getRawPassports (str: string) =
  str.Split([|"\n\n"|], StringSplitOptions.None)

getFile
  |> getRawPassports
  |> Array.fold countValid 0
  |> printfn "debug:: %A" // 219

