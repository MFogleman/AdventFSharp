#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"

open System
open System.IO

let getFile = File.ReadAllLines "day02_input.txt"

let parseInput (line: string) =
    let parts = line.Split[|' '|]
    parts

let charMatches char str =
    str = char

let isBetween min max n =
    min <= n && n <= max

let countChars char str =
    String.filter (charMatches char) str
    |> String.length

let getFirstChar (str: string) =
    str.[0]

let parseRange (range: string) =
    let digits = range.Split '-'
    (digits.[0] |> int, digits.[1] |> int)

let matchesAt (password: string) char num =
    password.[num-1] = char

let countValidPasswords acc (line: string) =
    let arr = line.Split[|' '|]
    let (a, b) = parseRange arr.[0]
    let char = getFirstChar arr.[1]

    let matchesCharAt = matchesAt arr.[2] char

    if matchesCharAt a <> matchesCharAt b
        then acc+1
        else acc

getFile
    |> Array.fold countValidPasswords 0
    |> Console.WriteLine // 485
