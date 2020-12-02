#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"
// include Fake modules, see Fake modules section

open System
open System.IO

let getFile = File.ReadAllLines "day01_01_input.txt"
let comps p x = p + x = 2020
let multiply a b = a * b

// find 2 numbers in nums that equal 2020
let rec findDigitsToMatch2020 nums =
    match nums with
    | [p; q] -> [p; q]
    | [] -> [] 
    | p::xs -> match List.tryFind (comps p) xs with
                | Some q -> [p; q] // "Found a value %d" value
                | None -> findDigitsToMatch2020 xs

getFile 
    |> Array.toList
    |> List.map int
    |> findDigitsToMatch2020
    |> List.reduce multiply
    |> Console.WriteLine

