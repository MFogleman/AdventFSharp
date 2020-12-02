open System
open System.IO

let getFile = File.ReadAllLines "inputs/day01_01_input.txt"
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


[<EntryPoint>]
let main argv =
    getFile 
    |> Array.toList
    |> List.map int
    |> findDigitsToMatch2020
    |> List.reduce multiply
    |> Console.WriteLine

    0 // return an integer exit code

