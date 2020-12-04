#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"

open System
open System.IO

let getFile = File.ReadAllLines "day01_input.txt"
let unpackAndMultiply tup =
    let a,b = tup
    a*b

let sumEq2020 p x = p + x = 2020

// find 2 numbers in nums that equal 2020
let rec findDigitsToMatch2020 nums =
    [for i in nums do
        for j in nums do
        if sumEq2020 i j then yield i,j]

getFile
    |> Array.toList
    |> List.map int
    |> findDigitsToMatch2020
    |> (fun arr -> arr.Head)
    |> unpackAndMultiply
    |> Console.WriteLine // 1003971

