#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"

open System
open System.IO

let getFile = File.ReadAllLines "day01_01_input.txt"
let unpackAndMultiply tup =
    let a,b,c = tup
    a*b*c

let threeEqual2020 a b c = a + b + c = 2020

let rec findDigitsToMatch2020 xs = 
    [for i in xs do 
        for j in xs do 
            for k in xs do                 
                if threeEqual2020 i j k then yield i,j,k]

getFile 
    |> Array.toList
    |> List.map int
    |> findDigitsToMatch2020
    |> (fun arr -> arr.Head)
    |> unpackAndMultiply
    |> Console.WriteLine // 84035952

