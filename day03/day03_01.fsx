#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"

open System
open System.IO

let getFile = File.ReadAllLines "day03_input.txt"

let circleAdd max a b =
  let sum = a + b
  if sum > max then (sum-max-1) else sum

let addToRow = circleAdd 30
// right 3 down 1
// # is tree
let countTrees (input: string list) =
  let mutable total = 0;
  let mutable y = 0;
  for row in input.Tail do
    y <- addToRow 3 y
    printfn "On row %A, y is %A, val is %A" row y row.[y]
    if row.[y] = '#' then total <- total + 1
  total
  // let tail = input.Tail;
  // List.iter (funtail.iter






getFile
  |> Array.toList
  |> countTrees
  |> Console.WriteLine // 286
