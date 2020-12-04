#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"

open System
open System.IO

let getFile = File.ReadAllLines "day03_input.txt"

(*
  Our input repeats infinitely to the right.  It is 31 characters long
  I.E elements 0-30
  If we are at character 30, and need to go right 3, we loop back to 0, 1, 2.
  so our new total is starting position 30 + add 3  = 33.  Then 33 - 30 elements
  puts us at element 3.  Then account for 0 indexing, and avoid the off by 1 error
*)
let circleAdd max a b =
  let sum = a + b
  if sum > max then (sum-max-1) else sum

let addToRow = circleAdd 30

// right 3 down 1
// # is tree
let rec countTrees total y (input: string list) =
  match input with
  | row::rows ->
      let newY = addToRow 3 y
      if row.[y] = '#'
        then countTrees (total+1) newY rows
        else countTrees total newY rows
  | _ -> total

getFile
  |> Array.toList
  |> countTrees 0 0
  |> Console.WriteLine // 286

// let countTrees (input: string list) =
//   let mutable total = 0;
//   let mutable y = 0;
//   for row in input.Tail do
//     y <- addToRow 3 y
//     printfn "On row %A, y is %A, val is %A" row y row.[y]
//     if row.[y] = '#' then total <- total + 1
//   total
