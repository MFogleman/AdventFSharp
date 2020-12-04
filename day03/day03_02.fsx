#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"

open System
open System.IO

let getFile = File.ReadAllLines "day03_input.txt"

let multiply a b: bigint = a * b
let juxt arr arg =
  List.map (fun f -> f arg) arr

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

let nextRow (rows: string list) down =
  match down with
    | 1 -> rows
    | 2 -> if rows.Length >= 2
            then rows.Tail
            else []
    | _ -> rows

// # is tree
let rec countTrees  right down total y (input: string list) =
  match input with
  | row::rows ->
      let newY = addToRow right y
      if row.[y] = '#'
        then countTrees right down (total+1) newY (nextRow rows down)
        else countTrees right down total newY (nextRow rows down)
  | _ -> total |> bigint


getFile
  |> Array.toList
  |> juxt [countTrees 1 1 0 0; countTrees 3 1 0 0; countTrees 5 1 0 0; countTrees 7 1 0 0; countTrees 1 2 0 0]
  // |> List.map (fun n -> n |> bigint)
  |> List.reduce multiply // 60 * 286 * 76 * 62 * 45
  |> Console.WriteLine // 3638606400

// let countTrees (input: string list) =
//   let mutable total = 0;
//   let mutable y = 0;
//   for row in input.Tail do
//     y <- addToRow 3 y
//     printfn "On row %A, y is %A, val is %A" row y row.[y]
//     if row.[y] = '#' then total <- total + 1
//   total
