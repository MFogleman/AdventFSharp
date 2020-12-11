open System
open System.IO
(*
  I admittedly am still a little lost on the math behind this.  This is largely an implementation of
  pseale's solution from https://github.com/pseale/advent-of-code/blob/main/src/day10/src/index.test.js

*)
let getFile = File.ReadAllLines "day10_input.txt"

let prepend0AppendFinal (lst: int list) =
  let newLast = List.last lst |> (fun n -> n + 3)
  [0]@lst@[newLast]

let TRIB = [| 1; 1; 2; 4; 7; 13; 24; 44; 81; 149; |]

let getTrib num =
  TRIB.[num-1]

let rec solver factor streak (adapters: int list) =
  match adapters with
  |x::xs -> if List.contains (x+1) adapters
              then solver factor (streak + 1) xs
              else solver (factor * (getTrib streak) ) 1 xs
  |_ -> factor


getFile
  |> Array.map (int)
  |> Array.sort
  |> Array.toList
  |> prepend0AppendFinal
  |> solver 1 1
  |> printfn "debug:: %A"

// 2376