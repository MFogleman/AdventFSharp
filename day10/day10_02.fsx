open System
open System.IO
(*
  I admittedly am still a little lost on the math behind this.  This is largely an implementation of
  pseale's solution from https://github.com/pseale/advent-of-code/blob/main/src/day10/src/index.test.js
*)

let getFile = File.ReadAllLines "day10_input.txt"

let prepend0AppendFinal (adapters: int list) =
  let newLast = List.last adapters |> (fun n -> n + 3)
  [0]@adapters@[newLast]

let TRIBONACCI = [| 1; 1; 2; 4; 7; 13; 24; 44; 81; 149; |]

let getTrib num =
  TRIBONACCI.[num-1] |> uint64

let rec solver (combos: uint64) streak (adapters: int list) =
  match adapters with
  |adapter::remaining -> if List.contains (adapter+1) adapters
                          then solver combos (streak + 1) remaining
                          else solver (combos * (getTrib streak) ) 1 remaining
  |_ -> combos

getFile
  |> Array.map (int)
  |> Array.sort
  |> Array.toList
  |> prepend0AppendFinal
  |> solver ( 1|> uint64 ) 1
  |> Console.WriteLine
// 129586085429248

