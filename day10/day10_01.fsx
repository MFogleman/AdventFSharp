open System
open System.IO

let getFile = File.ReadAllLines "day10_input.txt"

type One = One of int
type Two = Two of int
type Three = Three of int
type Diff = One | Two | Three
type State = {
  One  : int
  Two  : int
  Three: int
  Top  : int
}

let defaultState = {
  One   = 0;
  Two   = 0;
  Three = 1; // account for device adapter
  Top   = 0;
}

let parseDiff num =
  match num with
  |1 -> One
  |2 -> Two
  |3 -> Three
  |_ -> failwith "Unexpected diff"

let countDiffs state num =
  match num - state.Top with
  |1 -> { state with One = state.One + 1; Top = num; }
  |2 -> { state with Two = state.Two + 1; Top = num; }
  |3 -> { state with Three = state.Three + 1; Top = num; }
  |_ -> failwith "Unexpected diff"

let multOnesAndThrees state =
  state.One * state.Three

getFile
  |> Array.map (int)
  |> Array.sort
  |> Array.fold countDiffs defaultState
  |> multOnesAndThrees
  |> Console.WriteLine
// 2376