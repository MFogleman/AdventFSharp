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
  Three = 0;
  Top   = 0;
}

let countDiffs state num =
  match num - state.Top with
  |1 -> { state with One = state.One + 1; Top = num; }
  |2 -> { state with Two = state.Two + 1; Top = num; }
  |3 -> { state with Three = state.Three + 1; Top = num; }
  |_ -> failwith "Unexpected diff"

let multOnesAndThrees state =
  state.One * state.Three

let addDeviceBuiltInAdapter state =
  { state with Three = state.Three + 1; Top = state.Top + 3}

getFile
  |> Array.map (int)
  |> Array.sort
  |> Array.fold countDiffs defaultState
  |> addDeviceBuiltInAdapter
  |> multOnesAndThrees
  |> Console.WriteLine
// 2376