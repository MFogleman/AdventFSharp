open System
open System.IO

type State = {
  Deg: int
  Y: int
  X: int
}

let starting = {
  Deg = 90
  Y = 0
  X = 0
}

let calcFacing amnt =
  if amnt >= 360 then
    amnt - 360
  else if amnt < 0 then
    360 + amnt
  else amnt


let calcMove (state: State) amnt =
  match state.Deg with
  | 0 -> { state with Y = state.Y + amnt}
  | 90 -> { state with X = state.X + amnt }
  | 180 -> { state with Y = state.Y - amnt }
  | 270 -> { state with X = state.X - amnt }
  | _ ->  failwithf "Unexpected degree %A and amnt %A" state amnt

let move (state: State) action amnt =
  match action with
  | 'N' -> { state with Y = state.Y + amnt }
  | 'E' -> { state with X = state.X + amnt }
  | 'S' -> { state with Y = state.Y - amnt }
  | 'W' -> { state with X = state.X - amnt }
  | 'R' -> { state with Deg = calcFacing (state.Deg + amnt) }
  | 'L' -> { state with Deg = calcFacing (state.Deg - amnt) }
  | 'F' -> calcMove state amnt
  | _ -> failwith "Unexpected direction"

let parseInput (state: State) (str: string) =
  let c = str.[0]
  let num = str.[1..] |> int
  move state c num

let calcManhattan state =
  Math.Abs state.Y + Math.Abs state.X

File.ReadAllLines "day12_input.txt"
|> Array.fold parseInput starting
|> calcManhattan
|> printfn "%A" // 1601
