open System
open System.IO

(*
    Once again, my poor math skills are failing me.  As this is intended to be an exercise in F#,
    and not math, I've essentially ported the answer from 
    https://github.com/andreamazzatxt/AdventOfCode2020/blob/main/13/13.js into F#
*)

type Bus = {
    Id: uint64
    Idx: uint64
}
type State = { Tts: uint64; Mult: uint64}

let getSomes = Array.choose id

let buildBus idx id =
    match id with 
    | "x" -> None
    | _ -> Some { Id = uint64 id; Idx = uint64 idx }

let rec multUntilDone state bus =
    match (state.Tts + bus.Idx) % bus.Id = uint64 0 with
    | true -> { Mult = state.Mult * bus.Id; Tts = state.Tts }
    | false -> multUntilDone { Tts = state.Tts + state.Mult; Mult = state.Mult } bus

File.ReadAllLines "day13_input.txt"
  |> Array.last
  |> fun str -> str.Split(',')
  |> Array.mapi buildBus
  |> getSomes
  |> Array.fold multUntilDone { Tts = uint64 0; Mult = uint64 1 }
  |> fun data -> printfn "%A" data.Tts
// 305068317272992UL