open System
open System.IO

type Bus = {
  Id: int
  NextTime: int
}

let parseSchedule (str: string) =
  str.Split(',')
    |> Array.filter (fun n -> n <> "x")
    |> Array.map int

let rec findNextTime time (bus: Bus) =
  match time > bus.NextTime with
  | true -> findNextTime time { bus with NextTime = bus.NextTime + bus.Id }
  | false -> bus

let buildBus n: Bus =
  { Id = n; NextTime = n }

let solve (time, busIds) =
  Array.map (findNextTime time << buildBus) busIds
  |> Array.minBy(fun bus -> bus.NextTime)
  |> (fun bus -> (bus.NextTime - time) * bus.Id )

File.ReadAllLines "day13_input.txt"
  |> (fun arr -> int arr.[0], parseSchedule arr.[1])
  |> solve
  |> printfn "%A"