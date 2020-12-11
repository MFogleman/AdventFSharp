open System
open System.IO

type Space = Empty | Occupied | Floor

let ts1 = File.ReadAllLines "day11_input.txt"

let buildSeats seat =
  match seat with
    | 'L' -> Empty
    | '.' -> Floor
    | '#' -> Occupied
    | _ -> failwith "unexpected character"

let buildRows row  =
  Seq.map buildSeats row |> Seq.toList

type Location = Top | Bottom | Left | Right | TopRight | TopLeft | BottomRight | BottomLeft | NotEdge

let determineLocation (chart: Space list list) (rowNum: int) (colNum: int) =
  let bottom = (List.length chart) - 1
  let right = (List.length chart.[0]) - 1

  match (rowNum, colNum) with
  | (0, 0) -> TopLeft
  | (0, r) when r = right -> TopRight
  | (b, 0) when b = bottom -> BottomLeft
  | (b, r) when (b = bottom && r = right) -> BottomRight
  | (0, _) -> Top
  | (b, _) when (b = bottom) -> Bottom
  | (_, 0) -> Left
  | (_, r) when (r = right) -> Right
  | _ -> NotEdge

let buildAdjacent (chart: Space list list) row col location =
  let bottom = (List.length chart) - 1
  let right = (List.length chart.[0]) - 1
  match location with
  | TopLeft -> [                chart.[0].[1];
                  chart.[1].[0];chart.[1].[1];]

  | TopRight -> [chart.[0].[right-1];
                 chart.[1].[right-1];chart.[1].[right];]

  | BottomLeft -> [chart.[bottom-1].[0]; chart.[bottom-1].[1];
                                         chart.[bottom].[1]]
  | BottomRight -> [chart.[bottom-1].[right-1];chart.[bottom-1].[right];
                    chart.[bottom].[right-1];]
  | Top -> [
      chart.[0].[col-1];                chart.[0].[col+1];
      chart.[1].[col-1];chart.[1].[col];chart.[1].[col+1];
    ]
  | Bottom -> [ chart.[bottom-1].[col-1]; chart.[bottom-1].[col]; chart.[bottom-1].[col+1];
                chart.[bottom].[col-1];                           chart.[bottom].[col+1];]
  | Left -> [chart.[row-1].[0]; chart.[row-1].[1];
                                chart.[row].[1];
            chart.[row+1].[0]; chart.[row+1].[1]]
  | Right -> [chart.[row-1].[right-1]; chart.[row-1].[right];
              chart.[row].[right-1];
              chart.[row+1].[right-1]; chart.[row+1].[right]]
  | NotEdge -> [chart.[row-1].[col-1]; chart.[row-1].[col]; chart.[row-1].[col+1];
              chart.[row].[col-1];                          chart.[row].[col+1];
                chart.[row+1].[col-1]; chart.[row+1].[col]; chart.[row+1].[col+1]]


let appRulesToSeats chart rowNum colNum seat =
  let location = determineLocation chart rowNum colNum
  let adjacentSpaces = buildAdjacent chart rowNum colNum location
  let current = chart.[rowNum].[colNum]
  if current = Empty && not (List.contains(Occupied) adjacentSpaces) then
    Occupied
  else if current = Occupied && ( List.filter(fun x -> x = Occupied ) adjacentSpaces |> List.length |> (fun n -> n >= 4) ) then
    Empty
  else current


let appRulesToRow chart rowNum row =
  List.mapi (appRulesToSeats chart rowNum) row

let eq a1 a2 = a1 = a2

let testInner r1 r2 = List.forall2 eq r1 r2


let printSeat seat =
   match seat with
   | Empty -> 'L'
   | Floor -> '.'
   | Occupied -> '#'

let printRow row =
  List.map printSeat row
  |> fun x -> String.Join("", x)

let prettyPrint chart =
  List.map printRow chart
  |> fun x -> String.Join("\n", x)

let rec recurseTilSame chart =
  let newChart = List.mapi (appRulesToRow chart) chart
  let prettyOld = prettyPrint chart
  let prettyNew = prettyPrint newChart

  if prettyOld = prettyNew then
    printfn "done"
    newChart
    else
    printfn "testing next"
    recurseTilSame newChart



ts1
  |> Array.map buildRows |> Array.toList
  |> (fun chart -> List.mapi (appRulesToRow chart) chart)
  |> recurseTilSame
  |> prettyPrint
  |> String.filter (fun c -> c = '#')
  |> String.length
  |> Console.WriteLine

