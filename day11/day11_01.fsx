open System
open System.IO

type Space = Empty | Occupied | Floor
type Layout = Space [,]

let map2DWithState fn arr = Array2D.mapi (fn arr) arr

let buildSpace c =
  match c with
  | '.' -> Floor
  | '#' -> Occupied
  | 'L' -> Empty
  | _ -> failwith "Unexpected character"
let printChar space =
  match space with
  | Floor -> '.'
  | Occupied -> '#'
  | Empty -> 'L'

let getFrom (raw: char [] []) x y = buildSpace raw.[x].[y]

let getAdjacentCoords x y =
  [
   (x-1, y-1); (x-1, y); (x-1, y+1);
   (x,   y-1);           (x,   y+1);
   (x+1, y-1); (x+1, y); (x+1, y+1);
  ]

let getInBoundsSpace (mapState: Layout) (x, y) =
  try
    Some (Array2D.get mapState x y)
  with :? IndexOutOfRangeException ->
    None

let isOccupied s =
  match s with
  | Some Occupied -> 1
  | _ -> 0

let countAdjacentOccupied (mapState: Layout) x y  =
  getAdjacentCoords x y
  |> List.map (getInBoundsSpace mapState)
  |> List.sumBy isOccupied


let parseSpace (mapState: Layout) (x: int) (y: int) (space: Space) =
  match space with
  | Empty -> if (countAdjacentOccupied mapState x y) = 0
             then Occupied
             else Empty

  | Floor -> Floor
  | Occupied -> if (countAdjacentOccupied mapState x y) >= 4
                then Empty
                else Occupied

let rec parseUntilStable layout =
  let newLayout = map2DWithState parseSpace layout
  if newLayout = layout
    then newLayout
    else parseUntilStable newLayout

let countOccupied layout =
  let mutable total = 0
  Array2D.iter(fun s -> if s = Occupied then total <- total+1) layout
  total

File.ReadAllLines "day11_input.txt"
|> Array.map (fun str -> str.ToCharArray())
|> (fun raw -> (Array2D.init (Array.length raw) (Array.length raw.[0]) (getFrom raw)): Layout )
|> parseUntilStable
|> countOccupied
|> printfn "%A"
