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

let getFrom (raw: char [] []) x y = buildSpace raw.[x].[y]

let dec n = n - 1
let inc n = n + 1
let I n = n
let thrush (x, y) (fX, fY) = (fX x, fY y)
let biMap (fA, fB) (a, b) = (fA a, fB b)

let coordFns = [
   (dec , dec ); (dec , I ); (dec , inc );
   (I ,   dec );               (I ,   inc );
   (inc , dec ); (inc , I ); (inc , inc );
  ]

let isOccupied s =
  match s with
  | Some Occupied -> 1
  | _ -> 0

let safeCheck (mapState: Layout) x y =
  try
    Some (Array2D.get mapState x y)
    with :? IndexOutOfRangeException ->
    None

let rec checkTilNoneOrOcc (mapState: Layout) idx (x, y) =
  match safeCheck mapState x y with
  | Some Occupied -> Some Occupied
  | None -> None
  | Some Empty -> Some Empty
  | Some Floor -> checkTilNoneOrOcc mapState idx (biMap coordFns.[idx] (x, y))

let countVisibleOccupied (mapState: Layout) (x: int) (y: int) =
  List.map (thrush (x, y)) coordFns
  |> List.mapi (checkTilNoneOrOcc mapState)
  |> List.sumBy isOccupied

let parseSpace (mapState: Layout) (x: int) (y: int) (space: Space) =
  match space with
  | Floor -> Floor
  | Empty -> if (countVisibleOccupied mapState x y) = 0
             then Occupied
             else Empty
  | Occupied -> if (countVisibleOccupied mapState x y) >= 5
                then Empty
                else Occupied

let rec parseUntilStable (layout: Layout) =
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
// 1990