open System
open System.IO

let getFile = File.ReadAllLines "day09_input.txt"

type Data = {
  Idx: int
  Num: uint64
}

let hasSum (lst: Data list ) (targetData: Data) =
  let target = targetData.Num

  (List.tryFind (fun d1 ->
    (List.exists(fun d2 ->
      d1.Num + d2.Num = target && d1.Idx <> d2.Idx
    ) lst)
  ) lst)

let PREAMBLE = 25
let WINDOW = PREAMBLE+1

let mapSnd (fn: 'B -> 'C) (a: 'a, b: 'B) = (a, fn(b))

let unCur2 f (a, b) = f a b
let tupHasSum = unCur2 hasSum

let sumOfBodyEqTail (window: Data list) =
  List.splitAt PREAMBLE window
  |> mapSnd List.exactlyOne
  |> (fun tup -> match tupHasSum tup with
                  | None -> true
                  | _ -> false)

let worseScript (target) (nums: Data list ) =
  [for i in [0.. (nums.Length - 2)] do
    [for j in [i.. (nums.Length - 1)] do
      let newArr = nums.[i..j]
      if (List.sumBy(fun (d: Data) -> d.Num) newArr) |> int = target then
        let min = List.minBy(fun d -> d.Num) newArr
        let max = List.maxBy(fun d -> d.Num) newArr

        let minA = min.Num |> int
        let maxA = max.Num |> int
        let output = minA + maxA
        printfn "output:: %A" output
        // output:: 16773507 42568378 59341885
    ]
  ]

//part1
// getFile
//   |> Array.toList
//   |> List.mapi (fun (idx: int) (num: string) -> { Idx = idx|>int; Num = Convert.ToUInt64(num)})
//   |> List.windowed WINDOW
//   |> List.find(sumOfBodyEqTail) // 393911906
//   |> List.last // get target of window
//   |> Console.WriteLine


part2
getFile
  |> Array.toList
  |> List.mapi (fun (idx: int) (num: string) -> { Idx = idx|>int; Num = Convert.ToUInt64(num)})
  |> worseScript 393911906
  |> Console.WriteLine // 59341885


