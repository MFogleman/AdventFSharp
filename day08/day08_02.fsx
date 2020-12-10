open System
open System.IO
open System.Text.RegularExpressions

type Acc = Acc of string
type Jmp = Jmp of string
type Nop = Nop of string

type Operation = Acc | Jmp | Nop
type Idx = int
type Argument = int
type Accumulator = int

type Instruction = {
  Operation: Operation
  Argument: Argument
  Idx: Idx
}

type State = {
  History: Idx list
  Accumulator: Accumulator
}

type Program = Instruction list

let defaultState = {
  History = []
  Accumulator = 0
}

let getFile = File.ReadAllLines "day08_input.txt"

let parseOp str =
  match str with
  |"acc" -> Acc
  |"jmp" -> Jmp
  |"nop" -> Nop
  | _ -> failwith "invalid op"

let toInstruction (idx: int) (str: string) =
  let arr = str.Split([|' '|])
  let operation = parseOp arr.[0]
  let argument = arr.[1] |> int
  {
   Operation = operation
   Argument = argument
   Idx = idx
  }

let parseState (state, instruction) =
  {
    History = instruction.Idx::state.History
    Accumulator = match instruction.Operation with
                  | Acc -> state.Accumulator + instruction.Argument
                  | _   -> state.Accumulator
  }

let parseIdx instruction =
  match instruction.Operation with
  | Jmp -> instruction.Idx + instruction.Argument
  | _   -> instruction.Idx + 1

exception InfiniteException of string

let rec runProgram (state: State) (instruction: Instruction) (program: Program) =
  if List.exists (fun histIdx -> histIdx = instruction.Idx) state.History then
    raise (InfiniteException(sprintf"Entered infinite loop, current state is %A" state))

  let (newState, newIdx) = ( parseState(state, instruction), (parseIdx instruction) )

  let newInst = List.tryItem newIdx program

  match newInst with
  | None -> newState.Accumulator
  | Some newInst  -> runProgram newState newInst program

let initProgram program =
  runProgram defaultState (List.head program) program

let toggleNopJmp operator =
  match operator with
  |Nop -> Jmp
  |Jmp -> Nop
  |_ -> operator


let rec bruteForceFix checkedIdxs (program: Instruction list)  =
  let swpIdx = (List.findIndex (fun inst ->
    (inst.Operation = Nop || inst.Operation = Jmp ) && inst.Idx > checkedIdxs
  ) program )

  let swpVal = program.[swpIdx]
  let newVal = { swpVal with Operation = (toggleNopJmp swpVal.Operation) }

  let beforeVal = program.[0..swpIdx-1]
  let afterVal = program.[swpIdx+1..]

  let newProgram = beforeVal@[newVal]@afterVal

  try
    let x = initProgram newProgram
    printfn "works when we fix idx %A " swpIdx // 198
    x
  with InfiniteException(str) ->
    bruteForceFix swpIdx program

getFile
  |> Array.toList
  |> List.mapi toInstruction
  |> bruteForceFix 0
  |> printfn "Program finished with %A" // 501
