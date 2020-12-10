open System
open System.IO
open System.Text.RegularExpressions



type Acc = string
type Jmp = string
type Nop = string
type Operation = Acc | Jmp | Nop

type Idx = int
type Argument = int
type Accumulator = int

type Instruction = {
  Operation: Operation
  Argument: Argument
  Idx: Idx
}

type Program = Instruction list

type State = {
  History: Idx list
  Accumulator: Accumulator
}

let defaultState = {
  History = []
  Accumulator = 0
}

let (|Acc|Jmp|Nop|) str =
  match str with
  | "acc" -> Acc
  | "jmp" -> Jmp
  | "nop" -> Nop
  | _ -> failwith "Invalid operator"

let getFile = File.ReadAllLines "day08_input.txt"

let testOp str =
  match str with
  |Acc -> Acc
  |Jmp -> Jmp
  |Nop -> Nop

let parseOpAndArg (str: string): Operation * Argument =
  let arr = str.Split([|' '|])
  let op = testOp arr.[0]
  let arg = arr.[1] |> int
  (op, arg)

let toInstruction (idx: int) (str: string) =
  let arr = str.Split([|' '|])
  let operation = testOp arr.[0]
  let argument = arr.[1] |> int
  {
   Operation = operation
   Argument = argument
   Idx = idx
  }

let rec runProgram (state: State) (instruction: Instruction) (program: Program) =
  if List.exists (fun histIdx -> histIdx = instruction.Idx) state.History then
    failwith (sprintf "Entered infinite loop, current state is %A" state)

  let newHistory = instruction.Idx::state.History

  if instruction.Operation = Acc then
    let newState = {
      History = newHistory
      Accumulator = state.Accumulator + instruction.Argument
    }
    let newIdx = instruction.Idx + 1
    runProgram newState program.[newIdx] program

  if instruction.Operation = Nop then
    let newState = {
      History = newHistory
      Accumulator = state.Accumulator
    }
    let newIdx = instruction.Idx + 1
    runProgram newState program.[newIdx] program

  if instruction.Operation = Jmp then
    let newState = {
      History = newHistory
      Accumulator = state.Accumulator
    }
    let newIdx = instruction.Idx + instruction.Argument
    runProgram newState program.[newIdx] program


let initProgram program =
  match program with
  | (x::xs) -> runProgram defaultState x program
  | _ -> failwith "could not start program"

getFile
  |> Array.toList
  |> List.mapi toInstruction
  |> initProgram // 1217
