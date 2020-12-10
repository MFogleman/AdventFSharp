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

let rec runProgram (state: State) (instruction: Instruction) (program: Program) =
  if List.exists (fun histIdx -> histIdx = instruction.Idx) state.History then
    failwith (sprintf "Entered infinite loop, current state is %A" state)

  let (newState, newIdx) = ( parseState(state, instruction), (parseIdx instruction) )
  runProgram newState program.[newIdx] program

let initProgram program =
  runProgram defaultState (List.head program) program

getFile
  |> Array.toList
  |> List.mapi toInstruction
  |> initProgram // 1217
