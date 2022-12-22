import Util
namespace Day10

inductive Instruction : Type where
  | noop : Instruction
  | addx : Int → Instruction


def parseInput (lines : List String) : List Instruction :=
  match lines with
  | [] => []
  | line :: lines => 
    let temp := line.splitOn " "
    match temp[0]? with
    | none => []
    | some "noop" => ([.noop]) ++ parseInput lines
    | some "addx" => 
      match temp[1]? with
      | none => []
      | some n =>
        match n.toInt? with
        | none => []
        | some k => [.addx k] ++ parseInput lines
    | _ => []

def computeSignalStrength (instructions : List Instruction) : Int :=
  let rec helper : List Instruction → State (Nat × Int) Int := 
    10

  10


end Day10