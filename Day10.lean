import Util
namespace Day10

inductive Instruction : Type where
  | noop : Instruction
  | addx : Int → Instruction
deriving Repr

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

def registerValues (instructions : List Instruction) : List (Nat × Int) :=
  let rec helper : List Instruction → State (Nat × Int) (List (Nat × Int))
    | [] => ok []
    | instruction :: instructions => do
      let (cycle, regX) ← getState
      match instruction with
      | .noop =>
        setState (cycle + 1, regX)
        let rest ← helper instructions
        ok ([(cycle + 1, regX)] ++ rest)
      | .addx k =>
        setState (cycle + 2, regX + k)
        let rest ← helper instructions
        ok ([(cycle+1, regX), (cycle+2, regX)] ++ rest)
  (helper instructions (0,1)).snd

def sample := ["addx 16", "addx -11", "noop", "noop", "addx 21", "addx -15"]
def ins := parseInput sample
#eval registerValues ins

def drawCRT : List (Nat × Int) → String 
  | [] => ""
  | (cycle, regX) :: rest =>
    let foo := (cycle - 1) % 40
    let c := if foo == regX - 1 || foo == regX || foo == regX + 1 then "#" else "."
    let mbNewline := if foo == 0 then "\n" else ""
    mbNewline ++ c ++ (drawCRT rest)
      

def computeSignalStrengths : List (Nat × Int) → Option (Int) := fun regXValues => do
  let foo := regXValues.toArray
  let fst' ← foo.findIdx? (fun (cycle, _) => cycle > 20)
  let snd' ← foo.findIdx? (fun (cycle, _) => cycle > 60)
  let thd' ← foo.findIdx? (fun (cycle, _) => cycle > 100)
  let fou' ← foo.findIdx? (fun (cycle, _) => cycle > 140)
  let fif' ← foo.findIdx? (fun (cycle, _) => cycle > 180)
  let six' ← foo.findIdx? (fun (cycle, _) => cycle > 220)
  
  let (_, fst) ← foo[fst' - 1]?
  let (_, snd) ← foo[snd' - 1]?
  let (_, thd) ← foo[thd' - 1]?
  let (_, fou) ← foo[fou' - 1]?
  let (_, fif) ← foo[fif' - 1]?
  let (_, six) ← foo[six' - 1]?

  pure (fst * 20 + snd * 60 + thd * 100 + fou * 140 + fif * 180 + six * 220)




def runDay : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let lines ← readLines stdin

  let regVals := (registerValues (parseInput lines))
  stdout.putStrLn s!"{regVals}"
  let strengths := computeSignalStrengths regVals
  stdout.putStrLn s!"{strengths}"
  stdout.putStrLn s!"{drawCRT regVals}"

end Day10