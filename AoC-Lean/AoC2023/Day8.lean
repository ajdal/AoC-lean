import Std.Data.HashMap

namespace Day8

inductive Instruction where
  | Left
  | Right
deriving Repr

structure Label where
  label : String
  len3 : label.length = 3
deriving Repr

structure Node where
  label : Label
  left : Label
  right : Label
deriving Repr

#check Std.HashMap

def parseInstructions : List String → List Instruction × List String
  | [] => ([], [])
  | s :: ss =>
    let rec fold (ins : List Instruction) : List Char → List Instruction
    | [] => ins
    | c :: cs =>
      if c == 'L' then
        fold (Instruction.Left :: ins) cs
      else
        fold (Instruction.Right :: ins) cs
    (fold [] s.data, ss.tail!)

def parseNodes : List String → List Node := fun ss =>
  let rec fold (ns : List Node) : List String → List Node
  | [] => ns
  | s :: ss =>
    let s := s.data
    let lbl := [s[0]!, s[1]!, s[2]!]
    let lft := [s[7]!, s[8]!, s[9]!]
    let rght := [s[12]!, s[13]!, s[14]!]
    let node : Node := {
      label := { label := String.mk lbl, len3 := by unfold String.length; simp}
      left := { label := String.mk lft, len3 := by unfold String.length; simp}
      right := { label := String.mk rght, len3 := by unfold String.length; simp}
    }
    fold (node :: ns) ss
  fold [] ss

def parseLines : List String → List Instruction × (List Node) :=
  fun ss =>
    let (ins, ss) := parseInstructions ss
    (ins, parseNodes ss)

def runDay : List String → String := fun lns =>
  let data := parseLines lns
  let sln1 := 0
  let sln2 := 0
  s!"Part 1: {sln1}\nPart 2: {sln2}"

end Day8
