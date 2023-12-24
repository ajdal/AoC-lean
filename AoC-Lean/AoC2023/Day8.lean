import Std.Data.HashMap
import Std.Data.Nat.Gcd

namespace Day8

open Std

inductive Instruction where
  | Left
  | Right
deriving Repr, Inhabited

structure Label where
  label : String
  len3 : label.length = 3
deriving Hashable

instance : Repr Label where
  reprPrec l _ := l.label

instance : ToString Label where
  toString l := l.label

instance : Inhabited Label where
  default := { label := "AAA", len3 := by simp [String.length] }

instance : BEq Label where
  beq l1 l2 := l1.label == l2.label

structure Node where
  label : Label
  left : Label
  right : Label
deriving Repr

def Map := HashMap Label (Label × Label)

def Label.endsWith : Char → Label → Bool := fun c l =>
  have h : 2 < List.length l.label.data := by
    have h' := l.len3
    simp [String.length] at *
    simp [h']
  l.label.data[2]'h == c

def parseInstructions : List String → List Instruction × List String
  | [] => ([], [])
  | s :: ss =>
    let rec fold : List Char → List Instruction
    | [] => []
    | c :: cs =>
      if c == 'L' then
        Instruction.Left :: fold cs
      else
        Instruction.Right :: fold cs
    (fold s.data, ss.tail!)

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

def makeMap : List Node → Map := fun nodes =>
  let foo := nodes.map (fun n => (n.label, (n.left, n.right)))
  HashMap.ofList foo

partial def findFirstEnd (m : Map) (ins : List Instruction) (start : Label) : Label × Nat :=
  let ins := ins.toArray
  let len := ins.size
  let rec fold : Nat → Label → Label × Nat := fun n l =>
    if l.endsWith 'Z' then
      (l, n)
    else
      match ins[n % len]! with
      | .Left => fold (n + 1)  (HashMap.find! m l).fst
      | .Right => fold (n + 1) (HashMap.find! m l).snd
  fold 0 start

def findAllEnds (m : Map) (ins : List Instruction) (labels : List Label) : List (Label × Nat) :=
  let starts := labels.filter (Label.endsWith 'A')
  List.map (findFirstEnd m ins) starts

def startLabel : Label := { label := "AAA", len3 := by simp [String.length] }

def lcm : List Nat → Nat
  | [] => 1
  | n :: ns => Nat.lcm n (lcm ns)

def runDay : List String → String := fun lns =>
  let (ins, nodes) := parseLines lns
  let map := makeMap nodes
  let labels := nodes.map (fun n => n.label)
  let sln1 := (findFirstEnd map ins startLabel).2
  let sln2 := (findAllEnds map ins labels)
  let vals := sln2.map (fun x => x.2)
  s!"Part 1: {sln1}\nPart 2: {sln2}\nInstruction length: {ins.length}\nlcm: {lcm vals}"

end Day8
