import Util
import Lean
import Qq

namespace Grid

open Lean

def Grid (α : Type) : Type :=
  Array (Array α)

def helper : TSyntaxArray `num -> Array Nat := fun ns =>
  ns.map (fun n => n.getNat)

#check mkAppN

syntax "g[" sepBy(num*, ";") "]" : term
macro_rules
| `(g[ $[$nns:num*];* ]) => do
    let foo := Array.foldl (fun prev ns => prev ++ helper ns) #[] nns
    `($foo)

-- #check g[ 1 2 ; 1 2 ; 3 4 ]

def Grid.get? : Grid α → Nat → Nat → Option α :=
  fun g i j => do
    let row ← Array.get? g i
    let el ← row[j]?
    el

def Grid.getRowD : Grid α → Nat → Array α :=
  fun g i => 
    match Array.get? g i with
    | none => #[]
    | some row => row

def Grid.set : Grid α → Nat → Nat → α → Grid α :=
  fun g i j x => 
    match Array.get? g i with
    | none => g
    | some row =>
      let newRow := Array.setD row j x
      Array.setD g i newRow

-- Creates Grid α of size m x n filled with default
def Grid.make (m n : Nat) (default : α) : Grid α :=
  mkArray m (mkArray n default)

def Grid.toString [ToString α] : Grid α → String := fun g =>
  g.foldl (fun s r => 
    s ++ (r.foldl (fun s' c => s' ++ s!"{c}") "") ++ "\n"
  ) ""

instance [ToString α] : ToString (Grid α) where
  toString := Grid.toString

def Grid.getCol : Grid α → Nat → Array α := fun g col =>
  let rec helper (g : Grid α) (col : Nat) : List Nat → List α
    | [] => []
    | i :: is =>
      let opt := g.get? i col
      match opt with
      | none => []
      | some x => [x] ++ (helper g col is)
  let row := Util.range 0 (g.size)
  (helper g col row).toArray

def Grid.getRow : Grid α → Nat → Array α := fun g row =>
  let rec helper (g : Grid α) (row : Nat) : List Nat → List α
    | [] => []
    | j :: js =>
      let opt := g.get? row j
      match opt with
      | none => []
      | some x => [x] ++ (helper g row js)
  match Array.get? g 0 with
  | none => #[]
  | some r₀ =>
    let col := Util.range 0 (r₀.size)
    (helper g row col).toArray

end Grid