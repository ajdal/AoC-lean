import Util
import Lean
import Qq

namespace Grid

def Grid (α : Type) : Type :=
  Array (Array α)

def Grid.mk {α : Type} (data : Array (Array α)) : Grid α := data

def Grid.mk? {α : Type} (data : Array (Array α)) : Option (Grid α) :=
  match data.get? 0 with
  | none => some data
  | some row0 =>
    if Array.all data (fun row => row.size == row0.size) then
      some data
    else
      none

-- Creates Grid α of size m x n filled with default
def Grid.fill (m n : Nat) (default : α) : Grid α :=
  mkArray m (mkArray n default)

def Grid.toString [ToString α] : Grid α → String := fun g =>
  g.foldl (fun s r =>
    let row := " ".joinSep (r.map ToString.toString).toList
    s ++ row ++ "\n"
  ) ""

instance [ToString α] : ToString (Grid α) where
  toString := Grid.toString

instance [ToString α] : Repr (Grid α) where
  reprPrec g _ := g.toString

def Grid.numRows : Grid α → Nat := fun grid =>
  Array.size grid

def Grid.dimensions : Grid α → Nat × Nat := fun grid =>
  match grid.get? 0 with
  | none => (0,0)
  | some row0 => (Array.size grid, row0.size)

section Parser
open Lean Elab Macro Tactic

/--
  Define a matlab-like notation for grids.
  We can defined a grid as e.g. [[ 1 2 ; 3 4 ]]
  for a grid with two rows and two columns.

  We don't allow the creating of empty grids with this syntax
-/
syntax (name := gridNotation) "[[" sepBy1(sepBy1(term, ","), ";") "]]" : term
syntax (name := gridNotationOnlyCommas) "[[" ","+ "]]" : term
syntax (name := gridNotationOnlySemiColons) "[[" ";"* "]]" : term
macro_rules
| `([[ $[$[$rows],*];* ]]) => do
    let m := rows.size
    let n := if h : 0 < m then rows[0].size else 0
    let rowVecs ← rows.mapM fun row : Array (TSyntax `term) => do
      if row.size != n then
        Macro.throwErrorAt (mkNullNode row)
          s!"Rows must be of equal length; this row has {row.size} items, the previous rows {"
          "}have {n}"
      else
        `(#[ $[$row],* ])
    `(Grid.mk #[ $rowVecs,* ])
  | `([[ $[;%$semicolons]* ]]) =>
      Macro.throwError "Cannot create empty Grid"
  | `([[$[,%$commas]* ]]) =>
      Macro.throwError "Cannot create empty Grid"

end Parser

def Grid.get? : Grid α → Nat → Nat → Option α :=
  fun g i j => do
    let row ← Array.get? g i
    let el ← row[j]?
    el

def Grid.get! {α : Type} [Inhabited α] : Grid α → Nat → Nat → α :=
  fun g i j =>
    let row := Array.get! g i
    row[j]!

/--
  Return the `n`-th row, or any empty array if `n` is not a
  valid index array.
-/
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

def Grid.transpose : Grid α → Grid α := fun g =>
  let m := (g.getRow 0).size
  let cols := List.foldl (fun cols i => g.getCol i :: cols) [] (Util.range 0 m)
  Grid.mk (cols.reverse.toArray)

end Grid
