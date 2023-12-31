import Grid
import Util

namespace Day13

open Grid

inductive Tile where
  | Rock
  | Ash
deriving BEq, Hashable, Inhabited

def Tile.toString : Tile → String
  | Rock => "#"
  | Ash => "."

instance : Repr Tile where
  reprPrec t _ := t.toString

instance : ToString Tile where
  toString t := t.toString

def Tile.ofChar : Char → Tile
  | '#' => .Rock
  | '.' => .Ash
  | _ => panic! "Unknown tile type"

def arrayDistance (x y : Array Tile) : Nat :=
  let xy := Array.zip x y
  Array.foldl (fun acc (xi, yi) => if xi == yi then acc else 1 + acc) 0 xy

instance : HSub (Array Tile) (Array Tile) Nat where
  hSub := arrayDistance

def checkRowSymmetry : List (Array Tile) → List (Array Tile) → Bool
  | h1 :: t1, h2 :: t2 => h1 == h2 && checkRowSymmetry t1 t2
  | _, _ => true

def symmetryDistance : List (Array Tile) → List (Array Tile) → Nat
  | [], [] => 0
  | [], _ => 0
  | _, [] => 0
  | h1 :: t1, h2 :: t2 => (h1 - h2) + symmetryDistance t1 t2

def findRowSymmetry : Grid Tile → List Nat := fun grid =>
  let rec helper (acc : List (Array Tile)) : List (Array Tile) → List Nat
    | [] => []
    | _ :: [] => []
    | r :: rws =>
      let acc' := r :: acc
      let dist := symmetryDistance acc' rws
      dist :: helper acc' rws
  helper [] grid.toList

def findSymmetry (n : Nat) : Grid Tile → Nat := fun grid =>
  let rowSyms := findRowSymmetry grid
  match rowSyms.toArray.findIdx? (· = n) with
  | some row => (row + 1) * 100
  | none =>
    let colSyms := findRowSymmetry grid.transpose
    match colSyms.toArray.findIdx? (· = n) with
    | some col => col + 1
    | none => panic! "Did not find symmetry"


def splitInto : List String → List (List String) := fun lns =>
  let rec helper (acc : List String) : List String → List (List String)
    | [] => [acc.reverse]
    | "" :: lns => acc.reverse :: (helper [] lns)
    | ln :: lns => helper (ln :: acc) lns

  helper [] lns

def parseLines : List String → List (Grid Tile) := fun lns =>
  let splitted := splitInto lns
  let grids := splitted.map (fun data =>
    (data.map (fun s =>
      (s.data.map Tile.ofChar).toArray
    )).toArray
  )
  List.map Grid.mk grids

def runDay : List String → String := fun lns =>
  let grids := parseLines lns
  let sln1 := Util.sum 0 (grids.map (findSymmetry 0))
  let sln2 := Util.sum 0 (grids.map (findSymmetry 1))
  s!"Part 1: {sln1}\nPart 2: {sln2}"

end Day13
