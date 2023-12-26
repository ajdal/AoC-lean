import Grid

namespace Day11

open Grid

inductive Tile where
  | Space
  | Galaxy
deriving Repr, BEq, Inhabited

def Tile.toString : Tile → String
  | Space => "."
  | Galaxy => "#"
instance : Repr Tile where
  reprPrec t _ := t.toString

instance : ToString Tile where
  toString t := t.toString

def Tile.ofChar : Char → Tile
  | '.' => .Space
  | '#' => .Galaxy
  | _ => .Space

def onlySpace : Array Tile → Bool := fun row =>
  row.all (· == .Space)

def findEmptyRows : Grid Tile → Array Nat := fun u =>
  ((u.mapi (fun i row => (i, onlySpace row))).filter (
    fun (_, b) => b
  )).map Prod.fst

def duplicateRows : Grid Tile → Array Nat → Grid Tile := fun u rows =>
  Array.foldl (fun u i =>
    Array.insertAt! u i (Grid.getRow u i)
  ) u rows

def expandUniverse (u : Grid Tile) : Grid Tile :=
  let emptyRowIdxs := findEmptyRows u
  let emptyColIdxs := findEmptyRows (u.transpose)
  let u := duplicateRows u emptyRowIdxs.reverse
  let u := (duplicateRows u.transpose emptyColIdxs.reverse).transpose
  u

def findGalaxies (u : Grid Tile) : List (Nat × Nat) :=
  let pairs := (u.mapi (fun i row =>
    row.mapi (fun j t =>
      ((i,j), t == .Galaxy)
    )
  ))
  let galxs := Array.foldl (fun acc x => (x.filter (fun (_, b) => b)).toList ++ acc) [] pairs
  galxs.map Prod.fst

def manhattanDistance : Nat × Nat → Nat × Nat → Nat :=
  fun (i,j) (i',j') => (max i' i - min i' i) + (max j' j - min j' j)

def allDistances : List (Nat × Nat) → Nat
  | [] => 0
  | glx :: glxs =>
    Util.sum 0 (glxs.map (manhattanDistance glx)) + allDistances glxs

def expandGalaxies (k : Nat) (emptyRows emptyCols : Array Nat) :
  List (Nat × Nat) → List (Nat × Nat)
  | [] => []
  | (i,j) :: glxs =>
    let smallerRows := (emptyRows.filter (fun r => r < i)).size
    let smallerCols := (emptyCols.filter (fun r => r < j)).size
    let glx := (i + smallerRows * (k-1), j + smallerCols * (k-1))
    glx :: expandGalaxies k emptyRows emptyCols glxs

def parseLines : List String → Grid Tile :=
  fun lines =>
    let rec fold
    | [] => []
    | ln :: lns =>
      let chars := ln.data
      let x := List.map Tile.ofChar chars
      let xa := List.toArray x
      xa :: (fold lns)
    let foo := fold lines
    let foo' := List.toArray foo
    Grid.mk foo'

def runDay : List String → String := fun lns =>
  let u := parseLines lns
  let sln1 := allDistances (findGalaxies (expandUniverse u))
  let rows := findEmptyRows u
  let cols := findEmptyRows u.transpose
  let distances := expandGalaxies 1000000 rows cols (findGalaxies u)
  let sln2 := allDistances distances
  s!"Part 1: {sln1}\nPart 2: {sln2}"

end Day11
