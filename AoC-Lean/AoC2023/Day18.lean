import Util
import Grid

namespace Day18

open Grid

inductive Tile where
  | Trench
  | Level
deriving BEq, Hashable, Inhabited

def Tile.toString : Tile → String
  | Trench => "#"
  | Level => "."

instance : Repr Tile where
  reprPrec t _ := t.toString

instance : ToString Tile where
  toString t := t.toString

def Tile.ofChar : Char → Tile
  | '#' => .Trench
  | '.' => .Level
  | _ => panic! "Unknown tile type"

inductive Direction where
  | L
  | R
  | U
  | D
deriving Inhabited, Repr

def Instruction : Type := Direction × Nat × String

open Direction in
def Direction.ofChar : Char → Direction
  | 'L' => L
  | 'R' => R
  | 'U' => U
  | 'D' => D
  | _ => panic! "Unknown direction"

def parseLine : String → Instruction := fun s =>
  let parts := String.splitOn s " "
  let dir := Direction.ofChar ((parts.get! 0).data.get! 0)
  let distance := String.toNat! (parts.get! 1)
  let hexCode := parts.get! 2
  (dir, distance, hexCode)

def parseLines : List String → List Instruction := List.map parseLine

structure Boundaries where
  maxX : Int
  minX : Int
  maxY : Int
  minY : Int

def findBoundaries : List Instruction →  Boundaries := fun ins =>
  let rec fold (maxX minX maxY minY : Int) (x y : Int) : List Instruction → Boundaries
    | [] => {maxX := maxX, minX := minX, maxY := maxY, minY := minY}
    | (.L, dist, _) :: ins =>
      let x := x - dist
      if x < minX then
        fold maxX x maxY minY x y ins
      else
        fold maxX minX maxY minY x y ins
    | (.R, dist, _) :: ins =>
      let x := x + dist
      if maxX < x then
        fold x minX maxY minY x y ins
      else
        fold maxX minX maxY minY x y ins
    | (.U, dist, _) :: ins =>
      let y := y - dist
      if y < minY then
        fold maxX minX maxY y x y ins
      else
        fold maxX minX maxY minY x y ins
    | (.D, dist, _) :: ins =>
      let y := y + dist
      if maxY < y then
        fold maxX minX y minY x y ins
      else
        fold maxX minX maxY minY x y ins

  fold (-9999999999) 99999999999 (-99999999999) 9999999999999 0 0 ins

def dig : Grid Tile → List Instruction → Grid Tile := fun grid ins =>
  let rec fold (grid : Grid Tile) (x y : Nat) : List Instruction → Grid Tile
    | [] => grid
    | (.L, dist, _) :: ins =>
      let x := x - dist
      sorry
    | (.R, dist, _) :: ins =>
      let x := x + dist
      sorry
    | (.U, dist, _) :: ins =>
      let y := y - dist
      sorry
    | (.D, dist, _) :: ins =>
      let y := y + dist
      sorry

  let boundaries := findBoundaries ins
  let m := Int.toNat (boundaries.maxY - boundaries.minY)
  let n := Int.toNat (boundaries.maxX - boundaries.minX)
  let initialGrid : Grid Tile := Grid.fill m n Tile.Level
  fold grid (Int.toNat (-boundaries.minX)) (Int.toNat (-boundaries.minY)) ins


def runDay : List String → String := fun lns =>
  let u := parseLines lns
  let sln1 := 0
  let sln2 := 0
  s!"Part 1: {sln1}\nPart 2: {sln2}"

end Day18
