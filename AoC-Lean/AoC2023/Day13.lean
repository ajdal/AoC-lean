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

def checkRowSymmetry : List (Array Tile) → List (Array Tile) → Bool
  | h1 :: t1, h2 :: t2 => h1 == h2 && checkRowSymmetry t1 t2
  | _, _ => true


def findRowSymmetry : Grid Tile → Option Nat := fun grid =>
  let rec helper (acc : List (Array Tile)) : List (Array Tile) → Option Nat
    | [] => none
    | r1 :: r2 :: rws =>
      -- dbg_trace "{acc} {r1} {r2} {rws}"
      -- dbg_trace "{r1 == r2}"
      -- dbg_trace "{checkRowSymmetry acc rws}"
      if r1 == r2 && checkRowSymmetry acc rws then
        some (acc.length + 1)
      else
        helper (r1 :: acc) (r2 :: rws)
    | _ => none
  helper [] grid.toList


def findSymmetry : Grid Tile → Nat := fun grid =>
  match findRowSymmetry grid with
  | some row => row * 100
  | none => match findRowSymmetry grid.transpose with
    | some col => col
    | none => 0


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

def input1 := [
"#.##..##.",
"..#.##.#.",
"##......#",
"##......#",
"..#.##.#.",
"..##..##.",
"#.#.##.#.",
"",
"#...##..#",
"#....#..#",
"..##..###",
"#####.##.",
"#####.##.",
"..##..###",
"#....#..#",
]

def grids := parseLines input1
-- #eval Util.sum 0 (grids.map findSymmetry)

def runDay : List String → String := fun lns =>
  let grids := parseLines lns
  let sln1 := Util.sum 0 (grids.map findSymmetry)
  let sln2 := 0
  s!"Part 1: {sln1}\nPart 2: {sln2}"


end Day13
