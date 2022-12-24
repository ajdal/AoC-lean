import Util

namespace Day12

structure Hill : Type where
  heightMap : Grid Nat
  start : Nat × Nat
  goal : Nat × Nat

def heightMapToString : Grid Nat → String := fun g =>
  let lines := (g.map (fun row => row.map (fun n => Char.ofNat n))).map (fun l => String.mk (l.toList))
  lines.foldl (fun s line => s ++ "\n" ++ line) ""

def Hill.toString : Hill → String := fun h =>
  s!"\{{heightMapToString h.heightMap}\n  start: {h.start}\n  goal: {h.goal}\n}"

instance : ToString Hill where
  toString := Hill.toString

def fillHeights (heights : Grid Nat) (row : Nat) : List (Nat × Nat) → Grid Nat
  | [] => heights
  | (col, height) :: rest =>
    let newHeights := Grid.set heights row col height
    fillHeights newHeights row rest

def parseInput : List String → Option Hill := fun lines => do
  let n := lines.length
  let fst ← lines[0]?
  let m := fst.length
  let heights := Grid.make n m 0
  let rows := range 0 n
  let cols := range 0 m
  let inputs := List.zip rows lines
  let rec helper (heights : Grid Nat) (start : Nat × Nat) (goal : Nat × Nat) : List (Nat × String) → Hill
    | [] => {heightMap := heights, start := start, goal := goal}
    | (row, line) :: lines =>
      let withIndices := List.zip cols (line.data.map (fun c =>
        match c with
        | 'S' => 'a'.toNat
        | 'E' => 'z'.toNat
        | ch => ch.toNat
      ))
      let newHeights := fillHeights heights row withIndices
      let input := line.data.toArray
      let idxStart := input.findIdx? (fun c => c == 'S')
      let idxEnd := input.findIdx? (fun c => c == 'E')
      match idxStart, idxEnd with
      | none, none => helper newHeights start goal lines 
      | some idxStart, none => helper newHeights (row, idxStart) goal lines
      | none, some idxEnd => helper newHeights start (row, idxEnd) lines
      | some idxStart, some idxEnd => helper newHeights (row, idxStart) (row, idxEnd) lines

  helper heights (0,0) (0,0) inputs

def sampleInput := ["Sabqponm", "abcryxxl", "accszExk", "acctuvwj", "abdefghi"]
#eval parseInput sampleInput


def findShortestPath : Hill → List (Nat × Nat) := sorry

end Day12