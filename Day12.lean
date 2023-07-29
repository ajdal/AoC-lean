import Util
import Grid
import NatInf
import Stack

namespace Day12

open Grid NatInf Stack

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
  let heights := Grid.fill n m 0
  let rows := Util.range 0 n
  let cols := Util.range 0 m
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



def canMoveFrom (heights : Grid Nat) (dists : Grid NatInf) : Nat × Nat → Nat × Nat → Bool :=
  fun ⟨ x₁, y₁ ⟩ ⟨ x₂, y₂ ⟩ => 
    match (heights.get? x₁ y₁), (heights.get? x₂ y₂), (dists.get? x₁ y₁), (dists.get? x₂ y₂) with
    | some h₁, some h₂, some d₁, some d₂ => 
      -- d₁ + 1 < d₂ && h₂ + 1 ≥ h₁
      d₁ + 1 < d₂ && (h₂ ≥ h₁ || h₂ + 1 == h₁)
    | _, _, _, _ => false
  

def updateDists (dists : Grid NatInf) (dist : NatInf) : List (Nat × Nat) → Grid NatInf 
  | [] => dists
  | ⟨ x, y ⟩ :: rest =>
    let newDists := dists.set x y (dist + 1)
    updateDists newDists dist rest

partial def helper (hill : Hill) (dists : Grid NatInf) (stack : Stack (Nat × Nat)) : Grid NatInf :=
  let ⟨ heights, _, _ ⟩ := hill
  let ⟨ top, rest ⟩ := stack.pop
  match top with
  | none => dists
  | some ⟨ x, y ⟩ => 
    match (dists.get? x y) with
    | none => Grid.fill 1 1 0
    | some dCurr => 
      let neighbors := [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]
      let theChosenOnes := neighbors.foldl (
        fun lst el => if (canMoveFrom heights dists (x, y) el) then [el] ++ lst else lst
      ) []
      let newStack := rest.pushN theChosenOnes
      let newDists := updateDists dists dCurr theChosenOnes
      helper hill newDists newStack
  
def findShortestDist (m n : Nat) (hill : Hill) : NatInf := 
  let ⟨ _, ⟨ xₛ , yₛ ⟩, ⟨ xₑ, yₑ ⟩ ⟩ := hill
  let dists := Grid.fill m n (∞)
  let dists := dists.set xₑ yₑ 0
  let shortestDists := helper hill dists [(xₑ, yₑ)]
  match shortestDists.get? xₛ yₛ with
  | none => ∞
  | some dist => dist


def sampleInput := ["Sabqponm", "abcryxxl", "accszExk", "acctuvwj", "abdefghi"]
#eval parseInput sampleInput

def run : (Option Hill) → NatInf 
  | none => 0
  | some hill => findShortestDist 5 8 hill
    -- let ⟨ _, ⟨ xₛ , yₛ ⟩, ⟨ xₑ, yₑ ⟩ ⟩ := hill
    -- let dists := Grid.fill 5 8 (∞)
    -- let dists := dists.set xₑ yₑ 0
    -- helper hill dists [(xₑ, yₑ)]

#eval run (parseInput sampleInput)

def indToSub (m n i : Nat) : Nat × Nat := 
  (i / n, i % n)

def getHill : (Option Hill) → Hill
  | none => ⟨ (Grid.fill 2 2 0), (0, 0), (1, 1)⟩
  | some hill => hill 

def findShortestStart (m n : Nat) (heights : Grid Nat) (dists : Grid NatInf) : List Nat → NatInf
  | [] => ∞
  | idx :: idxs => 
    let ⟨ r, c ⟩ := indToSub m n idx 
    match (heights.get? r c), (dists.get? r c) with
    | some h, some d => 
      let recMin := findShortestStart m n heights dists idxs
      if h == 'a'.toNat && d < recMin then
        d
      else 
        recMin
    | _, _ => ∞

def getShortestDist (m n : Nat) (hill : Hill) : NatInf := 
  let ⟨ heights, ⟨ xₛ , yₛ ⟩, ⟨ xₑ, yₑ ⟩ ⟩ := hill
  let dists := Grid.fill m n (∞)
  let dists := dists.set xₑ yₑ 0
  let shortestDists := helper hill dists [(xₑ, yₑ)]
  findShortestStart m n heights shortestDists (Util.range 0 (m * n))

def runDay (input : List String) : String :=
  match parseInput input with
  | none => ""
  | some hill => 
    let m := input.length
    let n := input[0]!.length
    let res1 := findShortestDist m n hill
    let res2 := getShortestDist m n hill
    s!"Shortest path length: {res1}\nShortest path from 'a': {res2}"






































def hill := getHill (parseInput sampleInput)

def getHeights : Hill → Grid Nat 
  | ⟨ heights, ⟨ xₛ , yₛ ⟩, ⟨ xₑ, yₑ ⟩ ⟩ => heights

def getStart : Hill → Nat × Nat 
  | ⟨ heights, s, e ⟩ => s

def getEnd : Hill → Nat × Nat
  | ⟨ heights, s, e ⟩ => e

def start := getStart hill
def heights := getHeights hill
def goal := getEnd hill
#eval goal
def m := 5
def n := 8
def dists := Grid.fill m n (∞)
def x := 5
def y := 2
def neighbors := [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]
def theChosenOnes := neighbors.foldl (
  fun lst el => if (canMoveFrom heights dists (x, y) el) then [el] ++ lst else lst
) []
#eval List.map (fun pos => dists.get? pos.fst pos.snd) neighbors
-- def stack := []


end Day12