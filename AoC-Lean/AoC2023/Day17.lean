import Util
import Grid
import State
import Std.Data.HashMap

namespace Day17

open Grid State Std

def Pos := Nat × Nat
deriving Repr, Inhabited, BEq, Hashable, ToString

def inLine (p1 p2 p3 p4 : Pos) : Bool :=
  (p1.fst == p2.fst && p2.fst == p3.fst && p3.fst == p4.fst) ||
  (p1.snd == p2.snd && p2.snd == p3.snd && p3.snd == p4.snd)

#eval inLine (0, 3) (0, 2) (0, 1) (0, 0)

def nInLine : List Pos → Nat
  | [] => 0
  | (y, x) :: poss =>
    let rec count (x : Nat) : List Nat → Nat
      | [] => 0
      | x' :: xs => if x' == x then
          (count x xs) + 1
        else
          0
    let nRows := count y (poss.map Prod.fst)
    let nCols := count x (poss.map Prod.snd)
    (max nRows nCols) + 1

def findNext (m n : Nat) (curr : Pos) (path : List Pos): List Pos :=
  let (i,j) := curr
  let nexts := [(i+1,j), (i-1,j),(i,j+1),(i,j-1)]
  nexts.filter  (fun p =>
    p.1 < m && p.2 < n && p != curr && !path.contains p &&
    (if path.length < 2 then true else
      !inLine (path.get! 0) (path.get! 1) (path.get! 2) p
    )
  )

#eval findNext 2 4 (0,2) [(0, 1), (0, 0)]

def value (grid : Grid Nat) : List Pos → Nat
  | [] => 0
  | (i,j) :: ps => grid.get! i j + value grid ps

def findMin : List (List Pos × Nat) → List Pos × Nat
  | [] => ([], 999999999999)
  | (lps, val) :: rest =>
    let bestOfRest := findMin rest
    if bestOfRest.snd > val then
      (lps, val)
    else
      bestOfRest


def Cache := HashMap (Pos × Nat) (List Pos × Nat)
-- State (HashMap Record Nat) Nat
partial def findBestPath : Grid Nat → List Pos × Nat := fun grid =>
  let (m, n) := grid.dimensions
  let rec fold (path : List Pos) (grid : Grid Nat) (curr : Pos) : State Cache (List Pos × Nat) := do
    let currVal := grid.get! curr.fst curr.snd
    if curr == (m-1, n-1) then
      return (curr :: path, currVal)
    else
      let path := curr :: path
      let nexts := findNext m n curr path
      dbg_trace s!"{curr} {nexts}"
      let nextBest ← nexts.mapM (fun next => fold path grid next)
      let (lps, val) := findMin nextBest
      return (lps, val + currVal)

  let initialCache := HashMap.empty
  (fold [] grid (0,0) initialCache).snd

def parseLines : List String → Grid Nat := fun lns =>
  let rec fold : List String → List (Array Nat)
    | [] => []
    | ln :: lns =>
      (ln.data.map (fun c => String.toNat! c.toString)).toArray :: fold lns
  Grid.mk (fold lns).toArray

def input := [
  "2413432311323",
  "3215453535623",
  "3255245654254",
  "3446585845452",
  "4546657867536",
  "1438598798454",
  "4457876987766",
  "3637877979653",
  "4654967986887",
  "4564679986453",
  "1224686865563",
  "2546548887735",
  "4322674655533",
]

def input' := [
  "111111111111",
  "222222222222"
]

#eval findBestPath (parseLines input')

def runDay : List String → String := fun lns =>
  let u := parseLines lns
  let sln1 := 0
  let sln2 := 0
  s!"Part 1: {sln1}\nPart 2: {sln2}"

end Day17
