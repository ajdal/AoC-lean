import Grid

namespace Day10
open Grid

inductive Tile where
  | Ground
  | NS
  | EW
  | NE
  | NW
  | SW
  | SE
  | Start
  | None
  | Inner
deriving Repr, BEq, Inhabited

def Tile.toString : Tile → String
  | Ground => "."
  | NS => "|"
  | EW => "-"
  | NE => "L"
  | NW => "J"
  | SW => "7"
  | SE => "F"
  | Start => "S"
  | None => "o"
  | Inner => "☻"
instance : Repr Tile where
  reprPrec t _ := t.toString

instance : ToString Tile where
  toString t := t.toString


def Tile.ofChar : Char → Tile
  | '.' => Ground
  | '|' => NS
  | '-' => EW
  | 'L' => NE
  | 'J' => NW
  | '7' => SW
  | 'F' => SE
  | 'S' => Start
  | _ => Ground

inductive Direction where
  | N
  | S
  | E
  | W
deriving Repr, Inhabited

def Loc := Nat × Nat
deriving Repr, Inhabited, BEq, ToString

structure Map where
  tiles : Grid Tile
  start : Loc
deriving Repr


def findStart : Grid Tile → Loc :=
  fun grid =>
    let rec foldLines
    | [] => panic! "Start tile not found!"
    | (i, line) :: lines =>
      match line.findIdx? (. == Tile.Start) with
      | none => foldLines lines
      | some j => (i, j)
  foldLines grid.toList.enum

open Direction Tile in
def nextLoc (dir : Direction) (t : Tile) (curr : Loc) : Option Loc :=
  let (i, j) := curr
  match dir, t with
    | N, NS => some (i+1, j)
    | S, NS => some (i-1, j)
    | E, EW => some (i, j-1)
    | W, EW => some (i, j+1)
    | N, NE => some (i, j+1)
    | E, NE => some (i-1, j)
    | N, NW => some (i, j-1)
    | W, NW => some (i-1, j)
    | S, SW => some (i, j-1)
    | W, SW => some (i+1, j)
    | S, SE => some (i, j+1)
    | E, SE => some (i+1, j)
    | _, _ => none


open Direction in
def getDir : Loc → Loc → Direction := fun (i, j) (i', j')  =>
  let di : Int := i' - i
  let dj : Int := j' - j
  match di, dj with
    | 1, 0 => N
    | 0, -1 => E
    | -1, 0 => S
    | 0, 1 => W
    | _, _ => panic! "Invalid move!"

def getNeighbors (n m : Nat) : Loc → List Loc :=
  fun (i, j) =>
    let dxs : List ( Int × Int ) := [(-1, 0), (1, 0), (0, -1), (0, 1)]
    dxs.foldl (fun acc (di, dj) =>
      let newi := i + di
      let newj := j + dj
      if 0 <= newi && newi < n && 0 <= newj && newj < m then
        ((i + di).toNat, (j + dj).toNat) :: acc
      else
        acc
    ) []

def startNeighbors : Map → (Loc × Loc) := fun ⟨ tiles, start ⟩ =>
  let n := tiles.size
  let m := Array.size (Array.get! tiles 0)
  let neighbors := getNeighbors n m start
  let nbhTiles := neighbors.map (fun (i, j) => tiles.get! i j)
  let directions := List.map (fun loc => getDir start loc) neighbors
  let locsTilesDirs := List.zip neighbors (List.zip nbhTiles directions)
  let foo := locsTilesDirs.map (fun (loc, (tile, dir)) =>
    (loc, nextLoc dir tile loc)
  )
  let legit := foo.foldl (fun acc x =>
    match x with | (n, some _) => n :: acc | (_, none) => acc
  ) []
  (legit.get! 0, legit.get! 1)


partial def findLongestPath : Map → Nat :=
  fun map =>
    let rec search (tiles : Grid Tile) (start prev curr : Loc) : Nat :=
      if curr == start then
        1
      else
        let tile := tiles.get! curr.fst curr.snd
        let direction := getDir prev curr
        match nextLoc direction tile curr with
          | some new => 1 + (search tiles start curr new)
          | none => panic! "Invalid move!"
    let (n1, _) := startNeighbors map
    search map.tiles map.start map.start n1

partial def findCycle : Map → List Loc :=
  fun map =>
    let rec search (tiles : Grid Tile) (start prev curr : Loc) : List Loc :=
      if curr == start then
        []
      else
        let tile := tiles.get! curr.fst curr.snd
        let direction := getDir prev curr
        match nextLoc direction tile curr with
          | some new => curr :: (search tiles start curr new)
          | none => panic! "Invalid move!"
    let (n1, _) := startNeighbors map
    map.start :: (search map.tiles map.start map.start n1)

def Array.mapi {α : Type u} {β : Type v} (f : Nat → α → β) (as : Array α) : Array β :=
  let asi := List.toArray (List.enum (Array.toList as))
  asi.map (fun (i, x) => f i x)

def filterPipes (map : Map) (cycle : List Loc) : Map :=
  let tiles := Array.mapi (fun i row =>
    Array.mapi (fun j t =>
      let loc : Loc := (i,j)
      match cycle.find? (· == loc) with
        | none => .Ground
        | some _ => t
    ) row
  ) map.tiles
  {map with tiles}

def skipHorizontal (row : List (Nat × Tile)) : (Tile × List (Nat × Tile)) :=
  match row with
  | [] => (.None, [])
  | (_, .EW) :: tiles => skipHorizontal tiles
  | (_, t) :: tiles => (t, tiles)

open Tile in
partial def findInternal (map : Map) : List Loc :=
  let rec findInRow (cnt : Nat) : List (Nat × Tile) → List Nat
    | [] => []
    | (i, t) :: ts => match t with
      | .Ground => if cnt % 2 = 0 then
          findInRow cnt ts
        else
          i :: (findInRow cnt ts)
      | NS => findInRow (cnt + 1) ts
      | SE => match skipHorizontal ts with
        | (NW, ts') => findInRow (cnt + 1) ts'
        | (SW, ts') => findInRow cnt ts'
        | _ => panic! "Shouldn't happen"
      | NE => match skipHorizontal ts with
        | (NW, ts') => findInRow cnt ts'
        | (SW, ts') => findInRow (cnt + 1) ts'
        | _ => panic! "Shouldn't happen"
      | None => findInRow cnt ts
      | Start => panic! "Shouldn't have no start tile"
      | _ => findInRow cnt ts

  let arrayLocs := Array.mapi (
    fun i row =>
      let listRow := row.toList
      let enumRow : List (Nat × Tile) := List.enum listRow
      let rowLocs : List Nat := findInRow 0 enumRow
      let locs : List Loc := List.map (fun j => (i, j)) rowLocs
      locs
  ) map.tiles
  arrayLocs.foldl (fun acc x => acc ++ x) []

open Tile Direction in
def findMiddle : Direction → Direction → Tile
  | N, S => NS
  | N, E => NE
  | N, W => NW
  | W, E => EW
  | W, S => SW
  | W, N => NW
  | S, N => NS
  | S, E => SE
  | S, W => SW
  | E, N => NE
  | E, W => EW
  | E, S => SE
  | _, _ => panic! "Some more panic"

open Tile in
def Map.substituteStart : Map → List Loc → Map := fun ⟨ tiles, start ⟩ cycle  =>
  let ⟨ i, j ⟩ := start
  let fstNeighbor := cycle.get! 1
  let sndNeighbor := cycle.getLast!
  let tileType := (findMiddle (getDir fstNeighbor start) (getDir sndNeighbor start))
  let newTiles := Grid.set tiles i j tileType
  {tiles := newTiles, start := start}

def substitute (map : Map) (tileType : Tile) : List Loc → Map
  | [] => map
  | (i, j) :: locs =>
    let tiles := map.tiles.set i j tileType
    substitute {map with tiles} tileType locs

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
  let tiles := parseLines lns
  let start := findStart tiles
  let map : Map := {tiles := tiles, start := start}
  let sln1 := (findLongestPath map) / 2
  let cycle := findCycle map
  let map := map.substituteStart cycle
  let filtered := filterPipes map cycle
  let internal := findInternal filtered
  let sln2 := (internal).length
  s!"Part 1: {sln1}\nPart 2: {sln2}"

end Day10
