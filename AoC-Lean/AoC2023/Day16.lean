import Grid

namespace Day16
open Grid

inductive TileType where
  | Empty
  | SplitV
  | SplitH
  | MirrorL
  | MirrorR
deriving BEq, Hashable, Inhabited

def TileType.toString : TileType → String
  | Empty => "."
  | SplitV => "|"
  | SplitH => "-"
  | MirrorL => "\\"
  | MirrorR => "/"

instance : Repr TileType where
  reprPrec t _ := t.toString

instance : ToString TileType where
  toString t := t.toString

def TileType.ofChar : Char → TileType
  | '.' => Empty
  | '|' => SplitV
  | '-' => SplitH
  | '\\' => MirrorL
  | '/' => MirrorR
  | _ => panic! "Invalid tile type"

inductive Direction where
  | R
  | L
  | U
  | D
deriving Repr, BEq

instance : ToString Direction where
  toString b := match b with
    | .R => "R"
    | .L => "L"
    | .U => "U"
    | .D => "D"

structure Beam where
  pos : (Nat × Nat)
  dir : Direction
deriving Repr

instance : ToString Beam where
  toString b := s!"{b.pos}, {b.dir}"

instance : Inhabited Beam where
  default := {pos := (0,0), dir := .R}

structure Tile where
  tile : TileType
  energizedR : Bool
  energizedL : Bool
  energizedU : Bool
  energizedD : Bool
deriving Inhabited

def Tile.isEnergized : Tile → Bool := fun ⟨_, r, l, u, d⟩ =>
  r || l || u || d

def Tile.toString : Tile → String := fun tile =>
  if isEnergized tile then
    "#"
  else
    TileType.toString tile.tile

instance : Repr Tile where
  reprPrec t _ := t.toString

instance : ToString Tile where
  toString t := t.toString

def newPos (m n : Nat) : Beam → Option (Nat × Nat) := fun beam =>
  let ⟨y, x⟩ := beam.pos
  match beam.dir with
    | .R => if x + 1 < m then some ⟨y, x+1⟩ else none
    | .L => if x > 0 then some ⟨y, x-1⟩ else none
    | .U => if y > 0 then some ⟨y-1, x⟩ else none
    | .D => if y + 1 < n then some ⟨y+1, x⟩ else none

def isCycle : Tile → Beam → Bool := fun tile beam =>
  -- dbg_trace "{tile}, {tile.energizedR}, {tile.energizedL}, {tile.energizedU}, {tile.energizedD}, {beam}"
  match beam.dir with
    | .R => tile.energizedR
    | .L => tile.energizedL
    | .U => tile.energizedU
    | .D => tile.energizedD

def step : Grid Tile → Beam → List Beam := fun grid beam =>
  let n := grid.size
  let m := (grid.getRowD 0).size
  match newPos m n beam with
    | none => []
    | some ⟨y, x⟩ =>
      let pos := ⟨ y, x ⟩
      let beam := {beam with pos}
      let tile := grid.get! y x
      match tile.tile with
        | .Empty => [beam]
        | .MirrorL => match beam.dir with
          | .R => [{beam with dir := .D}]
          | .L => [{beam with dir := .U}]
          | .U => [{beam with dir := .L}]
          | .D => [{beam with dir := .R}]
        | .MirrorR => match beam.dir with
          | .R => [{beam with dir := .U}]
          | .L => [{beam with dir := .D}]
          | .U => [{beam with dir := .R}]
          | .D => [{beam with dir := .L}]
        | .SplitV => match beam.dir with
          | .R | .L => [{beam with dir := .D}, {beam with dir := .U}]
          | .U | .D => [beam]
        | .SplitH => match beam.dir with
          | .R | .L => [beam]
          | .U | .D => [{beam with dir := .L}, {beam with dir := .R}]

def energize (grid : Grid Tile) : List Beam → Grid Tile
  | [] => grid
  | beam :: beams =>
    let ⟨y, x⟩ := beam.pos
    let tile := grid.get! y x
    let tile' := match beam.dir with
      | .R => {tile with energizedR := true}
      | .L => {tile with energizedL := true}
      | .U => {tile with energizedU := true}
      | .D => {tile with energizedD := true}
    energize (grid.set y x tile') beams

def simulateStep (grid : Grid Tile) (beams : List Beam) : Grid Tile × List Beam :=
  let beams := List.flatMap (step grid) beams
  let beams := List.filter (fun b =>
    let ⟨y, x⟩ := b.pos
    let tile := grid.get! y x
    !isCycle tile b
  ) beams
  let grid := energize grid beams
  (grid, beams)

instance : Inhabited (Grid Tile) where
  default := Grid.mk #[]

partial def activateBeam (grid : Grid Tile) (start : Beam) : Grid Tile :=
  let rec fold (maxSteps : Nat) (grid : Grid Tile) : List Beam → Grid Tile × List Beam
    | [] => (grid, [])
    | beams =>
      if maxSteps == 0 then
        dbg_trace "TIMEOUT"
        (grid, beams)
      else
        let ⟨grid', beams'⟩ := simulateStep grid beams
        fold (maxSteps - 1) grid' beams'
  let (grid, b) := match (grid.get! 0 0).tile with
    | .Empty => fold 1000000 grid [start]
    | .SplitV => fold 1000000 grid [{start with dir := .D}]
    | .SplitH => fold 1000000 grid [start]
    | .MirrorL => fold 1000000 grid [{start with dir := .D}]
    | .MirrorR => (grid, [])
  dbg_trace s!"{b}"
  grid

def countEnergized : Grid Tile → Nat := fun grid =>
  Array.foldl (fun acc row =>
    Array.foldl (fun acc t => if t.isEnergized then acc + 1 else acc) acc row
  ) 0 grid

def parseLines : List String → Grid Tile := fun lns =>
  let grid := (lns.map (fun ln =>
    (ln.data.map (fun tt =>
      {
        tile := TileType.ofChar tt,
        energizedR := false
        energizedL := false
        energizedU := false
        energizedD := false
      }
    )).toArray
  )).toArray
  Grid.mk grid

def input := [
  ".|...\\....",
  "|.-.\\.....",
  ".....|-...",
  "........|.",
  "..........",
  ".........\\",
  "..../.\\\\..",
  ".-.-/..|..",
  ".|....-|.\\",
  "..//.|....",
]

def input' := [
  "/.",
  "..",
]

def beam : Beam := {pos := (0, 0), dir := .R}
def grid := energize (parseLines input) [beam]
-- #eval countEnergized (activateBeam grid beam)

def runDay : List String → String := fun lns =>
  let data := parseLines lns
  let start : Beam := {pos := (0, 0), dir := .R}
  let grid := energize data [start]
  let sln1 := countEnergized (activateBeam grid start)

  let sln2 := 0
  s!"Part 1: {sln1}\nPart 2: {sln2}"

end Day16
