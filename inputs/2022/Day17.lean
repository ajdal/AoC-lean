import Util
import Grid

namespace Day17

open Grid

inductive Direction where
  | Left : Direction
  | Right : Direction
deriving Repr

instance : Inhabited Direction where
  default := Direction.Left

instance : ToString Direction where
  toString := fun d =>
    match d with
    | .Left => "Left"
    | .Right => "Right"

def Block : Type := List (Nat × Nat)

instance : ToString Block where
  toString := List.toString

def dash : Block := [(0, 0), (0, 1), (0, 2), (0, 3)]
def plus : Block := [(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)]
def ell : Block := [(0, 0), (0, 1), (0, 2), (1, 2), (2, 2)]
def bar : Block := [(0, 0), (1, 0), (2, 0), (3, 0)]
def box : Block := [(0, 0), (0, 1), (1, 0), (1, 1)]

def blocks := #[dash, plus, ell, bar, box]

instance : Inhabited Block where
  default := box

def World : Type := Grid Bool
def World.get! : World → Nat → Nat → Bool := Grid.get!

def isCrashed : World → Block → Bool  := fun world block =>
  List.any block (fun (y, x) => world.get! y x)

def Block.moveLeft : Block → World → Block := fun b w =>
  let hitWall := !List.all b (fun (_, x) => x > 0)
  let movedBlock := List.map (fun (y, x) => (y, x-1)) b
  if !hitWall && !(isCrashed w movedBlock) then
    movedBlock
  else
    b

def Block.moveRight : Block → World → Block := fun b w =>
  let hitWall := !List.all b (fun (_, x) => x < 6)

  let movedBlock := List.map (fun (y, x) => (y, x+1)) b
  if !hitWall && !(isCrashed w movedBlock) then
    movedBlock
  else
    b

def Block.moveRightN (n : Nat) : Block → Block := List.map (fun (y, x) => (y, x+n)) 

def Block.move : Block → World → Direction → Block := fun b w d =>
  match d with
  | .Left => b.moveLeft w
  | .Right => b.moveRight w


def Block.moveDown : Block → World → Option Block := fun b w =>
  let hitBottom := !List.all b (fun (y, _) => y > 0)
  let movedBlock := List.map (fun (y, x) => (y-1, x)) b
  if !hitBottom && !(isCrashed w movedBlock) then
    some movedBlock
  else
    none

def Block.moveUpN (n : Nat) : Block → Block := List.map (fun (y, x) => (y+n, x))

def world : World := Grid.fill 4 7 false

def World.addBlock : World → Block → World := fun w b =>
  b.foldl (fun w (y, x) => Grid.set w y x true) w

def bToC : Bool → Char
  | true => '#'
  | false => '.'

def World.toString : World → String := 
  fun w => 
    let w' := w.map (fun row => row.map bToC)
    Grid.toString (w'.reverse)

instance : ToString World where
  toString := World.toString

instance : Inhabited (World × Nat) where
  default := (world, 0)

partial def World.dropBlock (step : Nat) : World → Array Direction → Block → World × Nat :=
  fun w d b =>
    -- dbg_trace s!"dropping block {b}, starting step: {step}"
    let rec helper (step : Nat) : World → Array Direction → Block → World × Nat :=
      fun world directions block =>
        let direction := directions[step%directions.size]!
        -- dbg_trace s!"{world.addBlock block}direction: {direction}"
        let movedBlock := block.move world direction
        match movedBlock.moveDown world with
        | some block => helper (step+1) world directions block 
        | none => (world.addBlock movedBlock, step + 1)
    helper step w d b

def World.maxHeight : World → Nat := fun w =>
  let rec helper (h : Nat) : List (Array Bool) → Nat
    | [] => h
    | row :: rows =>
      if row.any id then helper (h+1) rows else h
  helper 0 w.toList

def Block.position : Block → World → Block := fun b w =>
  (b.moveUpN (w.maxHeight + 3)).moveRightN 2

instance : HAppend World World World where
  hAppend := Array.append

def World.extendN (n : Nat) : World → World := fun w =>
  let extenso : World := Grid.fill n 7 false
  w ++ extenso

partial def dropBlocks (maxSteps : Nat) : World → Array Direction → Array Block → World :=
  fun w ds bs =>
    let rec helper (dirStep blockStep : Nat) : Nat → World → Array Direction → Array Block → World :=
      fun maxSteps world directions blocks =>
        match maxSteps with
        | 0 => world
        | n+1 =>
          let block := (blocks[blockStep%blocks.size]!).position world
          let th := world.maxHeight
          let wh := world.size
          let world := world.extendN (7 - (wh - th))
          let (world, dirStep) := world.dropBlock dirStep directions block
          helper dirStep (blockStep + 1) n world directions blocks
    helper 0 0 maxSteps w ds bs


def parseInput : String → Array Direction :=
  fun s =>
    let rec helper : List Char → List Direction
      | [] => []
      | '<' :: cs => [.Left] ++ helper cs
      | '>' :: cs => [.Right] ++ helper cs
      | c :: _ => panic! s!"Unknown direction {c}"
    (helper s.data).toArray

def directions := parseInput ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

def runDay : List String → String
  | [] => ""
  | input :: _ =>
    let directions := parseInput input
    s!"{(dropBlocks 1000000 world directions blocks).maxHeight}"


end Day17