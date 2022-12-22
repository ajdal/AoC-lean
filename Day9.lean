import Util
namespace Day9

inductive Direction : Type where
  | left : Direction
  | right : Direction
  | up : Direction
  | down : Direction
deriving Repr

def parseInput : List String → List (Direction × Int)
  | [] => []
  | input :: inputs =>
    let temp := input.splitOn " "  
    match temp[1]? with
    | none => []
    | some num =>
      match num.toInt? with
      | none => []
      | some n =>
        match temp[0]? with 
        | none => []
        | some "L" => [(.left, n)] ++ parseInput inputs
        | some "R" => [(.right, n)] ++ parseInput inputs
        | some "U" => [(.up, n)] ++ parseInput inputs
        | some "D" => [(.down, n)] ++ parseInput inputs
        | _ => []


def moveTail (head : Int × Int) (tail : Int × Int) : Int × Int :=
  let ⟨ hx, hy ⟩ := head
  let ⟨ tx, ty ⟩ := tail
  if hy == ty then
    if tx + 1 < hx then 
      (tx+1, ty)
    else if hx + 1 < tx   then
      (tx-1, ty)
    else
      (tx, ty)
  else if hx == tx then
    if ty + 1 < hy then 
      (tx, ty+1)
    else if hy + 1 < ty  then
      (tx, ty-1)
    else
      (tx, ty)
  else
    if tx < hx then
      if ty + 1 < hy || (tx + 1 < hx && ty < hy) then
        (tx+1, ty+1)
      else if hy + 1 < ty || (tx + 1 < hx && hy < ty) then
        (tx+1, ty-1)
      else
        (tx, ty)
    else -- tx > hx
      if ty + 1 < hy || (hx + 1 < tx && ty < hy) then
        (tx-1, ty+1)
      else if hy + 1 < ty || (hx + 1 < tx && hy < ty) then
        (tx-1, ty-1)
      else
        (tx,ty)

#eval moveTail (4,1) (3,3)
    
def moveHead : Int × Int → Direction → Int × Int := fun (x,y) d =>
  match d with
  | .left => (x-1, y) 
  | .right => (x+1, y)
  | .up => (x, y+1)
  | .down => (x, y-1)

partial def computeTailPositions (head tail : Int × Int) (positions : Set (Int × Int)) : List (Direction × Int) → Set (Int × Int)
  | [] => positions
  | instruction :: instructions =>
    let ⟨ d, n ⟩ := instruction
    if n == 0 then
      computeTailPositions head tail positions instructions
    else
      let newHead := moveHead head d
      let newTail := moveTail newHead tail
      computeTailPositions newHead newTail (positions.insert newTail) ([(d, n-1)] ++ instructions)

def computePathLength (instructions: List (Direction × Int)) : Int := 
  let pos := computeTailPositions (0,0) (0,0) {data := []} instructions
  pos.size
  
def runDay : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let lines ← readLines stdin
  
  stdout.putStrLn s!"{computePathLength (parseInput lines)}"

def sample := parseInput ["R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2"] 

#eval computePathLength sample

end Day9