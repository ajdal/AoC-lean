import Util
namespace Day14

def Line : Type := (Nat × Nat) × (Nat × Nat)

def Path : Type := List Line

def parseLines : List String → List Line
  | [] => []
  | p1 :: p2 :: rest =>
    let p1Split := p1.splitOn ","
    let p2Split := p2.splitOn ","
    let x1 := p1Split[0]?
    let y1 := p1Split[1]?
    let x2 := p2Split[0]?
    let y2 := p2Split[1]?
    match x1, y1, x2, y2 with
    | some x1, some y1, some x2, some y2 =>
      let l : Line := ((x1.toNat!, y1.toNat!), (x2.toNat!, y2.toNat!))
      match rest with
      | [] => [l]
      | h :: t => [l] ++ parseLines ([p2, h] ++ t)
    | _, _, _, _ => []
  | _ => []


def parseInput : List String → List Path
  | [] => []
  | path :: paths =>
    let lines := (path.splitOn "-> ").map (fun s => s.dropRightWhile Char.isWhitespace)
    let p : Path := parseLines lines
    [p] ++ parseInput paths

def sampleInput := ["498,4 -> 498,6 -> 496,6", "503,4 -> 502,4 -> 502,9 -> 494,9"]

#eval parseInput sampleInput

end Day14