import Util

namespace Day6

structure Races where
  times : List Nat
  distances : List Nat
deriving Repr

structure Race where
  time : Nat
  distance : Nat
deriving Repr

/--
  (M - k) * k == M * k - k^2
  d < M*k - k^2
  0 < -k^2 + M * k - d
  0 > k^2 - M * k + d

  0 = k^2 - M * k + d

            - b +- D
   x₁,₂ =   ---------
                2a

  D = √ b² - 4ac = √ M² - 4d
-/
def dist (totalMiliseconds : Nat) (hold : Nat) : Nat :=
  (totalMiliseconds - hold) * hold

def solution1 : Races -> List Nat := fun rs =>
  let rec beatsRecord : List (Nat × Nat) -> List Nat
  | [] => []
  | (maxT, d) :: ts_ds =>
    let allTimes := Util.range 0 maxT
    let beats := (allTimes.map (dist maxT)).filter (d < ·)
    beats.length :: beatsRecord ts_ds
  beatsRecord (List.zip rs.times rs.distances)

def solution2 : Nat → Nat → Float := fun time distance =>
  let M := Float.ofNat time
  let d := Float.ofNat distance
  let D := Float.sqrt (M * M - 4 * d)
  let x₁ := (M - D) / 2
  let x₂ := (M + D) / 2
  (Float.floor x₂) - (Float.ceil x₁) + 1

def parseLines1 : List String -> Races := fun lns =>
  let times := Util.parseNats (lns[0]!.drop 5)
  let distances := Util.parseNats (lns[1]!.drop 9)
  {
    times := times
    distances := distances
  }

def parseLines2 : List String -> Race := fun lns =>
  let time := String.toNat! (((Util.parseNats (lns[0]!.drop 5)).map (fun n => s!"{n}")).foldl (.++.) "")
  let distance := String.toNat! (((Util.parseNats (lns[1]!.drop 9)).map (fun n => s!"{n}")).foldl (.++.) "")
  {
    time := time
    distance := distance
  }

def runDay : List String → String := fun lns =>
  let races := parseLines1 lns
  let sln1 := List.foldl (·*·) 1 (solution1 races)
  let race := parseLines2 lns
  let sln2 := solution2 race.time race.distance
  s!"Part 1: {sln1}\nPart 2: {sln2}"

end Day6
