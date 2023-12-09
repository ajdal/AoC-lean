import Util

namespace Day5


-- DestStart, SourceStart, RangeLen
def Rule := (Nat × Nat × Nat)
deriving Repr, ToString


def Map := List Rule
deriving Repr, ToString

def Map.sort : Map -> Map := fun m =>
  (Array.insertionSort (m.toArray) (fun r1 r2 =>
    r1.snd.fst < r2.snd.fst
  )).toList

structure Almanac where
  seeds : List Nat
  seedToSoil : Map
  soilToFert : Map
  fertToWater : Map
  waterToLight : Map
  lightToTemp : Map
  tempToHum : Map
  humToLocation : Map
deriving Repr

def isInRange : Nat → Rule → Bool :=
  fun n (_, s, l) =>
    s <= n && n < s + l

def Map.apply (m : Map) : Nat → Nat :=
  fun n =>
    match m.find? (isInRange n) with
      | none => n
      | some (d, s, _) => d + n - s

def Almanac.findLocation : Almanac → Nat → Nat :=
  fun a =>
    a.humToLocation.apply ∘
    a.tempToHum.apply ∘
    a.lightToTemp.apply ∘
    a.waterToLight.apply ∘
    a.fertToWater.apply ∘
    a.soilToFert.apply ∘
    a.seedToSoil.apply

def findLocations : Almanac → List Nat :=
  fun a => a.seeds.map a.findLocation


-- def foo  (r : Rule) (range: Nat × Nat) {cond : range.fst <= range.snd} :
def Rule.isContained (r : Rule) (range: Nat × Nat):
  Bool :=
    let (_, start, len) := r
    start <= range.fst && range.fst <= start + len ||
    start <= range.snd && range.snd <= start + len ||
    range.fst <= start && start + len <= range.snd


def Rule.minInRange (r : Rule) (range: Nat × Nat) : Option Nat :=
  let (dest, start, len) := r
  if range.fst <= start && range.snd >= start then
    dest
  else if start <= range.fst && start + len >= range.fst then
    dest + range.fst - start
  else
    none

def r1 : Rule := (52, 50, 48)
-- CASE 1: range (79, 93) | exp: 81
#eval r1.minInRange (79, 93)
-- CASE 2: range (48, 58) | exp:52
#eval r1.minInRange (48, 58)
-- CASE 3: range (60, 70) // range covered fully by rule | exp : 62
#eval r1.minInRange (60, 70)
-- CASE 4: range (48, 108) // range covers the whole rule | exp : 52
#eval r1.minInRange (48, 108)
-- CASE 6: range (48, 49) // range not overlapping rule | exp : none
#eval r1.minInRange (45, 49)
-- CASE 7: range (48, 50) | exp : 52
#eval r1.minInRange (45, 50)

  -- f : A -> B, r = [m .. n]
  -- min f x ; x ∈ r
  -- (10, 30, 3) -> [30, 33] -> [10, 13] ; min = 10
  -- (50, 25, 2) -> min = 50

def optMin : List (Option Nat) → Option Nat
  | [] => none
  | none :: ons => optMin ons
  | some n :: ons => match optMin ons with
    | none => n
    | some n' => some (min n n')

-- We assume that the Map `m` is sorted
def Map.min : Map → (Nat × Nat) → Option Nat :=
  fun m (a, b) =>
    let rec findMin (pt : Nat) : Map -> Option Nat
    | [] => none
    | (_, s, l) :: rules =>
      if s <= pt && pt <= s+l then
        if s+l+1 > b then
          none
        else
          findMin (s+l+1) rules
      else if pt > s + l then
        findMin pt rules
      else
        some pt
    let minIdOpt := findMin a m
    let mins := minIdOpt :: m.map (fun r => r.minInRange (a, b))
    optMin mins

def addIdentityMaps : Map → Map := fun m =>
  let rec doStuff (newRules : List Rule) : Map → List Rule
  | [] => newRules
  | _ :: [] => newRules
  | (_, s1, l1) :: (d2, s2, l2) :: rules =>
    let s := s1 + l1
    doStuff ((s, s, s2 - s) :: newRules) ((d2, s2, l2) :: rules)
  let m' : List Rule := m
  let newM : Map := doStuff [] m ++ m'
  let newM := match m.head? with
  | none => newM
  | some (_, s, _) => (0, 0, s - 1) :: newM
  newM.sort.filter (fun (_, _, l) => l != 0)

def m1 : Map := [(1, 9, 2), (3, 11, 3), (5, 16, 5)]
#eval addIdentityMaps m1

def Map.compose : Map → Map → Map := fun m₁ m₂ => sorry


-- #eval m1.min (10, 20)  -- exp: 2
-- #eval m1.min (12, 20)  -- exp: 4
-- #eval m1.min (8, 20)   -- exp: 1
-- #eval m1.min (16, 20)  -- exp: 5
-- Range (10, 20)
-- [(_, 9, 2), (_, 11, 3), (_, 16, 5)]


-- def minPerRange (a : Almanac) : List Nat → List Nat
--   | [] => []
--   | [n] => panic! "List length isn't even"
--   | s :: l :: rest =>
--     match ((Util.range s (s + l)).map a.findLocation).minimum? with
--     | none => minPerRange a rest
--     | some x => x :: minPerRange a rest

-- def findLocations2 : Almanac → List Nat :=
--   fun a => minPerRange a a.seeds


def parseSeeds : List String → List Nat × List String := fun ls =>
  let line := ls.head!.drop 7
  let nums := Util.parseNats line
  (nums, ls.tail!)

partial def parseMap : List String -> Map × List String := fun ls =>
  let rec fold (map : List Rule) (ls : List String) :=
    if ls.length = 0 || ls.head! = ""  then
      let tail := if ls.length = 0 then [] else ls.tail!
      (map, tail)
    else
      let ns := Util.parseNats ls.head!
      let rng : Rule := (ns[0]!, ns[1]!, ns[2]!)
      fold (rng :: map) ls.tail!
  fold [] ls.tail!

def parseLines : List String → Almanac := fun ls =>
  let (seeds, ls) := parseSeeds ls
  let ls := ls.tail!
  let (seedToSoil, ls) :=  parseMap ls
  let (soilToFert, ls) :=  parseMap ls
  let (fertToWater, ls) :=  parseMap ls
  let (waterToLight, ls) :=  parseMap ls
  let (lightToTemp, ls) :=  parseMap ls
  let (tempToHum, ls) :=  parseMap ls
  let (humToLocation, _) :=  parseMap ls

  {
    seeds := seeds,
    seedToSoil := seedToSoil,
    soilToFert := soilToFert,
    fertToWater := fertToWater,
    waterToLight := waterToLight,
    lightToTemp := lightToTemp,
    tempToHum := tempToHum,
    humToLocation := humToLocation,
  }

def runDay : List String → String := fun ls =>
  let almanac := parseLines ls
  let locs := findLocations almanac
  let sln1 := locs.minimum?

  -- let locs2 := findLocations2 almanac
  -- let sln2 := locs2.minimum?
  let sln2 : Option Nat := none
  s!"Part 1: {sln1}\nPart 2: {sln2}"

end Day5
