import Util

namespace Day15

/- Determine the ASCII code for the current character of the string.
   Increase the current value by the ASCII code you just determined.
   Set the current value to itself multiplied by 17.
   Set the current value to the remainder of dividing itself by 256.
-/
def hashSeq : String → Nat := fun s =>
  let rec helper (curr : Nat) : List Char → Nat
  | [] => curr
  | c :: cs =>
    let curr' := ((curr + c.toNat) * 17) % 256
    helper curr' cs
  helper 0 s.data

inductive Command where
  | Insert : String → Nat → Command
  | Remove : String → Command
deriving Repr

def parseCommand : String → Command := fun s =>
  if s.contains '=' then
    let foo := s.splitOn "="
    Command.Insert foo[0]! (String.toNat! foo[1]!)
  else
    let foo := s.splitOn "-"
    Command.Remove foo[0]!

def insertLens : List (String × Nat) → (String × Nat) → List (String × Nat) :=
  fun box ⟨ new_key, new_val ⟩ =>
    match box with
    | [] => [⟨ new_key, new_val ⟩]
    | ⟨ key, val ⟩ :: box =>
      if new_key = key then
        ⟨ new_key, new_val ⟩ :: box
      else
        ⟨ key, val ⟩ :: insertLens box ⟨new_key, new_val⟩

def removeLens : List (String × Nat) → String → List (String × Nat) :=
  fun box delKey =>
    match box with
    | [] => []
    | ⟨ key, val ⟩ :: box =>
      if delKey = key then
        box
      else
        ⟨ key, val ⟩ :: removeLens box delKey

open Command in
def executeCommand (hashmap : Array (List (String × Nat))) :
  Command → Array (List (String × Nat))
  | Insert key value =>
    let index := hashSeq key
    let box := hashmap.get! index
    let box := insertLens box ⟨ key, value ⟩
    hashmap.set! index box
  | Remove key =>
    let index := hashSeq key
    let box := hashmap.get! index
    let box := removeLens box key
    hashmap.set! index box

/- One plus the box number of the lens in question.
   The slot number of the lens within the box: 1 for the first lens, 2 for the second lens, and so on.
   The focal length of the lens.
-/
def getFocusPwr : Array (List (String × Nat)) → Nat := fun hashmap =>
  let rec helper (boxNo slotNo : Nat) : List (String × Nat) → Nat
    | [] => 0
    | ⟨_, v⟩ :: lenses =>
      let focusPwr := ((boxNo + 1) * (slotNo + 1) * v)
      focusPwr + (helper boxNo (slotNo + 1) lenses)
  let foo := Array.mapi (fun boxNo box => helper boxNo 0 box) hashmap
  Array.foldl (. + .) 0 foo

def parseLine : String → List String := fun s =>
  s.splitOn ","

def runDay : List String → String := fun lns =>
  let data := parseLine lns.head!
  let sln1 := Util.sum 0 (List.map hashSeq data)

  let hashmap : Array (List (String × Nat)) := Array.mkArray 256 []
  let hashmap' := List.foldl (fun acc x => executeCommand acc (parseCommand x)) hashmap data
  let sln2 := getFocusPwr (hashmap')
  s!"Part 1: {sln1}\nPart 2: {sln2}"

end Day15
