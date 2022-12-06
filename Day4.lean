namespace Day4

structure Range where
  low : Nat
  high : Nat
deriving Repr

partial def readLines (stream : IO.FS.Stream) : IO (List String) := do 
  let line ← stream.getLine
  if line.length = 0 then
    return []
  else
    let rest ← readLines stream
    return [line.dropRightWhile Char.isWhitespace] ++ rest

def getRange (string : String) : Range :=
  let splits := string.splitOn "-"
  match splits with
  | a :: b :: [] => ⟨ a.toNat!, b.toNat! ⟩ 
  | _ => ⟨ 0, 0 ⟩ 

def contains : Range → Range → Bool
| r1, r2 => r1.low ≤ r2.low && r2.high ≤ r1.high

def getPair : String →  String × String :=
  fun string => 
  match string.splitOn "," with
  | s1 :: s2 :: [] => (s1, s2)
  | _ => ("", "")

def countContains (lines : List String) : Nat :=
  match lines with 
  | [] => 0
  | line :: rest => 
    let resRec := countContains rest
    let ⟨ s1, s2 ⟩ := getPair line
    let r1 := getRange s1
    let r2 := getRange s2
    if (contains r1 r2) || (contains r2 r1) then
      resRec + 1
    else
      resRec
    

#eval countContains ["2-6,3-5", "1-4,3-5", "4-100,3-1000"]

def overlap : Range → Range → Bool
| r1, r2 => (Nat.max r1.low r2.low) ≤ (Nat.min r1.high r2.high)

def countOverlap (lines : List String) : Nat :=
  match lines with 
  | [] => 0
  | line :: rest => 
    let resRec := countOverlap rest
    let ⟨ s1, s2 ⟩ := getPair line
    let r1 := getRange s1
    let r2 := getRange s2
    if overlap r1 r2 then
      resRec + 1
    else
      resRec

def solve : IO Unit := do 
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout

  let lines ← readLines stdin
  let cnt := countContains lines
  stdout.putStrLn s!"Number of contained intervals {cnt}"
  let cnt2 := countOverlap lines
  stdout.putStrLn s!"Number of overlapping intervals {cnt2}"


end Day4