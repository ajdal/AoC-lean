namespace Day8

def Grid : Type :=
  Array (Array Nat)

def Grid.get? : Grid → Nat → Nat → Option Nat :=
  fun g i j => do
    let row ← Array.get? g i
    let el ← row[j]?
    el

def Grid.getRowD : Grid → Nat → Array Nat :=
  fun g i => 
    match Array.get? g i with
    | none => #[]
    | some row => row

def Grid.getColD (g : Grid) (c : Nat) : Array Nat :=
  let rec helper (g : Grid) (c : Nat) : Nat → Array Nat
  | 0 =>  match g.get? 0 c with
    | none => #[]
    | some x => #[x]
  | r + 1 => match g.get? (r+1) c with
    | none => #[]
    | some x => (helper g c (r)) ++ #[x]
  helper g c ((Array.size g) - 1)

def Grid.set : Grid → Nat → Nat → Nat → Grid :=
  fun g i j x => 
    match Array.get? g i with
    | none => g
    | some row =>
      let newRow := Array.setD row j x
      Array.setD g i newRow

-- Creates Grid of size m x n filled with default
def Grid.make (m n : Nat) (default := 0) : Grid :=
  mkArray m (mkArray n default)

def Grid.toString : Grid → String := fun g =>
  g.foldl (fun s r => 
    s ++ (r.foldl (fun s' c => s' ++ s!"{c}") "") ++ "\r\n"
  ) ""

instance : ToString Grid where
  toString := Grid.toString

partial def readLines (stream : IO.FS.Stream) : IO (List String) := do 
  let line ← stream.getLine
  if line.length = 0 then
    return []
  else
    let rest ← readLines stream
    return [line.dropRightWhile Char.isWhitespace] ++ rest

def parseRow (g : Grid) (i : Nat) (j : Nat) : List Char → Grid
  | [] => g
  | c :: cs => parseRow (g.set i j (c.toNat - 48)) i (j + 1) cs

def parseRows (g : Grid) (i : Nat) : List String → Grid
  | [] => g
  | l :: ls => parseRows (parseRow g i 0 l.data) (i + 1) ls

def parseInput (lines : List String) : (Grid × Nat × Nat) :=
  let m := lines.length
  match lines[0]? with
  | none => (#[], m, 0)
  | some row0 =>
    let n := row0.length
    let g := Grid.make m n 0
    (parseRows g 0 lines, m, n)

def indToSub (m n i : Nat) : Nat × Nat := 
  (i / n, i % n)

def maxEl : Array Nat → Nat
  | #[] => 0
  | a => a.foldl (fun max x => if x > max then x else max) 0

def checkVisible (g : Grid) (m n i: Nat) : Bool :=
  let ⟨ r, c ⟩ := indToSub m n i
  if r <= 0 || r >= (m - 1) || c <= 0 || c >= (n - 1) then
    true
  else
    let el := match g.get? r c with
      | none => 0
      | some x => x
    let row := g.getRowD r
    let col := g.getColD c
    let visL := (maxEl (row[:c])) < el
    let visR := (maxEl (row[c+1:])) < el
    let visT := (maxEl (col[:r])) < el
    let visB := (maxEl (col[r+1:])) < el
    visL || visR || visT || visB

def countVisible (g : Grid) (m n : Nat) : Nat → Nat
  | 0 => if checkVisible g m n 0 then 1 else 0
  | i + 1 => 
    let recVis := countVisible g m n i
    if checkVisible g m n (i+1) then 1 + recVis else recVis

def countSeen : List Nat → Nat → Nat
  | [], _ => 0
  | t :: ts, h => 
    if t >= h then 1 else 1 + countSeen ts h

def scenicScore (g : Grid) (m n i: Nat) : Nat :=
  let ⟨ r, c ⟩ := indToSub m n i
  if r <= 0 || r >= (m - 1) || c <= 0 || c >= (n - 1) then
    0
  else
    let el := match g.get? r c with
      | none => 0
      | some x => x
    let row := g.getRowD r
    let col := g.getColD c
    let visL := (countSeen (Array.ofSubarray row[:c]).reverse.data el)
    let visR := (countSeen (Array.ofSubarray row[c+1:]).data el)
    let visT := (countSeen (Array.ofSubarray col[:r]).reverse.data el)
    let visB := (countSeen (Array.ofSubarray col[r+1:]).data el)
    visL * visR * visT * visB

def bestScenic (g : Grid) (m n : Nat) : Nat → Nat
  | 0 => scenicScore g m n 0
  | i + 1 => 
    let recBest := bestScenic g m n i
    max (scenicScore g m n (i+1)) recBest

def runDay : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let lines ← readLines stdin

  let ⟨ grid, m, n ⟩ := parseInput lines
  let numEl := (m * n)

  -- Part 1
  let nVis := countVisible grid m n (numEl - 1)
  stdout.putStrLn s!"Number of visible trees {nVis}"

  -- Part 2
  let bestScore := bestScenic grid m n (numEl - 1)
  stdout.putStrLn s!"Highest scenic score {bestScore}"

end Day8