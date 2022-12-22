def range (start : Nat) (stop : Nat) : List Nat :=
  if start < stop then
    (List.range stop).drop (start)
  else
    ((List.range (start + 1)).drop (stop + 1)).reverse


def Grid (α : Type) : Type :=
  Array (Array α)

def Grid.get? : Grid α → Nat → Nat → Option α :=
  fun g i j => do
    let row ← Array.get? g i
    let el ← row[j]?
    el

def Grid.getRowD : Grid α → Nat → Array α :=
  fun g i => 
    match Array.get? g i with
    | none => #[]
    | some row => row

def Grid.set : Grid α → Nat → Nat → α → Grid α :=
  fun g i j x => 
    match Array.get? g i with
    | none => g
    | some row =>
      let newRow := Array.setD row j x
      Array.setD g i newRow

-- Creates Grid α of size m x n filled with default
def Grid.make (m n : Nat) (default : α) : Grid α :=
  mkArray m (mkArray n default)

def Grid.toString [ToString α] : Grid α → String := fun g =>
  g.foldl (fun s r => 
    s ++ (r.foldl (fun s' c => s' ++ s!"{c}") "") ++ "\n"
  ) ""

instance [ToString α] : ToString (Grid α) where
  toString := Grid.toString

def Grid.getCol : Grid α → Nat → Array α := fun g col =>
  let rec helper (g : Grid α) (col : Nat) : List Nat → List α
    | [] => []
    | i :: is =>
      let opt := g.get? i col
      match opt with
      | none => []
      | some x => [x] ++ (helper g col is)
  let row := range 0 (g.size)
  (helper g col row).toArray

def Grid.getRow : Grid α → Nat → Array α := fun g row =>
  let rec helper (g : Grid α) (row : Nat) : List Nat → List α
    | [] => []
    | j :: js =>
      let opt := g.get? row j
      match opt with
      | none => []
      | some x => [x] ++ (helper g row js)
  match Array.get? g 0 with
  | none => #[]
  | some r₀ =>
    let col := range 0 (r₀.size)
    (helper g row col).toArray


partial def readLines (stream : IO.FS.Stream) : IO (List String) := do 
  let line ← stream.getLine
  if line.length = 0 then
    return []
  else
    let rest ← readLines stream
    return [line.dropRightWhile Char.isWhitespace] ++ rest

def State (σ : Type) (α : Type) : Type :=
  σ → (σ × α)

def ok (x : α) : State σ α :=
  fun s => (s, x)

def getState : State σ σ :=
  fun s => (s, s)

def setState (s : σ) : State σ Unit :=
  fun _ => (s, ⟨⟩)

def andThen (first : State σ α) (next : α → State σ β) : State σ β :=
  fun s =>
    let (s', x) := first s
    next x s'

instance (σ : Type) : Monad (State σ) where
  pure := ok
  bind := andThen

structure Set (α : Type) [BEq α] where
  data : List α 
  
@[inline]
def Set.contains [BEq α] : α → Set α → Bool := fun x A => A.data.contains x

infix:40 " in " => Set.contains

def Set.insert [BEq α] : Set α → α → Set α := fun A x => 
  if x in A then
    A
  else
    {data := [x] ++ A.data}
    
def Set.toString [BEq α] [ToString α] : Set α → String := fun A =>
  let foo := A.data.toString
  let moo := foo.replace "[" "{"
  let boo := moo.replace "]" "}"
  boo
  
instance [BEq α] [ToString α] : ToString (Set α) where
  toString := Set.toString

def Set.size [BEq α] : Set α → Nat := fun A => A.data.length

#eval {data := [1,2] : Set Nat}