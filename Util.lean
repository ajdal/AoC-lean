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

def runDay (day : List String → String) : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let lines ← readLines stdin
  stdout.putStrLn (day lines)

#eval {data := [1,2] : Set Nat}


def Stack (α : Type) : Type := List α

def Stack.push : Stack α → α → Stack α :=
  fun stack crate => 
    let old : List α := stack
    [crate] ++ old

def Stack.pop : Stack α → Option α × Stack α :=
  fun stack =>
    match stack with
    | [] => (none, [])
    | head::tail => (head, tail)

def Stack.top : Stack α → Option α 
| [] => none
| head::_ => some head

def Stack.popN : Stack α → Nat → Option (List α) × Stack α
| stack, 0 => (some [], stack)
| stack, n + 1 => 
  let ⟨ el, s ⟩ := stack.pop
  match el with
  | none => (none, s)
  | some c => 
    let ⟨ poped, s2 ⟩ := s.popN n
    match poped with
    | none => (none, s2)
    | some list => (some ([c] ++ list), s2)

def Stack.pushNreversed : Stack α → List α → Stack α
| stack, [] => stack
| stack, c::cs => 
  let stack' := stack.pushNreversed cs
  stack'.push c

def Stack.pushN : Stack α → List α → Stack α 
| stack, [] => stack
| stack, c::cs => 
  let stack' := stack.push c
  stack'.pushN cs

instance [ToString α] : ToString (Stack α) where
  toString := fun s =>  (s.foldl (fun soFar el => soFar ++ s!"{el} ") "") ++ "||"


inductive NatInf : Type where
  | nat : Nat → NatInf
  | infinity : NatInf
deriving BEq

instance : ToString NatInf where
  toString := fun num => 
    match num with
    | .nat n => s!"{n}"
    | .infinity => "∞"

notation:200 "∞" => NatInf.infinity

def NatInf.plus : NatInf → NatInf → NatInf
  | .nat n₁, .nat n₂ => .nat (n₁ + n₂)
  | _, _ => ∞

def addNatNatInf : Nat → NatInf → NatInf
  | n₁, .nat n₂ => .nat (n₁ + n₂)
  | _, ∞ => ∞

def addNatInfNat : NatInf → Nat → NatInf 
  | .nat n₁, n₂ => .nat (n₁ + n₂)
  | ∞, _ => ∞

instance : HAdd Nat NatInf NatInf where
  hAdd := addNatNatInf

instance : HAdd NatInf Nat NatInf where
  hAdd := addNatInfNat

instance : OfNat NatInf n where
  ofNat := .nat n

#check LT.lt

def ltNatInf : NatInf → NatInf → Prop 
  | .nat n₁, .nat n₂ => n₁ < n₂
  | .nat _, ∞ => true
  | ∞, .nat _ => false
  | ∞, ∞ => false

instance : LT NatInf where
  lt := ltNatInf

instance (n1 n2 : NatInf) : Decidable (n1 < n2) :=
  match n1, n2 with
  | .nat n₁, .nat n₂ => dite (n₁ < n₂) (fun h => isTrue h) (fun h => isFalse h)
  | .nat _, ∞ => dite (true) (fun h => isTrue h) (fun h => isFalse h)
  | ∞, .nat _ => dite (false) (fun h => isTrue h) (fun h => isFalse h)
  | ∞, ∞ => dite (false) (fun h => isTrue h) (fun h => isFalse h)
