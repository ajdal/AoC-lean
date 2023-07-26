namespace State

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

end State