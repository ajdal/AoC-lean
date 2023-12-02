namespace Stack

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


end Stack