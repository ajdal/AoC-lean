namespace Util

structure Set (α : Type) [BEq α] where
  data : List α
deriving Repr

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

def Set.filter [BEq α] : Set α → (α → Bool) → Set α := fun s p => ⟨ s.data.filter p ⟩

def Set.intersection [BEq α] : Set α → Set α → Set α := fun s1 s2 =>
   s1.filter s2.contains

notation s1 "∩" s2 => Set.intersection s1 s2

end Util
