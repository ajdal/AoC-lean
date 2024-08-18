namespace Util

structure Range where
  low : Int
  high : Int
deriving Repr

instance : ToString Range where
  toString := fun r => s!"({r.low}, {r.high})"

instance : BEq Range where
  beq := fun r1 r2 => r1.low == r2.low && r1.high == r2.high

instance : LE Range where
  le := fun r1 r2 => r1.low ≤ r2.low && r2.high ≤ r1.high

instance : LT Range where
  lt := fun r1 r2 => r1.low < r2.low && r2.high < r1.high

def Int.max : Int → Int → Int := fun k l => if k < l then l else k
def Int.min : Int → Int → Int := fun k l => if k < l then k else l

def Range.intersection : Range → Range → Range := sorry

def Range.overlap : Range → Range → Bool
  | r1, r2 => (Int.max r1.low r2.low) ≤ (Int.min r1.high r2.high)

def Range.isEmpty : Range → Bool := fun r => r.high < r.low

def Range.blob : Range → Range → Range := fun r1 r2 => ⟨ Int.min r1.low r2.low, Int.max r1.high r2.high ⟩

def Range.contains : Range → Int → Bool := fun r k => r.low ≤ k && k ≤ r.high

def Range.size : Range → Nat := fun ⟨low, high⟩ => (high - low).natAbs + 1

def OpenSet : Type := List Range

-- instance : Coe (List Range) OpenSet where
--   coe := fun l => l

instance : HAppend OpenSet OpenSet OpenSet where
  hAppend := fun o1 o2 =>
    let l1 : List Range := o1
    let l2 : List Range := o2
    let l := l1 ++ l2
    let o : OpenSet := l
    o

instance : HAppend (List Range) OpenSet OpenSet where
  hAppend := fun l1 o2 =>
    let l2 : List Range := o2
    let l := l1 ++ l2
    let o : OpenSet := l
    o

instance : HAppend OpenSet (List Range) OpenSet where
  hAppend := fun o1 l2 =>
    let l1 : List Range := o1
    let l := l1 ++ l2
    let o : OpenSet := l
    o

def OpenSet.add : OpenSet → Range → OpenSet := fun o r =>
  match o with
  | [] => [r]
  | r' :: rs' =>
    if r.overlap r' then
      OpenSet.add rs' (Range.blob r r')
    else
      [r'] ++ (OpenSet.add rs' r)

def OpenSet.union : OpenSet → OpenSet → OpenSet := fun o1 o2 =>
  match o2 with
  | [] => o1
  | r :: rs => OpenSet.union (o1.add r) rs

notation:50 o1 " ∪ " o2 => OpenSet.union o1 o2

instance : SizeOf Range where
  sizeOf := fun r => (Int.max ((r.high - r.low) + 1) 0).natAbs

def OpenSet.size : OpenSet → Nat := fun o =>
  match o with
  | [] => 0
  | r :: rs => (sizeOf r) + OpenSet.size rs

instance : SizeOf OpenSet where
  sizeOf := fun o => o.size

def OpenSet.purge : OpenSet → OpenSet := fun o =>
  match o with
  | [] => []
  | r :: rs => if r.isEmpty then OpenSet.purge rs else [r] ++ (OpenSet.purge rs)

def OpenSet.removeElement : OpenSet → Int → OpenSet := fun o k =>
  match o with
  | [] => []
  | r :: rs =>
    if r.contains k then
      let r1 := ⟨ r.low, k - 1 ⟩
      let r2 := ⟨ k + 1, r.high ⟩
      let combined := [r1, r2] ++ rs
      OpenSet.purge combined
    else
      [r] ++ (OpenSet.removeElement rs k)

def r1 : Range := ⟨ -3, -2 ⟩
def r2 : Range := ⟨ -2, 0 ⟩
def r3 : Range := ⟨ 3, 4 ⟩
def o1 : OpenSet := [r1]

def r4 : Range := ⟨ -5, -1 ⟩
def r5 : Range := ⟨ 1, 2 ⟩
def o2 : OpenSet := [r4, r5]
def o3 : OpenSet := ((o1.add r3).add r2)

end Util
