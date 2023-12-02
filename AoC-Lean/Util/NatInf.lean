namespace NatInf

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


end NatInf