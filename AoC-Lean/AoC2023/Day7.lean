namespace Day7

def Card := Nat
deriving LT, DecidableEq, Inhabited, Repr

instance : OfNat Card n where
  ofNat := n

instance (c1 c2 : Card) : Decidable (c1 < c2) :=
  let n1 : Nat := c1
  let n2 : Nat := c2
  if h : (n1 < n2) then
    isTrue h
  else
    isFalse h

def Card.ofChar : Char → Card
  | '2' => 0
  | '3' => 1
  | '4' => 2
  | '5' => 3
  | '6' => 4
  | '7' => 5
  | '8' => 6
  | '9' => 7
  | 'T' => 8
  | 'J' => 9
  | 'Q' => 10
  | 'K' => 11
  | 'A' => 12
  | _ => panic! "Invalid card"

structure Hand where
  cards : List Card
deriving Repr

inductive HandType where
  | Five : HandType
  | Four : HandType
  | FullHouse : HandType
  | Three : HandType
  | TwoPair : HandType
  | OnePair : HandType
  | High : HandType
deriving DecidableEq, Repr

def HandType.toNat : HandType → Nat
  | Five => 6
  | Four => 5
  | FullHouse => 4
  | Three => 3
  | TwoPair => 2
  | OnePair => 1
  | High => 0

def HandType.lt : HandType → HandType → Bool := fun ht₁ ht₂ =>
  ht₁.toNat < ht₂.toNat

def Hand.getType : Hand → HandType := fun h =>
  let rec countCards (counts : Array Nat) : List Card → Array Nat
    | [] => counts
    | card :: cards =>
      countCards (counts.set! card ((counts.get! card) + 1)) cards
  let counts := countCards #[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] h.cards
  let counts := counts.insertionSort (· > ·)
  match counts[0]! with
  | 5 => HandType.Five
  | 4 => HandType.Four
  | 3 => match counts[1]! with
    | 2 => HandType.FullHouse
    | _ => HandType.Three
  | 2 => match counts[1]! with
    | 2 => HandType.TwoPair
    | _ => HandType.OnePair
  | _ => HandType.High

def Hand.removeJollys : Hand → Hand
  | {cards := cards} => {cards := cards.filter (fun x => x != Card.ofChar 'J')}

def Hand.countJollys : Hand → Nat := fun h =>
  h.cards.length - h.removeJollys.cards.length

open HandType in
def HandType.applyJollys : HandType → Nat → HandType
  | ht, 0 => ht
  | High, 1 => OnePair
  | OnePair, 1 => Three
  | TwoPair, 1 => FullHouse
  | Three, 1 => Four
  | High, 2 => Three
  | OnePair, 2 => Four
  | High, 3 => Four
  | _, _ => Five

def Hand.lt : Hand → Hand → Bool :=
  fun h₁ h₂ =>
    let rec lower : List (Card × Card) → Bool
      | [] => false
      | (c₁, c₂) :: cards => c₁ < c₂ || c₁ = c₂ && lower cards
    h₁.getType.lt h₂.getType ||
    h₁.getType = h₂.getType &&
    lower (List.zip h₁.cards h₂.cards)

def Hand.lt2 : Hand → Hand → Bool :=
  fun h₁ h₂ =>
    let rec lower : List (Card × Card) → Bool
      | [] => false
      | (9, 9) :: cards => lower cards
      | (_, 9) :: _ => false
      | (9, _) :: _ => true
      | (c₁, c₂) :: cards => c₁ < c₂ || c₁ = c₂ && lower cards
    let ht₁ := h₁.removeJollys.getType.applyJollys (h₁.countJollys)
    let ht₂ := h₂.removeJollys.getType.applyJollys (h₂.countJollys)
    ht₁.lt ht₂ || ht₁ = ht₂ && lower (List.zip h₁.cards h₂.cards)

def solve : List (Hand × Nat) → (Hand → Hand → Bool) →
  List (Nat × Hand × Nat) := fun hbs lt =>
  let sorted := (hbs.toArray.insertionSort (
    fun (h1, _) (h2, _) => lt h1 h2
  )).toList
  sorted.enum

def parseLines : List String → List (Hand × Nat)
  | [] => []
  | ln :: lns =>
    let play := ln.splitOn " "
    let bid := play.tail!.head!.toNat!
    let hand := {cards := play.head!.data.map Card.ofChar}
    (hand, bid) :: parseLines lns

def runDay : List String → String := fun lns =>
  let data := parseLines lns
  let sln1 := (solve data Hand.lt).foldl (
    fun acc (r, _, b) => acc + (r+1) * b) 0 -- 6440
  let sln2 := (solve data Hand.lt2).foldl (
    fun acc (r, _, b) => acc + (r+1) * b) 0 -- 5905
  s!"Part 1: {sln1}\nPart 2: {sln2}"

end Day7
