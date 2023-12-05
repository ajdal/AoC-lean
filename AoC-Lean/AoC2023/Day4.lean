import Util
import Set

namespace Day4

def Card := List Nat
deriving Repr, Nonempty

def whileDigit (digits : List Char) : List Char → List Char × List Char :=
  fun s => match s with
  | [] => (digits.reverse, [])
  | c :: cs =>
    if c.isDigit then
      whileDigit ([c] ++ digits) cs
    else
      (digits.reverse, s)

partial def parseCard : String → Card := fun s =>
  let cs := s.data
  let rec fold : List Char → List Nat
    | [] => []
    | c :: cs =>
      if Char.isDigit c then
        let (digits, cs) := whileDigit [c] cs
        let rest := fold cs
        match String.toNat? (String.mk digits) with
        | some n => [n] ++ rest
        | none => rest
      else
        fold cs
  fold cs

def parseCards : String → Card × Card := fun s =>
  let numbers := (s.data.dropWhile (fun c => c != ':')).drop 1
  let cards := (String.mk numbers).splitOn "|"
  let winning := parseCard cards[0]!
  let mine := parseCard cards[1]!
  (winning, mine)

open Util in
def intersection : Card × Card → List Nat := fun (winning, mine) =>
  let s1 : Set Nat := Set.mk winning
  let s2 : Set Nat := Set.mk mine
  (s1 ∩ s2).data

/-- Examples:
  [1,2,3,4,5] 2 3 => [3, 4, 5, 4, 5]
  [1,2,3,4,5] 3 7 => [4, 5, 6, 7, 8]
-/
def increaseNextK (n k : Nat) : List Nat → List Nat := fun nums => match k, nums with
  | 0, nums => nums
  | _, [] => []
  | k + 1, num::nums => [num + n] ++ increaseNextK n k nums

def totalCards (wins : List Nat) : List Nat → Nat := fun cards => match wins, cards with
  | [], _ => 0
  | _, [] => 0
  | w :: wins, c :: cards =>
    let next := increaseNextK c w cards
    c + totalCards wins next

def runDay : List String → String := fun ls =>
  let cards := ls.map parseCards
  let winning := cards.map (List.length ∘ intersection)
  let sum := winning.foldl (fun acc nw => if nw > 0 then acc + 2 ^ (nw - 1) else acc) 0
  let initial := winning.map (fun _ => 1)
  let total := totalCards winning initial
  s!"Part 1: {sum}¬Part 2: {total}"

end Day4
