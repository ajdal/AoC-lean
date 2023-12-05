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

def runDay : List String → String := fun ls =>
  let sum := List.foldl (fun acc s =>
    let cards := parseCards s
    let numWinning := (intersection cards).length
    if numWinning > 0 then
      acc + 2 ^ (numWinning - 1)
    else
      acc
  ) 0 ls
  s!"Part 1: {sum}"

end Day4
