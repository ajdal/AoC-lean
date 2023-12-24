import Util

namespace Day9

def Sequence := List Int

def Sequence.derive : Sequence -> Sequence
  | [] => []
  | _ :: [] => []
  | k1 :: k2 :: ks =>
    (k2 - k1) :: Sequence.derive (k2 :: ks)

notation "dx" s => Sequence.derive s

theorem seq_len_less (seq : Sequence) : List.length (dx seq) < List.length seq := by
  sorry

def allZeros : Sequence -> Bool
  | [] => true
  | k :: ks => k = 0 && allZeros ks

partial def findNext : Sequence -> Int := fun seq =>
  let rec fold : Sequence -> Int := fun seq =>
    if allZeros seq then
      0
    else
      let seq' := dx seq
      let n := fold seq'
      seq.getLast! + n
  fold seq

def parseLines : List String -> List Sequence
  | [] => []
  | s :: ss => Util.parseInts s :: parseLines ss

def input : List Sequence :=
[
  [0, 3, 6, 9, 12, 15],
  [1, 3, 6, 10, 15, 21],
  [10, 13, 16, 21, 30, 45]
]

#eval List.map findNext input

def runDay : List String → String := fun lns =>
  let seqs := parseLines lns
  let sln1 := Util.sum 0 (List.map findNext seqs)
  let sln2 := 0
  s!"Part 1: {sln1}\nPart 2: {sln2}"

end Day9
