import Util

namespace Day9

def Sequence := List Int
deriving ToString

def Sequence.derive : Sequence -> Sequence
  | [] => []
  | _ :: [] => []
  | k1 :: k2 :: ks =>
    (k2 - k1) :: Sequence.derive (k2 :: ks)

notation "dx" s => Sequence.derive s
theorem seq_len_less (seq : Sequence) :
  (List.length seq > 0) → List.length (dx seq) < List.length seq := by
  intro h
  induction seq with
  | nil => contradiction
  | cons k ks ih =>
    unfold Sequence.derive
    match ks with
    | [] =>
      simp
      exact Nat.zero_lt_succ 0
    | k' :: ks' => sorry

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

partial def findPrevious : Sequence → Int := fun seq =>
  let rec fold : Sequence → Int := fun seq =>
    if allZeros seq then
      0
    else
      let seq' := dx seq
      let n := fold seq'
      seq.head! - n
  fold seq

def parseLines : List String -> List Sequence
  | [] => []
  | s :: ss =>
    let nums := String.splitOn s " "
    let foo := List.foldl (fun acc s => match String.toInt? s with | none => acc | some k => acc ++ [k]) [] nums
    let rest := parseLines ss
    foo :: rest

def input : List Sequence :=
[
  [0, 3, 6, 9, 12, 15],
  [1, 3, 6, 10, 15, 21],
  [10, 13, 16, 21, 30, 45],
  [-1,-2,-3]
]

#eval List.map findPrevious input

def runDay : List String → String := fun lns =>
  let seqs := parseLines lns
  let sln1 := Util.sum (0 : Int) (List.map findNext seqs)
  let sln2 := Util.sum (0 : Int) (List.map findPrevious seqs)
  s!"Part 1: {sln1}\nPart 2: {sln2}"

end Day9
