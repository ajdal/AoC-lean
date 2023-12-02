namespace Day1

def findFirst : List Char → Char
  | [] => panic! "First not found"
  | c :: cs => if c.isDigit then c else findFirst cs

def findLast := findFirst ∘ List.reverse

def parseLine : String → Int :=
  fun s => (String.mk [(findFirst s.data), (findLast s.data)]).toInt!

def numbers := [
  (1, "1"),
  (2, "2"),
  (3, "3"),
  (4, "4"),
  (5, "5"),
  (6, "6"),
  (7, "7"),
  (8, "8"),
  (9, "9"),
  (1, "one"),
  (2, "two"),
  (3, "three"),
  (4, "four"),
  (5, "five"),
  (6, "six"),
  (7, "seven"),
  (8, "eight"),
  (9, "nine")
]

def reverseNum := numbers.map (fun x => (x.fst, String.mk x.snd.data.reverse))

def findFirst2 (numbers : List (Nat × String)) : List Char → Int :=
  fun cs => match cs with
  | [] => panic! ""
  | (_ :: cs') =>
      let m := numbers.filter (fun x => x.snd.isPrefixOf (String.mk cs))
      match m with
      | [] => findFirst2 numbers cs'
      | m :: _ => m.fst

def parseLine2 : String → Int :=
  fun s => (findFirst2 numbers s.data) * 10 + (findFirst2 reverseNum s.data.reverse)

def runDay : List String → String :=
  fun ls =>
    let sum := ls.foldl (fun acc x => acc + (parseLine2 x)) 0
    s!"{sum}"

end Day1
