import Util

namespace Day3

def Pos := Nat × Nat
deriving Repr, ToString

structure Number where
  value : Nat
  location : Pos
deriving Repr

structure Sym where
  symbol : Char
  pos : Pos
deriving Repr

structure Schematic where
  numbers : List Number := []
  symbols : List Sym := []
deriving Repr

inductive NumOrSym where
  | Num : Nat × Nat → NumOrSym
  | Sym : Char × Nat → NumOrSym
deriving Repr

def whileDigit (digits : List Char) : List Char → List Char × List Char :=
  fun s => match s with
  | [] => (digits.reverse, [])
  | c :: cs =>
    if c.isDigit then
      whileDigit ([c] ++ digits) cs
    else
      (digits.reverse, s)

partial def parseLine : String → List NumOrSym := fun s =>
  let rec fold (i : Nat) (acc : List NumOrSym) : List Char → List NumOrSym
  | [] => acc
  | c :: cs =>
    if c.isDigit then
      let (d, cs) := whileDigit [] cs
      let num := String.mk ([c] ++ d)
      fold (i+(d.length)+1) ([.Num (String.toNat! num, i)] ++ acc) cs
    else if c != '.' then
      fold (i+1) ([.Sym (c,i)] ++ acc) cs
    else -- c == '.'
      fold (i+1) acc cs
  fold 0 [] s.data


def parseInput : List String → Schematic :=
  fun lns =>
    let rec parse (i : Nat) (s : Schematic) : List String → Schematic
    | [] => s
    | ln :: lns =>
      let parsed := parseLine ln
      let s := parsed.foldl (
        fun s p => match p with
        | .Num (n, col) =>
          { s with numbers := [⟨n, (i, col)⟩] ++ s.numbers }
        | .Sym (c, col) =>
          { s with symbols := [⟨c, (i, col)⟩] ++ s.symbols }
      ) s
      parse (i+1) s lns
    parse 0 {} lns


-- (i-1, j-1) --- (i-1, j+len+1)
-- (i+1, j-1) --- (i+1, j+len+1)

--           j
-- i-1  ....*........
--  i   .....345.....
-- i+1  ........%....

def isAdjacent (len : Nat) : (Nat × Nat) → (Nat × Nat) → Bool :=
  fun (i,j) (i', j') =>
    (i-1) ≤ i' && i' ≤ (i+1) &&
    (j-1) ≤ j' && j' ≤ (j+len)


def findParts : Schematic → List Number := fun s =>
  let parts := s.numbers.filter (
    fun n =>
      let i := n.location.fst
      let j := n.location.snd
      let len := (Nat.toDigits 10 n.value).length
      let sym := s.symbols.find? (fun sym =>
        let i' := sym.pos.fst
        let j' := sym.pos.snd
        isAdjacent len (i, j) (i', j')
      )
      match sym with
      | some _ => true
      | none => false
  )
  parts

def findAdjacentNumbers (s : Schematic) : Sym → List Number := fun sym =>
  s.numbers.filter (fun n =>
    let len := (Nat.toDigits 10 n.value).length
    isAdjacent len (n.location.fst, n.location.snd) (sym.pos.fst, sym.pos.snd)
  )

def findGears : Schematic → List Nat := fun s =>
  let gears := ((s.symbols.filter (fun sym => sym.symbol = '*')).map
    (fun sym => findAdjacentNumbers s sym)).filter (fun ls => ls.length = 2)
  gears.map (fun nums => nums.foldl (fun acc n => acc * n.value) 1)
  -- gears

def lines := [
  "467..114..",
  "...*......",
  "..35..633.",
  "......#...",
  "617*......",
  ".....+.58.",
  "..592.....",
  "......755.",
  "...$.*....",
  ".664.598.."
]

#eval findGears (parseInput lines)


def runDay : List String → String :=
  fun ls =>
    let s := parseInput ls
    -- Part 1
    let part1 := Util.sum 0 ((findParts s).map Number.value)
    let part2 := Util.sum 0 (findGears s)
    s!"Part 1:{part1}\nPart 2:{part2}"

end Day3
