namespace Day3

partial def readLines (stream : IO.FS.Stream) : IO (List String) := do 
  let line ← stream.getLine
  if line.length = 0 then
    return []
  else
    let rest ← readLines stream
    return [line.dropRightWhile Char.isWhitespace] ++ rest

def splitAtN (chars : List Char) (n : Nat) : (List Char × List Char) :=
  match n with
  | 0 => ([], chars)
  | Nat.succ m => 
    match chars with
    | [] => ([], [])
    | chr :: chrs => 
      let (first, second) := splitAtN chrs m 
      ([chr]++first, second)

def getIndex (char : Option Char) : Nat :=
  match char with
  | none => 0
  | some c =>
    if c.isUpper then
      c.toNat - 38
    else
      c.toNat - 96

def fillArray (array : Array Bool) (chars : List Char) : Array Bool :=
  match chars with
  | [] => array
  | chr :: chrs => 
  let newArray := array.setD ((getIndex chr) - 1) true
  fillArray newArray chrs

def findExisting (array : Array Bool) (chars : List Char) : Option Char :=
  match chars with
  | [] => none
  | chr :: chrs => 
    match array[((getIndex chr) - 1)]? with
    | none => none
    | some bool => 
      if bool then
        chr
      else
        findExisting array chrs

def findCommon : (List Char × List Char) →  Option Char 
| ⟨ s1, s2 ⟩ =>
  let table := fillArray (mkArray 52 false) s1
  findExisting table s2

def score (strings : List String) : Nat :=
  strings.foldl (fun sum str => sum + getIndex (findCommon (splitAtN str.data (str.length/2))) ) 0

def splitBy3 (list : List String) : (List (String × String × String)) :=
  match list with
  | el1 :: el2 :: el3 :: rest => [(el1, el2, el3)] ++ splitBy3 rest
  | _ => []

def and : Bool → Bool → Bool := 
  fun x y => x && y

def groupScore : String × String × String →  Nat :=
  fun ⟨ g1, g2, g3 ⟩ =>
    let table1 := fillArray (mkArray 52 false) g1.data
    let table2 := fillArray (mkArray 52 false) g2.data
    let table3 := fillArray (mkArray 52 false) g3.data
    let bools := Array.zipWith (Array.zipWith table1 table2 and) table3 and
    match bools.findIdx? fun x => x = true with
    | none => 0
    | some x => x + 1

def assignGroupIds (groups : List (String × String × String)) : (List Nat) :=
  match groups with
  | [] => []
  | group :: rest => [groupScore group] ++ assignGroupIds rest

def scoreGroups : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout

  let rucksacks ← readLines stdin
  let groups := splitBy3 rucksacks
  let score := (assignGroupIds groups).foldl (fun sum el => sum + el) 0
  stdout.putStrLn s!"Sum of groups: {score}"

def rucksack : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let rucksacks ← readLines stdin
  let score := score rucksacks
  stdout.putStrLn s!"Sum of priorities: {score}"

end Day3