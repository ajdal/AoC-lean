namespace Day1

partial def readLines (stream : IO.FS.Stream) : IO (List (List Int)) := do
  let line ← stream.getLine
  if line.length == 0 then
    return []
  else
    let rest ← readLines stream
    let num := line.dropRightWhile Char.isWhitespace
    match String.toInt? num with
    | none => return [[]] ++ rest
    | some n =>
      match rest with
      | [] => return [[n]]
      | l :: ls => return [[n] ++ l] ++ ls


def printListHelper (stream : IO.FS.Stream) : List Int → IO Unit :=
  fun l => do
    match l with
    | [] => return ()
    | n::ns => do
      stream.putStr s!"{n} "
      printListHelper stream ns

def printList : List Int → IO Unit :=
  fun l => do
    let stdout ← IO.getStdout
    stdout.putStr "["
    printListHelper stdout l
    stdout.putStrLn "]"


def printListList : List (List Int) → IO Unit :=
  fun l =>
    match l with
    | [] => return ()
    | ns :: nns => do
      printList ns
      printListList nns
      return ()

def sum (list : List Int) : Int :=
  match list with
  | [] => 0
  | el :: els => el + sum els

def sumSublists : List (List Int) → List Int := 
  fun list => 
    match list with
    | [] => []
    | el :: els => [sum el] ++ sumSublists els

#eval sumSublists [[1, 2, 3], [4], [], [100, 24]]

-- def findMax (list : List (List Int)) : Int :=
def findMax : List Int → Int := 
  fun list => 
    match list with
    | [] => -1
    | el :: els => 
      let maxRec := findMax els
      if maxRec > el then
        maxRec
      else 
        el

#eval findMax (sumSublists [[1, 2, 3], [4], [], [100, 24]])

def removeEl (x : Int) : (List Int) → List Int :=
  fun list => 
    match list with
      | [] => []
      | el :: els =>
        if el = x then
          els
        else
          [el] ++ removeEl x els

#eval  removeEl 124 [6, 4, 0, 124]

def sumTopN (n : Nat) : List Int → Int := 
  fun list => 
    match n with 
      | 0 => 0
      | Nat.succ m => let max := findMax list
        let res := sumTopN m (removeEl max list)
        res + max

#eval sumTopN 4 [4, 2, 6, 1, 100]

def calories : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  
  let input ← Day1.readLines stdin
  let list := Day1.sumSublists input
  let res := Day1.findMax list
  stdout.putStrLn s!"Highest: {res}"
  let res2 := Day1.sumTopN 3 list
  stdout.putStrLn s!"Sum of top 3 {res2}"

end Day1