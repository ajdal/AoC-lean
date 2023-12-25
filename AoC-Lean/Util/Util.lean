namespace Util

def range (start : Nat) (stop : Nat) : List Nat :=
  if start < stop then
    (List.range stop).drop (start)
  else
    ((List.range (start + 1)).drop (stop + 1)).reverse

partial def readLines (stream : IO.FS.Stream) : IO (List String) := do
  let line ← stream.getLine
  if line.length = 0 then
    return []
  else
    let rest ← readLines stream
    return [line.dropRightWhile Char.isWhitespace] ++ rest


def runDay (day : List String → String) : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let lines ← readLines stdin
  stdout.putStrLn (day lines)

/--
  Sum the value of a list, with initial value `initial`
-/
def sum {α : Type} [HAdd α α α] (initial : α) : List α → α :=
  fun l => l.foldl (fun s n => s + n) initial

def whileDigit (digits : List Char) : List Char → List Char × List Char :=
  fun s => match s with
  | [] => (digits.reverse, [])
  | c :: cs =>
    if c.isDigit then
      whileDigit ([c] ++ digits) cs
    else
      (digits.reverse, s)

partial def parseNats : String → List Nat := fun s =>
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

-- DOESN'T WORK!
-- partial def parseInts : String → List Int := fun s =>
--   let cs := s.data
--   let rec fold : List Char → List Int
--     | [] => []
--     | c :: cs =>
--       if Char.isDigit c then
--         let (digits, cs) := whileDigit [c] cs
--         let rest := fold cs
--         match String.toInt? (String.mk digits) with
--         | some n => [n] ++ rest
--         | none => rest
--       else
--         fold cs
--   fold cs

end Util

def String.joinSep (sep : String) : List String → String
| [] => ""
| s :: [] => s
| s :: ss =>
  s ++ sep ++ joinSep sep ss
