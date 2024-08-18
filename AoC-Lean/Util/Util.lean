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

def Array.mapi {α : Type u} {β : Type v} (f : Nat → α → β) (as : Array α) : Array β :=
  let asi := List.toArray (List.enum (Array.toList as))
  asi.map (fun (i, x) => f i x)

def String.joinSep (sep : String) : List String → String
| [] => ""
| s :: [] => s
| s :: ss =>
  s ++ sep ++ joinSep sep ss

def List.flatMap {α : Type u} {β : Type v} (f : α → List β) (as : List α) : List β :=
  List.foldl (fun acc a => acc ++ f a) [] as

/--
  Sum the value of a list, with initial value `initial`
-/
def List.sum {α : Type} [HAdd α α α] (initial : α) : List α → α :=
  fun l => l.foldl (fun s n => s + n) initial
