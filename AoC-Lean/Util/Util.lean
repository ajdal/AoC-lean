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


def sum : List Nat → Nat := fun l => l.foldl (fun s n => s + n) 0

end Util

def String.joinSep (sep : String) : List String → String
| [] => ""
| s :: [] => s
| s :: ss =>
  s ++ sep ++ joinSep sep ss
