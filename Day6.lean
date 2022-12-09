namespace Day6 

def findDistinct4 (ind : Nat) : List Char → Option Nat
| ch1::ch2::ch3::ch4::rest =>
  if ch1 != ch2 && ch1 != ch3 && ch1 != ch4 && ch2 != ch3 && ch2 != ch4 && ch3 != ch4 then
    some (ind + 4)
  else
    findDistinct4 (ind + 1) (ch2::ch3::ch4::rest)
| _ => none

def findDistinct14 (ind : Nat) (buff : Array Char) : List Char → Option Nat
| [] => none
| c::cs =>
  let sorted := Array.qsort buff (fun c1 c2 => c1.toNat < c2.toNat)
  if sorted.binSearchContains c (fun c1 c2 => c1.toNat < c2.toNat) then
    let mbI := buff.findIdx? (. = c)
    match mbI with
    | none => none
    | some i =>
      let rest := buff.toSubarray 0 i
      let foo := findDistinct14 (ind + 1) (#[c] ++ rest) cs
      foo
  else
    let newBuff := #[c] ++ buff
    if newBuff.size == 14 then
      some (ind + 1)
    else
      findDistinct14 (ind + 1) newBuff cs

def runDay : IO Unit := do 
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let line ← stdin.getLine
  let sol := findDistinct14 0 #[] line.data
  match sol with
  | none => return ()
  | some val => stdout.putStrLn s!"val: {val}"

end Day6