namespace Day5

partial def readLines (stream : IO.FS.Stream) : IO (List String) := do 
  let line ← stream.getLine
  -- let stdout ← IO.getStdout
  if line.length = 0 then
    return []
  else
    let strippedLine := line.dropRightWhile (fun ch => ch == '\r' || ch == '\n')
    -- let strippedLine := line.dropRight 2
    let rest ← readLines stream
    -- stdout.putStrLn s!"{rest}"
    return [strippedLine] ++ rest

#eval "string\r\n".dropRight 1
#check Char.isWhitespace

structure Crate where 
  id : Char

def Stack : Type := List Crate

def Stack.push : Stack → Crate → Stack :=
  fun stack crate => 
    let old : List Crate := stack
    [crate] ++ old

def Stack.pop : Stack → Option Crate × Stack :=
  fun stack =>
    match stack with
    | [] => (none, [])
    | head::tail => (head, tail)

def Stack.top : Stack → Option Crate 
| [] => none
| head::_ => some head

def stack : Stack := [(⟨ 'X' ⟩ : Crate), (⟨ 'Y' ⟩ : Crate), (⟨ 'W' ⟩ : Crate), (⟨ 'Z' ⟩ : Crate)]
def newStack : Stack := Stack.push stack (⟨ 'Y' ⟩ : Crate)
def something := newStack.pop 

def Stack.popN : Stack → Nat → Option (List Crate) × Stack
| stack, 0 => (some [], stack)
| stack, n + 1 => 
  let ⟨ el, s ⟩ := stack.pop
  match el with
  | none => (none, s)
  | some c => 
    let ⟨ poped, s2 ⟩ := s.popN n
    match poped with
    | none => (none, s2)
    | some list => (some ([c] ++ list), s2)

def Stack.pushNreversed : Stack → List Crate → Stack 
| stack, [] => stack
| stack, c::cs => 
  let stack' := stack.pushNreversed cs
  stack'.push c


def Stack.pushN : Stack → List Crate → Stack 
| stack, [] => stack
| stack, c::cs => 
  let stack' := stack.push c
  stack'.pushN cs

instance : ToString Crate where
  toString := fun c => s!"[{c.id}]"

instance : ToString Stack where
  toString := fun s =>  (s.foldl (fun soFar  crate => soFar ++ s!"{crate} ") "") ++ "||"

def e : Stack := []

#eval toString newStack
#eval toString something 
#eval stack.popN 4
#eval e.pushN [(⟨ 'X' ⟩ : Crate), (⟨ 'Y' ⟩ : Crate), (⟨ 'W' ⟩ : Crate), (⟨ 'Z' ⟩ : Crate)]

def splitInput : List String → List String × List String
| [] => ([], [])
| line::lines =>
  if line == "" then
    ([], lines)
  else
    let ⟨ first, second ⟩  := splitInput lines
    ([line]++first, second)

def parseStack : List String → List (Crate × Nat) :=
  fun line =>
    let moo := line.dropWhile (fun c => c == " ")
    let pos := (line.length - moo.length) / 4
    match line with
    | [] => []
    | c::other =>
      let rest := parseStack other
      let cs := c.toList
      match cs[1]? with
      | none => rest
      | some idx =>
        let crate : Crate := ⟨ idx ⟩
        [(crate, pos)] ++ rest
    
def temp := ["[A]"]

#eval parseStack [" ", " ", " ", " ", "[A]"]

-- def parseStacks : List String → List Stack :=
--   fun l =>
--     let a := l.toArray
--     let nums := a.back
--     let rest := a.erase nums
--     let foo := rest.foldl (fun line )

def parseInstructions : List String → List (Nat × Nat × Nat)
| [] => []
| line::lines => 
  match line.splitOn " "  with
  | _::n1::_::n2::_::n3::[] => [(n1.toNat!, n2.toNat!, n3.toNat!)]  ++ parseInstructions lines
  | _ => []
  
def crateFromChars : Char × Char × Char → Option Crate
| ⟨ c1, c2, c3 ⟩ =>
  if c1 == '[' && c3 == ']' then
    some (⟨ c2 ⟩ : Crate)
  else
    none

def cratesFromString : List Char → List (Option Crate)
| [] => []
| c1::c2::c3::[] =>
  [crateFromChars (c1, c2, c3)]
| c1::c2::c3::_::rest => 
  [crateFromChars (c1, c2, c3)] ++ cratesFromString rest
| _ => []


def zippingFun : Stack →  Option Crate →  Stack :=
fun stack crate =>
  match stack, crate with
  | [], none => []
  | stack, none => stack
  | stack, some crate => stack.push crate

def parseStacks : List String → List Stack
| [] => []
| row::[] => (row.splitOn "   ").map (fun _ => [] : String → Stack) 
| row::rows => 
  let crates := cratesFromString row.data
  let stacks := parseStacks rows
  stacks.zipWith zippingFun crates

def getStack (stacks : Array Stack) (idx : Nat) : Stack :=
  match stacks[idx]? with
  | none => []
  | some stack => stack

def fromXtoY (stacks : Array Stack) (n fro to : Nat) (pushFun : Stack → List Crate → Stack) : Array Stack :=
  match stacks[fro]? with
  | none => stacks
  | some stack =>
    match stack.popN n with
    | ⟨ none, _ ⟩  => stacks
    | ⟨ some crates, newFroStack ⟩ =>
      match stacks[to]? with 
      | none => stacks
      | some toStack => 
        let newToStack := pushFun toStack crates
        (stacks.setD fro newFroStack).setD to newToStack

def rearrange : Array Stack → List (Nat × Nat × Nat) → (pushFun : Stack → List Crate → Stack) → Array Stack 
| stacks, [], _ => stacks
| stacks, ( n, fro, to )::rest, f  => 
  rearrange (fromXtoY stacks n (fro-1) (to-1) f) rest f

def concatTops : List Stack → String
| [] => ""
| stack :: stacks => 
  match stack.top with 
  | none => ""
  | some crate => s!"{crate.id}{concatTops stacks}"


def lines := [
  "    [D]    ", "[N] [C]    ",
  "[Z] [M] [P]", " 1   2   3 ", 
  "", 
  "move 1 from 2 to 1", 
  "move 3 from 1 to 3", 
  "move 2 from 2 to 1",
  "move 1 from 1 to 2"]
def splitIn := splitInput lines
def supply := parseStacks splitIn.fst
def instructions := parseInstructions splitIn.snd
#eval supply
#eval instructions
def rearranged := rearrange supply.toArray instructions Stack.pushN
#eval rearranged
#eval concatTops rearranged.data


def runDay : IO Unit := do 
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout

  let lines ← readLines stdin
  let splitIn := splitInput lines
  let supply := parseStacks splitIn.fst
  let instructions := parseInstructions splitIn.snd
  let part1 := concatTops (rearrange supply.toArray instructions Stack.pushN).data
  stdout.putStrLn s!"{part1}"
  let part2 := concatTops (rearrange supply.toArray instructions Stack.pushNreversed).data
  stdout.putStrLn s!"{part2}"

end Day5