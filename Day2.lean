namespace Day2

inductive RPS where
  | rock      : RPS
  | paper     : RPS
  | scissors  : RPS
deriving Repr

inductive Outcome where
  | win  : Outcome
  | loss : Outcome
  | draw : Outcome
deriving Repr

open Outcome
open RPS

def beats (a b : RPS) : Outcome :=
  match a, b with
  | rock, paper => loss 
  | rock, rock => draw 
  | rock, scissors => win
  | paper, paper => draw
  | paper, rock => win
  | paper, scissors => loss
  | scissors, paper => win
  | scissors, rock => loss
  | scissors, scissors => draw

def val : RPS → Nat 
| rock     => 1
| paper    => 2
| scissors => 3

def points : Outcome → Nat
| win  => 6
| draw => 3
| loss => 0

infixl:50 ">" => beats

#eval paper > rock

open Sum 

def fromString : String → Option (Sum RPS Outcome) :=
  fun s => 
    match s with
    | "A" => some (inl rock)
    | "B" => some (inl paper)
    | "C" => some (inl scissors)
    | "X" => some (inr loss)
    | "Y" => some (inr draw)
    | "Z" => some (inr win)
    | _   => none

def toString : RPS → String :=
fun rps =>  
  match rps with
  | rock => "rock"
  | paper => "paper"
  | scissors => "scissors"

#check fromString "A"
#eval fromString "A"
#eval toString scissors

-- #eval beats (fromString "A") (fromString "X")

#eval String.split "A B" fun x : Char => x = ' '

def needs (a : RPS) (o : Outcome) : RPS :=
  match a, o with
  | rock, win       => paper
  | rock, loss      => scissors
  | paper, win      => scissors
  | paper, loss     => rock
  | scissors, win   => rock
  | scissors, loss  => paper
  | b, draw         => b


partial def readLines (stream : IO.FS.Stream) : IO (List String) := do 
  let line ← stream.getLine
  if line.length = 0 then
    return []
  else
    let rest ← readLines stream
    return [line] ++ rest
    

partial def readGames (stream : IO.FS.Stream) : IO (List (RPS × Outcome)) := do 
  let line ← stream.getLine
  if line.length = 0 then
    return []
  else
    let rest ← readGames stream
    let str := line.dropRightWhile Char.isWhitespace
    let game := str.split (fun x => x = ' ')
    match game with 
    | [] => return []
    | [a, b] => 
      let p1 := fromString a
      let p2 := fromString b
      match p1, p2 with
        | some (inl r1), some (inr r2) => 
          return [(r1, r2)] ++ rest
        | _, _ => return []
    | _ => return []

def printGames (list : List (RPS × RPS)) : IO Unit := do 
  let stdout ← IO.getStdout
  match list with
  | [] => return ()
  | (r1, r2)::els => 
    stdout.putStrLn s!"{toString r1} {toString r2}"
    printGames els
    return ()
  
#eval printGames [(RPS.rock, RPS.paper)]


def score : List (RPS × Outcome) → Nat
  | [] => 0
  | (r1, r2) :: els => val (needs r1 r2)  + points ((needs r1 r2) > r1) + score els

#eval score [(rock, draw), (paper, loss), (scissors, win)]

def rockPaperScissors : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout

  let games ← Day2.readGames stdin
  let score := Day2.score games
  stdout.putStrLn s!"Score: {score}"

end Day2