import Util
namespace Day13

def parseInput : List String →  List (String × String)
  | [] => []
  | p1 :: p2 :: rest => 
    match p1 with
    | "" => parseInput ([p2] ++ rest)
    | p1 => [(p1, p2)] ++ (parseInput rest)
  | _ => []

inductive Contents : Type where
  | num : Nat → Contents
  | contents : List Contents → Contents

instance : OfNat Contents n where
  ofNat := .num n

def contentsToString : List Contents → String
  | [] => ""
  | .num n :: [] => s!"{n}"
  | .num n :: cs => s!"{n}," ++ (contentsToString cs)
  | .contents cont :: [] => "[" ++ (contentsToString cont)  ++ "]"
  | .contents cont :: cs => "[" ++ (contentsToString cont)  ++ "]," ++ (contentsToString cs)

instance : ToString Contents where
  toString := fun c => match c with
    | .num n => s!"{n}"
    | .contents lst => contentsToString lst

def Packet : Type := List Contents

def splitIntoTokens : List Char → String → List String
  | ',' :: cs, "" => splitIntoTokens cs ""
  | ',' :: cs, buff => [buff] ++ splitIntoTokens cs ""
  | '[' :: cs, "" => ["["] ++ (splitIntoTokens cs "")
  | '[' :: cs, buff => [buff] ++ ["["] ++ (splitIntoTokens cs "")
  | ']' :: cs, "" => ["]"] ++ (splitIntoTokens cs "")
  | ']' :: cs, buff => [buff] ++ ["]"] ++ (splitIntoTokens cs "")
  | c :: cs, buff => splitIntoTokens cs (buff.push c)
  | _, _ => []

#eval Char.toString 'a'

partial def helper (contents : List Contents) : List String → (List Contents × List String)
    | "[" :: rest =>
      let ⟨ newEntry, remaining ⟩ := helper [] rest
      let ⟨ newContents, rest' ⟩ := (helper contents remaining)
      ([.contents newEntry] ++ newContents, rest')
    | "]" :: rest => (contents, rest)
    | n :: rest =>
      let ⟨ newContents, remaining ⟩ := helper contents rest
      ([.num n.toNat!] ++ newContents, remaining)
    | [] => (contents, [])

def parseContents : String → Contents := fun s =>
  let tokens := splitIntoTokens s.data ""
  let ⟨ parsed, _ ⟩ := helper [] tokens
  .contents parsed

partial def Contents.lt : List Contents → List Contents → Option Bool
  | c :: cs, c' :: cs' => match c, c' with
    | .num n, .num m => if n < m then true else if n > m then false else Contents.lt cs cs'
    | .contents cont, .num m =>
      match Contents.lt cont [.num m] with
      | some b => b
      | none => Contents.lt cs cs'
    | .num n, .contents cont =>
      match Contents.lt [.num n] cont with
      | some b => b
      | none => Contents.lt cs cs'
    | .contents cont, .contents cont' =>
      match Contents.lt cont cont' with
      | some b => b
      | none => Contents.lt cs cs'
  | _ :: _, [] => false
  | [], _ :: _ => true
  | [], [] => none

infixl:57 " < " => Contents.lt

def sampleInput1 := "[[2],0,0]"
def sampleInput2 := "[0,100]"

#eval parseContents sampleInput2

#eval ([parseContents sampleInput1]) < ([parseContents sampleInput2])

def prepareDataForPart2 : List (String × String) → List String := fun l => 
  let ⟨ l₁, l₂ ⟩ := l.unzip
  l₁ ++ l₂

def divider1 := parseContents "[[2]]"
def divider2 := parseContents "[[6]]"

def sortMyShit : List String → Array Contents := fun l =>
  let contents := l.map (fun s => parseContents s) ++ [divider1] ++ [divider2]
  let sorted := contents.toArray.insertionSort (fun c c' => ([c] < [c']) == true)
  sorted

def Contents.eq : Contents → Contents → Bool := fun c c' => s!"{c}" == s!"{c'}"

instance : BEq Contents where
  beq := fun c c' => c.eq c'

#eval divider1 == divider1

def runDay (input : List String) : String :=
  let inOrder := (((parseInput input).map (fun ⟨ fst, snd ⟩ => ([parseContents fst] < [parseContents snd] ) == some true)).enumFrom 1)
  let indices := (inOrder.filter (fun ⟨ _, b ⟩ => b == true)).map (fun ⟨ i, _ ⟩ => i)
  let total := sum indices
  let sortedContents := (sortMyShit (prepareDataForPart2 (parseInput input)))
  match (sortedContents.indexOf? divider1), (sortedContents.indexOf? divider2) with
  | some i₁, some i₂ =>
    s!"Part 1: {total}\nDecoder key: {(i₁.val + 1) * (i₂.val + 1)}"
  | _, _ => "Fail"

end Day13