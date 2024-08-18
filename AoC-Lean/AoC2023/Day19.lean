import Std.Data.HashMap
import Range
import Util

namespace Day19

open Std

inductive Part where
  | x
  | m
  | a
  | s
deriving Repr

inductive Compare where
  | lt
  | gt
deriving Repr

structure Condition where
  part : Part
  cmp : Compare
  value : Nat
  next : String
deriving Repr

inductive Rule where
  | Condition : Condition → Rule
  | Workflow : String → Rule
deriving Repr

structure Workflow where
  name : String
  rules : List Rule
deriving Repr, Inhabited

structure Item where
  x : Nat
  m : Nat
  a : Nat
  s : Nat
deriving Repr, Inhabited

structure Chunk where
  x : Util.Range
  m : Util.Range
  a : Util.Range
  s : Util.Range
deriving Repr

def initialRange : Util.Range := ⟨1, 4000⟩
def initialChunk : Chunk := ⟨initialRange, initialRange, initialRange, initialRange⟩

instance : Inhabited Chunk where
  default := initialChunk

def Chunk.size : Chunk → Nat := fun ⟨x, m, a, s⟩ =>
  x.size * m.size * a.size * s.size

def stringToRule : String → Rule
  | x =>
    let part := match x.data[0]! with
      | 'x' => Part.x
      | 'm' => Part.m
      | 'a' => Part.a
      | _ => Part.s
    let cmp := match x.data[1]! with
      | '>' => Compare.gt
      | _ => Compare.lt
    let x := x.drop 2
    let y := x.splitOn ":"
    let value := y[0]!.toNat!
    let next := y[1]!
    Rule.Condition ⟨part, cmp, value, next⟩

def getRules : List String → List Rule
  | [] => []
  | [x] => [Rule.Workflow x]
  | x :: xs => stringToRule x :: (getRules xs)

def lineToWorkflow : String → Workflow := fun line =>
  let x := line.splitOn "{"
  let name := x[0]!
  let something := x[1]!.dropRight 1
  let conditions := something.splitOn ","
  let rules := getRules conditions
  ⟨name, rules⟩

def getWorkflows : List String → HashMap String Workflow × List String
  | [] => ⟨HashMap.empty, []⟩
  | ln :: lns =>
    if (ln.dropRightWhile Char.isWhitespace).isEmpty then
      ⟨ HashMap.empty, lns ⟩
    else
      let ⟨ ws, rest ⟩ := getWorkflows lns
      let wfl := lineToWorkflow ln
      let ws := ws.insert wfl.name wfl
      (ws, rest)

def lineToItem : String → Item := fun s =>
  let s := (s.drop 1).dropRight 1
  let ratings := s.splitOn ","
  let x := (ratings[0]!.drop 2).toNat!
  let m := (ratings[1]!.drop 2).toNat!
  let a := (ratings[2]!.drop 2).toNat!
  let s := (ratings[3]!.drop 2).toNat!
  ⟨x, m, a, s⟩

def parseLines : List String → HashMap String Workflow × List Item := fun lines =>
  let ⟨workflows, rest⟩ := getWorkflows lines
  let items := rest.map lineToItem
  ⟨workflows, items⟩

def getNextWorkflow (item : Item) : List Rule → String
  | [] => panic! "Empty rule"
  | [.Condition _] => panic! "Last rule is Condition."
  | [.Workflow name] => name
  | .Workflow _ :: _ => panic! "Workflow name before last element."
  | .Condition rule :: rules =>
    let rating := match rule.part with
      | .x => item.x
      | .m => item.m
      | .a => item.a
      | .s => item.s
    match rule.cmp with
      | .lt =>
        if rating < rule.value then
          rule.next
        else
          getNextWorkflow item rules
      | .gt =>
        if rating > rule.value then
          rule.next
        else
          getNextWorkflow item rules

partial def work : Item → HashMap String Workflow → String × List String := fun item wfls =>
  let rec helper : String → String ×  List String := fun name =>
    let wf := wfls.find! name
    match getNextWorkflow item wf.rules with
    | "A" => ("A", ["A"])
    | "R" => ("R", ["R"])
    | name =>
      let ⟨final, path⟩ := helper name
      (final, name :: path)
  helper "in"

def sumRating : Item → Nat := fun item =>
  item.x + item.m + item.a + item.s

def part1 (wfls : HashMap String Workflow) : List Item → Nat := fun items =>
  let accepted := items.filter (fun item => (work item wfls).1 == "A")
  let values := accepted.map sumRating
  values.foldl (· + ·) 0

-- Split range into the new range and remaining range, given compare function
def splitRange : Util.Range → Compare → Nat → Util.Range × Util.Range :=
  fun range cmp newVal =>
    match cmp with
      | Compare.lt => ({range with high := newVal-1}, {range with low := newVal})
      | Compare.gt => ({range with low := newVal+1}, {range with high := newVal})

def getRuleChunks (chunk : Chunk) : List Rule → List (String × Chunk)
  | [] => panic! "Empty rule"
  | [.Condition _] => panic! "Last rule is Condition."
  | [.Workflow name] => [(name, chunk)]
  | .Workflow _ :: _ => panic! "Workflow name before last element."
  | .Condition rule :: rules =>
    match rule.part with
      | Part.x =>
        let ⟨newRange, restRange⟩ := splitRange chunk.x rule.cmp rule.value
        (rule.next, {chunk with x := newRange}) :: getRuleChunks {chunk with x := restRange} rules
      | Part.m =>
        let ⟨newRange, restRange⟩ := splitRange chunk.m rule.cmp rule.value
        (rule.next, {chunk with m := newRange}) :: getRuleChunks {chunk with m := restRange} rules
      | Part.a =>
        let ⟨newRange, restRange⟩ := splitRange chunk.a rule.cmp rule.value
        (rule.next, {chunk with a := newRange}) :: getRuleChunks {chunk with a := restRange} rules
      | Part.s =>
        let ⟨newRange, restRange⟩ := splitRange chunk.s rule.cmp rule.value
        (rule.next, {chunk with s := newRange}) :: getRuleChunks {chunk with s := restRange} rules

partial def acceptedChunks : HashMap String Workflow → List Chunk := fun wfls =>
  let rec helper (chunk : Chunk) : String → List Chunk
    | "A" => [chunk]
    | "R" => []
    | name =>
      let wfl := wfls.find! name
      let next := getRuleChunks chunk wfl.rules
      next.flatMap (fun ⟨nextWfl, nextChunk⟩ => helper nextChunk nextWfl)
  helper initialChunk "in"


def lines := [
"px{a<2006:qkq,m>2090:A,rfg}",
"pv{a>1716:R,A}",
"lnx{m>1548:A,A}",
"rfg{s<537:gd,x>2440:R,A}",
"qs{s>3448:A,lnx}",
"qkq{x<1416:A,crn}",
"crn{x>2662:A,R}",
"in{s<1351:px,qqz}",
"qqz{s>2770:qs,m<1801:hdj,R}",
"gd{a>3333:R,R}",
"hdj{m>838:A,pv}",
"",
"{x=787,m=2655,a=1222,s=2876}",
"{x=1679,m=44,a=2067,s=496}",
"{x=2036,m=264,a=79,s=2244}",
"{x=2461,m=1339,a=466,s=291}",
"{x=2127,m=1623,a=2188,s=1013}",
]
def data := parseLines lines
def wfls := data.1
def items := data.2
def x1 := (getRuleChunks initialChunk (wfls.find! "in").rules).head!

def runDay : List String → String := fun lns =>
  let ⟨wfls, items⟩ := parseLines lns
  let sln1 := part1 wfls items
  let sln2 := ((acceptedChunks wfls).map Chunk.size).sum 0
  s!"Part 1: {sln1}\nPart 2: {sln2}"

end Day19
