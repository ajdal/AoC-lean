import Util
import State
import Std.Data.HashMap

namespace Day12
open Std State

inductive Tile where
  | Dmgd
  | Oper
  | Unwn
deriving BEq, Hashable

def Tile.toString : Tile → String
  | Dmgd => "#"
  | Oper => "."
  | Unwn => "?"

instance : Repr Tile where
  reprPrec t _ := t.toString

instance : ToString Tile where
  toString t := t.toString

def Tile.ofChar : Char → Tile
  | '#' => .Dmgd
  | '.' => .Oper
  | _ => .Unwn

-- ???.### 1,1,3
-- springs = [???.###]
-- groups = [1,1,3]
structure Record where
  springs : List Tile
  groups : List Nat
deriving Repr, Inhabited, BEq, Hashable

instance : ToString Record where
  toString r := s!"{r.springs} {r.groups}"

def tryFixN (n : Nat) : List Tile → Option (List Tile) := fun ts =>
  match n with
  | 0 => some ts
  | n + 1 => match ts with
    | [] => none
    | .Oper :: _ => none
    | _ :: ts => tryFixN n ts

partial def countArrangements : Record → Nat := fun record =>
  let rec helper : Record → Nat := fun ⟨springs, groups⟩ =>
    match springs, groups with
      | [], [] => 1
      | [], 0 :: [] => 1
      | [], _ => 0
      | .Dmgd :: _, [] => 0
      | _ :: ss, [] => helper {springs := ss, groups := groups}
      | s :: ss, g :: gs => match s, g with
        | .Oper, 0 => helper {springs := ss, groups := gs}
        | .Oper, _ => helper {springs := ss, groups := groups}
        | .Dmgd, 0 => 0
        | .Dmgd, _ => match tryFixN g springs with
          | none => 0
          | some ss => helper {springs := ss, groups := 0 :: gs}
        | .Unwn, 0 => helper {springs := ss, groups := gs}
        | .Unwn, _ =>
          let oper := helper {springs := ss, groups := groups}
          match tryFixN g springs with
            | none => oper
            | some ss =>
              let dmgd := helper {springs := ss, groups := 0 :: gs}
              dmgd + oper
  helper record

partial def countArrangementsMemo : Record → Nat := fun record =>
  let rec helper : Record → State (HashMap Record Nat) Nat := fun ⟨springs, groups⟩ => do
    let cache ← getState
    match cache.find? {springs := springs, groups := groups} with
    | some cnt =>
      return cnt
    | none =>
      match springs, groups with
        | [], [] => return 1
        | [], 0 :: [] => return 1
        | [], _ => return 0
        | .Dmgd :: _, [] => return 0
        | _ :: ss, [] =>
          let cnt ← helper {springs := ss, groups := groups}
          let cache ← getState
          let cache := (cache.insert ⟨springs, groups⟩ cnt)
          setState cache
          return cnt

        | s :: ss, g :: gs => match s, g with
          | .Oper, 0 =>
            let cnt ← helper {springs := ss, groups := gs}
            let cache ← getState
            let cache := (cache.insert ⟨springs, groups⟩ cnt)
            setState cache
            return cnt

          | .Oper, _ =>
            let cnt ← helper {springs := ss, groups := groups}
            let cache ← getState
            let cache := (cache.insert ⟨springs, groups⟩ cnt)
            setState cache
            return cnt

          | .Dmgd, 0 =>
            let cnt := 0
            let cache := (cache.insert ⟨springs, groups⟩ cnt)
            setState cache
            return cnt

          | .Dmgd, _ => match tryFixN g springs with
            | none =>
              let cnt := 0
              let cache := (cache.insert ⟨springs, groups⟩ cnt)
              setState cache
              return cnt

            | some ss =>
              let cnt ← helper {springs := ss, groups := 0 :: gs}
              let cache ← getState
              let cache := (cache.insert ⟨springs, groups⟩ cnt)
              setState cache
              return cnt

          | .Unwn, 0 =>
            let cnt ← helper {springs := ss, groups := gs}
            let cache ← getState
            let cache := (cache.insert ⟨springs, groups⟩ cnt)
            setState cache
            return cnt

          | .Unwn, _ =>
            let oper ← helper {springs := ss, groups := groups}
            let cache ← getState
            match tryFixN g springs with
              | none =>
                let cache := (cache.insert ⟨springs, groups⟩ oper)
                setState cache
                return oper

              | some ss =>
                let dmgd ← helper {springs := ss, groups := 0 :: gs}
                let cache ← getState
                let cache := (cache.insert ⟨springs, groups⟩ (dmgd + oper))
                setState cache
                return dmgd + oper
  let cache := HashMap.empty
  let (_, val) := helper record cache
  val


def extendInput : Record → Record := fun ⟨springs, groups⟩ =>
  let rec extendSprings (n : Nat) : List Tile → List Tile := fun ts =>
    if n = 0 then
      ts
    else
      ts ++ .Unwn :: extendSprings (n-1) ts
  let rec extendGroups (n : Nat) : List Nat → List Nat := fun gps =>
    if n = 0 then
      []
    else
      gps ++ extendGroups (n-1) gps
  ⟨ extendSprings 4 springs, extendGroups 5 groups ⟩

def parseLines : List String → List Record
  | [] => []
  | ln :: lns =>
    let x := ln.splitOn " "
    let springs := List.map Tile.ofChar x.head!.data
    let nums := (x.get! 1).splitOn ","
    let groups := List.map String.toNat! nums
    {springs := springs, groups := groups} :: (parseLines lns)

def runDay : List String → String := fun lns =>
  let data := parseLines lns
  let sln1 := Util.sum 0 (List.map countArrangements data)
  let sln2 := Util.sum 0 (List.map countArrangementsMemo (data.map extendInput))
  s!"Part 1: {sln1}\nPart 2: {sln2}"

end Day12
