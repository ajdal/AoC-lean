import Lean
open Lean

namespace Day16

declare_syntax_cat twoletter

syntax "[twoletter|" "my bag holds" ident,+ "balls" "]" : term

open Lean Elab Macro Tactic in
macro_rules
| `([twoletter| my bag holds $xs,* balls ]) => do
    let mut outs : Array (TSyntax `str) := #[]
    for x in xs.getElems do
      let xstr := x.getId.toString

      -- TODO: what is the right way to index a String?
      if xstr.length = 2 && (xstr.get! ⟨0⟩).isUpper &&  (xstr.get! ⟨1⟩).isUpper then
        do outs := outs.push (Lean.quote xstr)
        else Macro.throwErrorAt x s!"expected two letter uppercase string, found '{xstr}'"
    `([ $outs,* ])

#reduce [twoletter| my bag holds AA, BB, CC balls]  -- ["AA", "BB", "CC"]
#reduce [twoletter| my bag holds AA, BB, CX balls]  -- expected two letter repeated string, found 'CX'
#reduce [twoletter| my bag holds AA, BB, Cx balls]  -- expected two letter repeated string, found 'CX'
#reduce [twoletter| my bag holds AAA balls]  -- expected two letter repeated string, found 'AAA'


syntax "Valve " str " has flow rate=" num "; tunnels lead to valves " str,+ : term 



#eval ("AB".get! ⟨ 0 ⟩).isUpper




end Day16


-- Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
-- Valve BB has flow rate=13; tunnels lead to valves CC, AA
-- Valve CC has flow rate=2; tunnels lead to valves DD, BB
-- Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
-- Valve EE has flow rate=3; tunnels lead to valves FF, DD
-- Valve FF has flow rate=0; tunnels lead to valves EE, GG
-- Valve GG has flow rate=0; tunnels lead to valves FF, HH
-- Valve HH has flow rate=22; tunnel leads to valve GG
-- Valve II has flow rate=0; tunnels lead to valves AA, JJ
-- Valve JJ has flow rate=21; tunnel leads to valve II