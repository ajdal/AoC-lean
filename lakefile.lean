import Lake
open Lake DSL

package aoc {
  -- add package configuration options here
}

lean_lib Util {
  srcDir := "Util",
  roots := #[
    `Grid,
    `NatInf, 
    `Range,
    `Set,
    `Stack,
    `State,
    `Util
  ]
}
lean_lib Day1
lean_lib Day2
lean_lib Day3
lean_lib Day4
lean_lib Day5
lean_lib Day6
lean_lib Day7
lean_lib Day8
lean_lib Day9
lean_lib Day10
lean_lib Day11
lean_lib Day12
lean_lib Day13
lean_lib Day14
lean_lib Day15

require Qq from git
  "https://github.com/gebner/quote4.git"

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"


@[default_target]
lean_exe «aoc» {
  root := `Main
}
