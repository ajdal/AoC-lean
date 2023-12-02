import Lake
open Lake DSL

package aoc {
  -- add package configuration options here
}

require Qq from git
  "https://github.com/gebner/quote4.git"

lean_lib Util {
  srcDir := "AoC-Lean/Util",
  roots := #[`Grid, `NatInf, `Range, `Set, `Stack, `State, `Util]
}

lean_lib AoC2022 {
  srcDir := "AoC-Lean"
}

@[default_target]
lean_exe «aoc» {
  root := `Main
}
