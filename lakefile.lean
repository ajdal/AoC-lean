import Lake
open Lake DSL

package aoc {
  -- add package configuration options here
}

lean_lib Util
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


@[defaultTarget]
lean_exe «aoc» {
  root := `Main
}
