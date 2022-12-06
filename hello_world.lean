def add1 (n : Nat) : Nat := n  + 1

#eval add1 6

structure Point where
  x : Float
  y : Float
deriving Repr

def p : Point := {x := 0.0, y := 1.0}
#eval p

def main : IO Unit := IO.println "Hello World!"