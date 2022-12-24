import Util
namespace Day13

def parseInput : List String →  List (String × String)
  | [] => []
  | p1 :: p2 :: rest => 
    match p1 with
    | "" => parseInput ([p2] ++ rest)
    | p1 => [(p1, p2)] ++ (parseInput rest)
  | _ => []



def runDay (input : List String) : String :=
  List.toString (parseInput input)

end Day13