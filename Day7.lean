namespace Day7


inductive Directory where
  | file : Nat → String → Directory
  | directory : String → List Directory → (size : Nat := 0) → Directory

open Directory in
partial def dir2String (level : String :="") : Directory → String
  | file size name => s!"\n{level}- {name} (file, size={size})"
  | directory name contents size => 
    s!"\n{level}- {name} (dir, size={size}) {contents.map (fun c => (dir2String (level++"  ") c) )}"

instance : ToString Directory where
  toString := dir2String 

open Directory in
def fs := directory "/" [
  directory "a" [
    directory "e" [
      file 584 "i"
    ],
    file 29116 "f",
    file 2557 "g",
    file 62596 "h.lst"
  ],
  file 14848514 "b.txt",
  file 8504156 "c.txt",
  directory "d" [
    file 4060174 "j",
    file 8033020 "d.log",
    file 5626152 "d.ext",
    file 7214296 "k"
  ]
]

#eval fs


instance : Inhabited Directory where
  default := Directory.directory "" []


open Directory in
partial def getSizes : Directory → Nat × Directory
| file size name => (size, file size name)
| directory name contents _ =>
  let temp := contents.map (fun c => getSizes c)
  let newDirs := temp.map (fun ⟨ _, d ⟩ => d)
  let totalSize := temp.foldl (fun sum ⟨ size, _ ⟩  => sum + size) 0
  (totalSize, directory name newDirs totalSize)

open Directory in
partial def sumSmaller : Directory → Nat
| file _ _ => 0
| directory _ contents size => 
  if size <= 100000 then
    contents.foldl (fun sum c => sum + sumSmaller c) size
  else
    contents.foldl (fun sum c => sum + sumSmaller c) 0

partial def readLines (stream : IO.FS.Stream) : IO (List String) := do 
  let line ← stream.getLine
  if line.length = 0 then
    return []
  else
    let rest ← readLines stream
    return [line.dropRightWhile Char.isWhitespace] ++ rest

open Directory in
def parseInput : (List String) → Directory := fun lines =>
  let rec helper (currDir: Directory) (path: List Directory) : List String → Directory
  | [] => currDir
  | line::lines =>
    let parts := line.splitOn " "
    match parts[0]? with
    | none => currDir
    | some "$" =>
      match parts[1]? with
      | none => currDir
      | some "cd" => 
        match parts[2]? with
        | none => currDir
        | some ".." => 
          match path with
          | [] => currDir
          | _ :: [] => currDir
          | _ :: parent :: rest => helper parent ([parent] ++ rest) lines
        | some dirName => 
          let newDir := directory dirName []
          match currDir with
          | directory name contents size =>
            let currDir' := directory name ([newDir] ++ contents) size
            if contents.length > 0 then
              let newPath := [newDir] ++ [currDir'] ++ contents.tail!
              helper newDir newPath lines
            else
              let newPath := [newDir] ++ [currDir']
              helper newDir newPath lines
          | _ => currDir
      | some "ls" => helper currDir path lines
      | _ => currDir
    | some "dir" =>
      match parts[1]? with
      | none => currDir
      | some dirName => 
        match currDir with
        | file _ _ => currDir
        | directory name contents size =>
          let currDir' := directory name ([directory dirName []] ++ contents) size
          if contents.length > 0 then
            let newPath := [currDir'] ++ contents.tail!
            helper currDir' newPath lines
          else
            let newPath := [currDir']
            helper currDir' newPath lines
    | some fSize => 
      match parts[1]? with 
      | none => currDir
      | some fName => 
        match currDir with
        | file _ _ => currDir
        | directory name contents size =>
          let currDir' := directory name ([file fSize.toNat! fName] ++ contents) size
          if contents.length > 0 then
            let newPath := [currDir'] ++ contents.tail!
            helper currDir' newPath lines
          else
            let newPath := [currDir']
            helper currDir' newPath lines

  let root := directory "/" []
  helper root [root] lines.tail!


def testIn := ["$ cd /", "$ ls", "dir a", "1000 b.txt"]
#eval parseInput testIn

def runDay : IO Unit := do
  let stdin ← IO.getStdin
  let lines ← readLines stdin
  let fs := parseInput lines

  let stdout ← IO.getStdout
  stdout.putStrLn s!"{fs}"

end Day7