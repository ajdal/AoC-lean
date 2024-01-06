namespace Day7


inductive Directory where
  | file : Nat → String → Directory
  | directory : String → List Directory → (size : Nat := 0) → Directory

open Directory

partial def dir2String (level : String :="") : Directory → String
  | file size name => s!"\n{level}- {name} (file, size={size})"
  | directory name contents size =>
    s!"\n{level}- {name} (dir, size={size}) {contents.map (fun c => (dir2String (level++"  ") c) )}"

instance : ToString Directory where
  toString := dir2String

def insertDirectory : Directory → Directory → Directory
  | newDir, directory name contents size => directory name ([newDir] ++ contents) size
  | _, f => f

def getName : Directory → String
  | file _ name => name
  | directory name _ _ => name

def getSize : Directory → Nat
  | file size _ => size
  | directory _ _ size => size

def getContents : Directory → List Directory
  | file _ _ => []
  | directory _ contents _ => contents

def insertAt (root: Directory) : Directory → List String → Option Directory
  | d, [] => insertDirectory d root
  | d, h :: t =>
    match root with
    | file _ _ => none
    | directory rootName rootContents rootSize =>
      let mbDir := rootContents.find? (fun c =>
        match c with
        | file _ _ => false
        | directory childName _ _ => childName == h
      )
      match mbDir with
      | none => none
      | some (file _ _) => none
      | some childDir =>
        let mbNewDir := insertAt childDir d t
        match mbNewDir with
        | none => none
        | some newDir =>
          let temp := rootContents.filter (fun c =>
            match c with
            | file _ _ => true
            | directory cName _ _ => cName != (getName childDir)
          )
          let newContents := [newDir] ++ temp
          directory rootName newContents rootSize

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

def fs2 := directory "/" [
  directory "a" [
    directory "e" [
      file 584 "i.txt",
      directory "x" [
        directory "y" []
      ]
    ]
  ]
]
instance : Inhabited Directory where
  default := Directory.directory "" []

partial def getSizes : Directory → Nat × Directory
| file size name => (size, file size name)
| directory name contents _ =>
  let sizes := contents.map (fun c => getSizes c)
  let newDirs := sizes.map (fun ⟨ _, d ⟩ => d)
  let totalSize := sizes.foldl (fun sum ⟨ size, _ ⟩  => sum + size) 0
  (totalSize, directory name newDirs totalSize)

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

def parseInput : (List String) → Directory := fun lines =>
  let rec helper (root: Directory) (path: List String) : List String → Directory
  | [] => root
  | line::lines =>
    let parts := line.splitOn " "
    match parts[0]? with
    | none => root
    | some "$" =>
      match parts[1]? with
      | none => root
      | some "cd" =>
        match parts[2]? with
        | none => root
        | some ".." => helper root path.dropLast lines
        | some dirName => helper root (path ++ [dirName]) lines
      | some "ls" => helper root path lines
      | _ => root
    | some "dir" =>
      match parts[1]? with
      | none => root
      | some dirName =>
        match insertAt root (directory dirName []) path with
        | none => root
        | some newRoot => helper newRoot path lines
    | some fSize =>
      match parts[1]? with
      | none => root
      | some fName =>
        match String.toNat? fSize with
        | none => root
        | some size =>
          match insertAt root (file size fName) path with
          | none => root
          | some newRoot => helper newRoot path lines

  let root := directory "/" []
  let newRoot := helper root [] lines.tail!
  newRoot

def find (root : Directory) : Directory :=
  let rec helper (need : Nat) (min : Directory) (minSize : Nat) : List Directory → Directory := fun contents =>
    match contents with
    | [] => min
    | d::ds =>
      match d with
      | directory _ childContents size =>
        if size < need then
          helper need min minSize ds
        else if size < minSize then
          let minOfContents := helper need d size childContents
          helper need minOfContents (getSize minOfContents) ds
        else
          let minOfContents := helper need d size childContents
          if (getSize minOfContents) < minSize then
            helper need minOfContents (getSize minOfContents) ds
          else
            helper need min minSize ds
      | file _ _ => helper need min minSize ds
  let used := getSize root
  let need := used - 40000000
  helper need root used (getContents root)

def runDay : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let lines ← readLines stdin
  let fs := parseInput lines
  let root := (getSizes fs).snd
  let minDir := find root
  stdout.putStrLn s!"{minDir}"

end Day7
