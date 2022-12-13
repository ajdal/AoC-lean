namespace Day7


inductive Directory where
  | file : Nat → String → Directory
  | directory : String → List Directory → (size : Nat := 0) → Directory

open Directory in
partial def dir2string (level : String :="") : Directory → String
  | file size name => s!"\n{level}- {name} (file, size={size})"
  | directory name contents size => 
    s!"\n{level}- {name} (dir, size={size}) {contents.map (fun c => (dir2string (level++"  ") c) )}"

instance : ToString Directory where
  toString := dir2string 

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

#eval getSizes fs

open Directory in
partial def sumSmaller : Directory → Nat
| file _ _ => 0
| directory _ contents size => 
  if size <= 100000 then
    contents.foldl (fun sum c => sum + sumSmaller c) size
  else
    contents.foldl (fun sum c => sum + sumSmaller c) 0
  
#eval sumSmaller (getSizes fs).snd

partial def readLines (stream : IO.FS.Stream) : IO (List String) := do 
  let line ← stream.getLine
  if line.length = 0 then
    return []
  else
    let rest ← readLines stream
    return [line.dropRightWhile Char.isWhitespace] ++ rest




def runDay : IO Unit := do
  let stdin ← IO.getStdin
  let lines ← readLines stdin

  let stdout ← IO.getStdout
  stdout.putStrLn s!"{lines}"

end Day7