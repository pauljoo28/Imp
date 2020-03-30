type aexp =
  | Num of int

type bexp =
  | True
  | False

type com = 
  | Skip
  (* We add print as a command for ease of use *)
  (* The semantics are just evaluate aexp until we get a number *)
  (* Then print that number and return "Skip" *)
  | Print of aexp

type prog = com