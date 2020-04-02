type aexp =
  | Num of int
  | Plus of (aexp * aexp)
  | Minus of (aexp * aexp)
  | Mult of (aexp * aexp)

type bexp =
  | True
  | False
  | Equal of (aexp * aexp)
  | Leq of (aexp * aexp)
  | Not of bexp
  | Or of (bexp * bexp)
  | And of (bexp * bexp)

type com = 
  | Skip
  (* We add print as a command for ease of use *)
  (* The semantics are just evaluate aexp until we get a number *)
  (* Then print that number and return "Skip" *)
  | APrint of aexp
  | BPrint of bexp
  | Seq of (com * com)
  | ESeq of com

type prog = com
