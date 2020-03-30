type aexp =
  | Num of int

type bexp =
  | True
  | False

type com = 
  | Skip
  | Print of aexp

type prog = com