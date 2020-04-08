type expr =
  | Num of int
  | Var of string
  | Plus of (expr * expr)
  | Minus of (expr * expr)
  | Mult of (expr * expr)
  | Paren of expr
  | True
  | False
  | Equal of (expr * expr)
  | Leq of (expr * expr)
  | Not of expr
  | Or of (expr * expr)
  | And of (expr * expr)
  | Skip
  | Print of expr
  | Seq of (expr * expr)
  | ESeq of expr
  | If of (expr * expr * expr)
  | Assign of (string * expr)
  | While of (expr * expr)

type prog = expr
