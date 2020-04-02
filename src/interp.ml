(* Small-step interpreter for imp *)
(* Big-step interpreter setup left as an exercise *)
(* Note that I'm being extremely pedantic in this implementation *)
(* This is intentional to get started, but you can make it less annoying if you want *)

open ImpAST

type sigma = int Assoc.context

let rec step_aexp (a : aexp) : aexp =
  match a with
  | Num n -> Num n

let step_bexp (b : bexp) : bexp =
  match b with
  | True -> failwith "No evaluation possible for true"
  | False -> failwith "No evaluation possible for false"

let rec step_com (c : com) : com =
  match c with
  | Skip -> failwith "No evaluation possible for skip"
  | APrint a -> 
    (match a with
      | Num n -> print_endline (string_of_int n); Skip)
  | BPrint b -> 
    (match b with
      | True -> print_endline (string_of_bool true); Skip
      | False -> print_endline (string_of_bool false); Skip)

let rec eval_prog (p : prog) : unit =
  match p with
  | Skip -> ()
  | _ -> eval_prog (step_com p)
