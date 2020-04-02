(* Small-step interpreter for imp *)
(* Big-step interpreter setup left as an exercise *)
(* Note that I'm being extremely pedantic in this implementation *)
(* This is intentional to get started, but you can make it less annoying if you want *)

open ImpAST

type sigma = int Assoc.context

let rec step_aexp (a : aexp) : aexp =
  match a with
  | Num n -> Num n
  | Minus (n1, n2) -> 
      (match step_aexp n1, step_aexp n2 with
      | Num n1', Num n2' -> Num (n1' - n2')
      | _ -> failwith "Minus did not reduce down num value")
  | Plus (n1, n2) -> 
      (match step_aexp n1, step_aexp n2 with
      | Num n1', Num n2' -> Num (n1' + n2')
      | _ -> failwith "Plus did not reduce down num value")
  | Mult (n1, n2) -> 
      (match step_aexp n1, step_aexp n2 with
      | Num n1', Num n2' -> Num (n1' * n2')
      | _ -> failwith "Mult did not reduce down num value")

let rec step_bexp (b : bexp) : bexp =
  match b with
  | True -> True
  | False -> False
  | Equal (n1, n2) ->
      (match step_aexp n1, step_aexp n2 with
      | Num n1', Num n2' -> if n1' = n2' then True else False
      | _ -> failwith "Equal did not reduce down to num value")
  | Leq (n1, n2) ->
      (match step_aexp n1, step_aexp n2 with
      | Num n1', Num n2' -> if n1' <= n2' then True else False
      | _ -> failwith "Equal did not reduce down to num value")
  | Not b ->
      (match step_bexp b with
      | True -> False
      | False -> True
      | _ -> failwith "Not did not reduce down to bool value")
  | Or (b1, b2) ->
      (match step_bexp b1, step_bexp b2 with
      | True, True -> True
      | True, False -> True
      | False, True -> True
      | False, False -> False
      | _ -> failwith "Or did not reduce down to bool value")
  | And (b1, b2) ->
      (match step_bexp b1, step_bexp b2 with
      | True, True -> True
      | True, False -> False
      | False, True -> False
      | False, False -> False
      | _ -> failwith "And did not reduce down to bool value")

let rec step_com (c : com) : com =
  match c with
  | Skip -> failwith "No evaluation possible for skip"
  | APrint a -> 
    (match step_aexp a with
      | Num n -> print_endline (string_of_int n); Skip
      | _ -> failwith "aexp did not reduce down num value")
  | BPrint b -> 
    (match step_bexp b with
      | True -> print_endline (string_of_bool true); Skip
      | False -> print_endline (string_of_bool false); Skip
      | _ -> failwith "aexp did not reduce down to num value")
  | Seq (c1, c2) ->
    (match step_com c1, step_com c2 with
      | Skip, Skip -> Skip
      | _ -> failwith "Did not fully evaluate program with seq")
  | ESeq c ->
    (match step_com c with
      | Skip -> Skip
      | _ -> failwith "Did not fully evaluate program with seq")

let rec eval_prog (p : prog) : unit =
  match p with
  | Skip -> ()
  | _ -> eval_prog (step_com p)
