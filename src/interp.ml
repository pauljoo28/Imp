(* Small-step interpreter for imp *)
(* Big-step interpreter setup left as an exercise *)
(* Note that I'm being extremely pedantic in this implementation *)
(* This is intentional to get started, but you can make it less annoying if you want *)

open ImpAST

type sigma = int Assoc.context

let rec step_aexp (a : aexp) (s : int Assoc.context) : aexp =
  match a with
  | Num n -> Num n
  | Var v -> Num (Assoc.lookup v s)
  | Minus (n1, n2) -> 
      (match step_aexp n1 s, step_aexp n2 s with
      | Num n1', Num n2' -> Num (n1' - n2')
      | _ -> failwith "Minus did not reduce down num value")
  | Plus (n1, n2) -> 
      (match step_aexp n1 s, step_aexp n2 s with
      | Num n1', Num n2' -> Num (n1' + n2')
      | _ -> failwith "Plus did not reduce down num value")
  | Mult (n1, n2) -> 
      (match step_aexp n1 s, step_aexp n2 s with
      | Num n1', Num n2' -> Num (n1' * n2')
      | _ -> failwith "Mult did not reduce down num value")
  | AParen a ->
    (match step_aexp a s with
      | Num n -> Num n
      | _ -> failwith "Did not fully evaluate program to num")

let rec step_bexp (b : bexp) (s : int Assoc.context) : bexp =
  match b with
  | True -> True
  | False -> False
  | Equal (n1, n2) ->
      (match step_aexp n1 s, step_aexp n2 s with
      | Num n1', Num n2' -> if n1' = n2' then True else False
      | _ -> failwith "Equal did not reduce down to num value")
  | Leq (n1, n2) ->
      (match step_aexp n1 s, step_aexp n2 s with
      | Num n1', Num n2' -> if n1' <= n2' then True else False
      | _ -> failwith "Equal did not reduce down to num value")
  | Not b ->
      (match step_bexp b s with
      | True -> False
      | False -> True
      | _ -> failwith "Not did not reduce down to bool value")
  | Or (b1, b2) ->
      (match step_bexp b1 s, step_bexp b2 s with
      | True, True -> True
      | True, False -> True
      | False, True -> True
      | False, False -> False
      | _ -> failwith "Or did not reduce down to bool value")
  | And (b1, b2) ->
      (match step_bexp b1 s, step_bexp b2 s with
      | True, True -> True
      | True, False -> False
      | False, True -> False
      | False, False -> False
      | _ -> failwith "And did not reduce down to bool value")
  | BParen b ->
    (match step_bexp b s with
      | True -> True
      | False -> False
      | _ -> failwith "Did not fully evaluate program with paren")

let rec step_com (c : com) (s : int Assoc.context) : (com * int Assoc.context) =
  match c with
  | Skip -> Skip, s
  | APrint a -> 
    (match step_aexp a s with
      | Num n -> print_endline (string_of_int n); (Skip, s)
      | _ -> failwith "aexp did not reduce down num value")
  | BPrint b -> 
    (match step_bexp b s with
      | True -> print_endline (string_of_bool true); (Skip, s)
      | False -> print_endline (string_of_bool false); (Skip, s)
      | _ -> failwith "aexp did not reduce down to num value")
  | Seq (c1, c2) ->
    (match step_com c1 s with
      | (Skip, s1') -> 
          (match step_com c2 s1' with
            | (Skip, s2') -> (Skip, s2')
            | _ -> failwith "Did not fully evaluate program with seq")
      | _ -> failwith "Did not fully evaluate program with seq")
  | ESeq c ->
    (match step_com c s with
      | (Skip, s') -> (Skip, s')
      | _ -> failwith "Did not fully evaluate program with seq")
  | CParen c ->
    (match step_com c s with
      | (Skip, s') -> (Skip, s')
      | _ -> failwith "Did not fully evaluate program with paren")
  | If (b, c1, c2) ->
    (match step_bexp b s with
      | True -> step_com c1 s
      | False -> step_com c2 s
      | _ -> failwith "If statement did not fully evaluate program")
  | Assign (v, a) ->
    (match step_aexp a s with
      | Num n -> (Skip, Assoc.update v n s)
      | _ -> failwith "aexp did not reduce down num value")
  | While (b, c) ->
      let c1' = Seq (c, While (b, c)) in
      let c2' = Skip in
      step_com (If (b, c1', c2')) s
    
let rec eval_prog (p : prog) : unit =
  match p with
  | Skip -> ()
  | _ -> 
      match (step_com p Assoc.empty) with
      | p', _ -> eval_prog p'
