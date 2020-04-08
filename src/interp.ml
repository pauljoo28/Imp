(* Small-step interpreter for imp *)
(* Big-step interpreter setup left as an exercise *)
(* Note that I'm being extremely pedantic in this implementation *)
(* This is intentional to get started, but you can make it less annoying if you want *)

open ImpAST

type sigma = int Assoc.context

let rec step_expr (e : expr) (s : int Assoc.context) : 
  (expr * int Assoc.context) =
  match e with
  | Num n -> Num n, s
  | Var v -> Num (Assoc.lookup v s), s
  | Minus (n1, n2) -> 
      (match step_expr n1 s, step_expr n2 s with
      | (Num n1', _), (Num n2', _) -> Num (n1' - n2'), s
      | _ -> failwith "Minus did not reduce down num value")
  | Plus (n1, n2) -> 
      (match step_expr n1 s, step_expr n2 s with
      | (Num n1', _), (Num n2', _) -> Num (n1' + n2'), s
      | _ -> failwith "Plus did not reduce down num value")
  | Mult (n1, n2) -> 
      (match step_expr n1 s, step_expr n2 s with
      | (Num n1', _), (Num n2', _) -> Num (n1' * n2'), s
      | _ -> failwith "Mult did not reduce down num value")
  | Paren a ->
    (match step_expr a s with
      | (Num n, _) -> Num n, s
      | (True, _) -> True, s
      | (False, _) -> False, s
      | _ -> failwith "Did not fully evaluate program to num")
  | True -> True, s
  | False -> False, s
  | Equal (n1, n2) ->
      (match step_expr n1 s, step_expr n2 s with
      | (Num n1', _), (Num n2', _) -> if n1' = n2' then True, s else False, s
      | _ -> failwith "Equal did not reduce down to num value")
  | Leq (n1, n2) ->
      (match step_expr n1 s, step_expr n2 s with
      | (Num n1', _), (Num n2', _) -> if n1' <= n2' then True, s else False, s
      | _ -> failwith "Equal did not reduce down to num value")
  | Not b ->
      (match step_expr b s with
      | True, _ -> False, s
      | False, _ -> True, s
      | _ -> failwith "Not did not reduce down to bool value")
  | Or (b1, b2) ->
      (match step_expr b1 s, step_expr b2 s with
      | (True, _), (True, _) -> True, s
      | (True, _), (False, _) -> True, s
      | (False, _), (True, _) -> True, s
      | (False, _), (False, _) -> False, s
      | _ -> failwith "Or did not reduce down to bool value")
  | And (b1, b2) ->
      (match step_expr b1 s, step_expr b2 s with
      | (True, _), (True, _) -> True, s
      | (True, _), (False, _) -> False, s
      | (False, _), (True, _) -> False, s
      | (False, _), (False, _) -> False, s
      | _ -> failwith "And did not reduce down to bool value")
  | Skip -> Skip, s
  | Print a -> 
    (match step_expr a s with
      | Num n, _ -> print_endline (string_of_int n); (Skip, s)
      | True, _ -> print_endline (string_of_bool true); (Skip, s)
      | False, _ -> print_endline (string_of_bool false); (Skip, s)
      | _ -> failwith "aexp did not reduce down num value")
  | Seq (c1, c2) ->
    (match step_expr c1 s with
      | (Skip, s1') -> 
          (match step_expr c2 s1' with
            | (Skip, s2') -> (Skip, s2')
            | _ -> failwith "Did not fully evaluate program with seq")
      | _ -> failwith "Did not fully evaluate program with seq")
  | ESeq c ->
    (match step_expr c s with
      | (Skip, s') -> (Skip, s')
      | _ -> failwith "Did not fully evaluate program with seq")
  | If (b, c1, c2) ->
    (match step_expr b s with
      | True, _ -> step_expr c1 s
      | False, _ -> step_expr c2 s
      | _ -> failwith "If statement did not fully evaluate program")
  | Assign (v, a) ->
    (match step_expr a s with
      | Num n, _ -> (Skip, Assoc.update v n s)
      | _ -> failwith "aexp did not reduce down num value")
  | While (b, c) ->
      let c1' = Seq (c, While (b, c)) in
      let c2' = Skip in
      step_expr (If (b, c1', c2')) s
    
let rec eval_prog (p : prog) : unit =
  match p with
  | Skip -> ()
  | _ -> 
      match (step_expr p Assoc.empty) with
      | p', _ -> eval_prog p'
