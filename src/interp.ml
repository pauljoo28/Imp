(* Small-step interpreter for imp *)
(* Big-step interpreter setup left as an exercise *)
(* Note that I'm being extremely pedantic in this implementation *)
(* This is intentional to get started, but you can make it less annoying if you want *)

open ImpAST
open Check

type sigma = int Assoc.context
type tau = expr Assoc.context

let rec step_expr (e : expr) (s : int Assoc.context) (t : expr Assoc.context) : 
  (expr * int Assoc.context * expr Assoc.context) =
  match e with
  | Num n -> Num n, s, t
  | Var v -> Num (Assoc.lookup v s), s, t
  | Minus (n1, n2) -> 
      (match step_expr n1 s t, step_expr n2 s t with
      | (Num n1', _, _), (Num n2', _, _) -> Num (n1' - n2'), s, t
      | _ -> failwith "Minus did not reduce down num value")
  | Plus (n1, n2) -> 
      (match step_expr n1 s t, step_expr n2 s t with
      | (Num n1', _, _), (Num n2', _, _) -> Num (n1' + n2'), s, t
      | _ -> failwith "Plus did not reduce down num value")
  | Mult (n1, n2) -> 
      (match step_expr n1 s t, step_expr n2 s t with
      | (Num n1', _, _), (Num n2', _, _) -> Num (n1' * n2'), s, t
      | _ -> failwith "Mult did not reduce down num value")
  | Paren a ->
    (match step_expr a s t with
      | (Num n, _, _) -> Num n, s, t
      | (True, _, _) -> True, s, t
      | (False, _, _) -> False, s, t
      | _ -> failwith "Did not fully evaluate program to num")
  | True -> True, s, t
  | False -> False, s, t
  | Equal (n1, n2) ->
      (match step_expr n1 s t, step_expr n2 s t with
      | (Num n1', _, _), (Num n2', _, _) -> if n1' = n2' then True, s, t else
          False, s, t
      | _ -> failwith "Equal did not reduce down to num value")
  | Leq (n1, n2) ->
      (match step_expr n1 s t, step_expr n2 s t with
      | (Num n1', _, _), (Num n2', _, _) -> if n1' <= n2' then True, s, t else 
          False, s, t
      | _ -> failwith "Equal did not reduce down to num value")
  | Not b ->
      (match step_expr b s t with
      | True, _, _ -> False, s, t
      | False, _, _ -> True, s, t
      | _ -> failwith "Not did not reduce down to bool value")
  | Or (b1, b2) ->
      (match step_expr b1 s t, step_expr b2 s t with
      | (True, _, _), (True, _, _) -> True, s, t
      | (True, _, _), (False, _, _) -> True, s, t
      | (False, _, _), (True, _, _) -> True, s, t
      | (False, _, _), (False, _, _) -> False, s, t
      | _ -> failwith "Or did not reduce down to bool value")
  | And (b1, b2) ->
      (match step_expr b1 s t, step_expr b2 s t with
      | (True, _, _), (True, _, _) -> True, s, t
      | (True, _, _), (False, _, _) -> False, s, t
      | (False, _, _), (True, _, _) -> False, s, t
      | (False, _, _), (False, _, _) -> False, s, t
      | _ -> failwith "And did not reduce down to bool value")
  | Skip -> Skip, s, t
  | Print a -> 
    (match step_expr a s t with
      | Num n, _, _ -> print_endline (string_of_int n); (Skip, s, t)
      | True, _, _ -> print_endline (string_of_bool true); (Skip, s, t)
      | False, _, _ -> print_endline (string_of_bool false); (Skip, s, t)
      | _ -> failwith "aexp did not reduce down num value")
  | Seq (c1, c2) ->
    (match step_expr c1 s t with
      | (Skip, s1', t1') -> 
          (match step_expr c2 s1' t1' with
            | (Skip, s2', t2') -> (Skip, s2', t2')
            | _ -> failwith "Did not fully evaluate program with seq")
      | _ -> failwith "Did not fully evaluate program with seq")
  | ESeq c ->
    (match step_expr c s t with
      | (Skip, s', t') -> (Skip, s', t')
      | _ -> failwith "Did not fully evaluate program with seq")
  | If (b, c1, c2) ->
    (match step_expr b s t with
      | True, _, _ -> step_expr c1 s t
      | False, _, _ -> step_expr c2 s t
      | _ -> failwith "If statement did not fully evaluate program")
  | Assign (v, a) ->
    (match step_expr a s t with
      | Num n, _, _ -> (Skip, Assoc.update v n s, t)
      | _ -> failwith "aexp did not reduce down num value")
  | While (b, c) ->
      let c1' = Seq (c, While (b, c)) in
      let c2' = Skip in
      step_expr (If (b, c1', c2')) s t
  | Declare (v, e) -> 
    (match step_expr e s t with
     | Num n, _, _ -> (Skip, Assoc.update v n s, Assoc.update v e t)
      | _ -> failwith "aexp did not reduce to num value")
  | Update v ->
    let e = Assoc.lookup v t in
    (match step_expr e s t with
      | Num n, _, _ -> (Skip, Assoc.update v n s, t)
      | _ -> failwith "aexp did not reduce to num in update statement")
      
    
let rec eval_prog (p : prog) : unit =
  match p with
  | Skip -> ()
  | _ -> 
    check_prog p;
    match (step_expr p Assoc.empty Assoc.empty) with
    | p', _, _ -> eval_prog p'
