open ImpAST

let rec zero (mylist : string list) (a : int Assoc.context) : int Assoc.context =
  match mylist with
  | [] -> a
  | h :: t -> zero t (Assoc.update h 0 a)

let invalidate_immediate (x : string) (a : int Assoc.context) (d : string list Assoc.context) : int Assoc.context = 
  zero (Assoc.lookup x d) a

let rec invalidate_list (dep : string list) (a : int Assoc.context) (d : string list Assoc.context) : int Assoc.context =
  match dep with
  | [] -> a
  | h :: t ->
      let a' = invalidate_immediate h a d in
      let h_children = Assoc.lookup h d in
      let a'' = invalidate_list h_children a' d in
      invalidate_list t a'' d

let invalidate (x : string) (a : int Assoc.context) 
    (d : string list Assoc.context) : int Assoc.context =
  invalidate_list [x] a d

let rec search_zero (deps : string list) (a : int Assoc.context) : bool =
  match deps with
  | [] -> true
  | h :: t ->
      if (Assoc.lookup h a) = 0 then 
        (Printf.printf "Must update %s " h; 
        failwith " Dependencies not updated")
      else search_zero t a

let update (x : string) (a : int Assoc.context)
    (b : string list Assoc.context) : int Assoc.context =
  if search_zero (Assoc.lookup x b) a then Assoc.update x 1 a
  else failwith "Should not get here"


let append_env (key : string) (value : string) (env : string list Assoc.context)
    : string list Assoc.context =
  Assoc.update key (value :: (Assoc.lookup key env)) env

let rec specials_beta (v : string) (e : expr) (b : string list Assoc.context): string list Assoc.context =
  match e with
  | Num _ -> b
  | Var x -> if Assoc.mem x b then append_env v x b else b
  | Plus (e1, e2)
  | Minus (e1, e2)
  | Mult (e1, e2)
  | Equal (e1, e2)
  | Leq (e1, e2)
  | Or (e1, e2)
  | And (e1, e2) ->
      let b1 = specials_beta v e1 b in
      specials_beta v e2 b1
  | True -> b
  | False -> b
  | Paren e'
  | Not e' ->
      specials_beta v e' b
  | _ ->
      failwith "improper parsing of declare statement"

let declare_beta (v : string) (e : expr) (b : string list Assoc.context) : string list Assoc.context =
  Assoc.update v [] b |> specials_beta v e

let rec specials_delta (v : string) (e : expr) (d : string list Assoc.context) : string list Assoc.context =
  match e with
  | Num _ -> d
  | Var x -> if Assoc.mem x d then append_env x v d else d
  | Plus (e1, e2)
  | Minus (e1, e2)
  | Mult (e1, e2)
  | Equal (e1, e2)
  | Leq (e1, e2)
  | Or (e1, e2)
  | And (e1, e2) ->
      let d1 = specials_delta v e1 d in
      specials_delta v e2 d1
  | True -> d
  | False -> d
  | Paren e'
  | Not e' ->
      specials_delta v e' d
  | _ ->
      failwith "improper parsing of declare statement"

let declare_delta (v : string) (e : expr) (b : string list Assoc.context) : string list Assoc.context =
  Assoc.update v [] b |> specials_delta v e

let rec help_intersect (keys : string list) (b1 : int Assoc.context)
    (b2 : int Assoc.context) (builder : int Assoc.context) : int Assoc.context =
  match keys with
  | [] -> builder
  | h :: t -> 
      if (Assoc.mem h b2) then
        Assoc.update h (min (Assoc.lookup h b1) (Assoc.lookup h b2))
          (help_intersect t b1 b2 builder)
      else
        help_intersect t b1 b2 builder

let intersect (prev_b : int Assoc.context) (branch_b : int Assoc.context) : int Assoc.context =
  let keys = Assoc.keys prev_b in
  help_intersect keys prev_b branch_b Assoc.empty

let intersect_if (a : int Assoc.context) (a1 : int Assoc.context) (a2 : int Assoc.context) : int
    Assoc.context =
  let keys = Assoc.keys a in
  help_intersect keys a1 a2 Assoc.empty
