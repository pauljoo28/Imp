open ImpAST

let rec zero (mylist : string list) (a : int Assoc.context) : int Assoc.context =
  match mylist with
  | [] -> a
  | h :: t -> zero t (Assoc.update h 0 a)

let invalidate (x : string) (a : int Assoc.context) (d : string list Assoc.context) : int Assoc.context = 
  zero (Assoc.lookup x d) a

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
