open ImpAST

type gamma = typ Assoc.context

type valid = Valid | Invalid

type alpha = valid Assoc.context
type beta = string list Assoc.context
type delta = string list Assoc.context

let rec check_expr (e : expr) (g : gamma) (a : alpha) (b : beta) (d : delta)
  : (typ * gamma * alpha * beta * delta) =
  (match e with
    | Num n -> Int, g, a, b, d
    | Var v -> Assoc.lookup v g, g, a, b, d
    | True -> Bool, g, a, b, d
    | False -> Bool, g, a, b, d
    | Skip -> Unit, g, a, b, d
    | Plus (a1, a2) ->
        (match check_expr a1 g a b d, check_expr a2 g a b d with
          | (Int, _, _, _, _), (Int, _, _, _, _) -> Int, g, a, b, d
          | _, _ -> failwith "plus rule violated")
    | Minus (a1, a2) ->
        (match check_expr a1 g a b d, check_expr a2 g a b d with
          | (Int, _, _, _, _), (Int, _, _, _, _) -> Int, g, a, b, d
          | _, _ -> failwith "minus rule violated")
    | Mult (a1, a2) ->
        (match check_expr a1 g a b d, check_expr a2 g a b d with
          | (Int, _, _, _, _), (Int, _, _, _, _) -> Int, g, a, b, d
          | _, _ -> failwith "mult rule violated")
    | Equal (a1, a2) ->
        (match check_expr a1 g a b d, check_expr a2 g a b d with
          | (Int, _, _, _, _), (Int, _, _, _, _) -> Bool, g, a, b, d
          | _, _ -> failwith "equal rule violated")
    | Leq (a1, a2) ->
        (match check_expr a1 g a b d, check_expr a2 g a b d with
          | (Int, _, _, _, _), (Int, _, _, _, _) -> Bool, g, a, b, d
          | _, _ -> failwith "leq rule violated")
    | And (b1, b2) ->
        (match check_expr b1 g a b d, check_expr b2 g a b d with
          | (Bool, _, _, _, _), (Bool, _, _, _, _) -> Bool, g, a, b, d
          | _, _ -> failwith "and rule violated")
    | Or (b1, b2) ->
        (match check_expr b1 g a b d, check_expr b2 g a b d  with
          | (Bool, _, _, _, _), (Bool, _, _, _, _) -> Bool, g, a, b, d
          | _, _ -> failwith "or rule violated")
    | Not b1 ->
        (match check_expr b1 g a b d with
          | Bool, _, _, _, _ -> Bool, g, a, b, d
          | _ -> failwith "not rule violated")
    | Print (com) ->
        (match check_expr com g a b d with
          | Bool, _, _, _, _ -> Unit, g, a, b, d
          | Int, _, _, _, _ -> Unit, g, a, b, d
          | _ -> failwith "print rule violated")
    | Seq (c1, c2) ->
        (match check_expr c1 g a b d with
          | (Unit, g', _, _, _) -> 
              (match check_expr c2 g' a b d with
                | (Unit, g'', _, _, _) -> Unit, g'', a, b, d
                | _ -> failwith "Seq rule violated")
          | _ -> failwith "Seq rule violated")
    | ESeq c1 ->
        (match check_expr c1 g a b d with
          | Unit, g', _, _, _ -> Unit, g', a, b, d
          | _ -> failwith "ESeq rule violated")
    | Paren c ->
        (match check_expr c g a b d with
          | Int, g', _, _, _ -> Int, g', a, b, d
          | Bool, g', _, _, _ -> Bool, g', a, b, d
          | Unit, g', _, _, _ -> Unit, g', a, b, d)
    | If (b1, c1, c2) ->
        (match check_expr b1 g a b d, check_expr c1 g a b d, check_expr c2
                 g a b d with
          | (Bool, _, _, _, _), (Unit, _, _, _, _), (Unit, _, _, _, _) -> 
                Unit, g, a, b, d
          | _ -> failwith "If statement rule violated")
    | While (b1, c) ->
        (match check_expr b1 g a b d, check_expr c g a b d with
          | (Bool, _, _, _, _), (Unit, _, _, _, _) -> Unit, g, a, b, d
          | _ -> failwith "while statement rule violated")
    | Assign (v, n) ->
        (match check_expr n g a b d with
          | (Int, g, a, b, d) -> Unit, (Assoc.update v Int g), a, b, d
          | _ -> failwith "assign rule violated")
  )


let check_prog (p : prog) : unit =
  match p with
  | Skip -> ()
  | _ -> (match check_expr p Assoc.empty with
    | Unit, _ -> ()
    | _, _ -> failwith "did not check expression all the way")
