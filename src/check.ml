open ImpAST

type gamma = typ Assoc.context

let rec check_expr (e : expr) (g : gamma) : (typ * gamma) =
  (match e with
    | Num n -> Int, g
    | Var v -> Assoc.lookup v g, g
    | True -> Bool, g
    | False -> Bool, g
    | Skip -> Unit, g
    | Plus (a1, a2) ->
        (match check_expr a1 g, check_expr a2 g with
          | (Int, _), (Int, _) -> Int, g
          | _, _ -> failwith "plus rule violated")
    | Minus (a1, a2) ->
        (match check_expr a1 g, check_expr a2 g with
          | (Int, _), (Int, _) -> Int, g
          | _, _ -> failwith "minus rule violated")
    | Mult (a1, a2) ->
        (match check_expr a1 g, check_expr a2 g with
          | (Int, _), (Int, _) -> Int, g
          | _, _ -> failwith "mult rule violated")
    | Equal (a1, a2) ->
        (match check_expr a1 g, check_expr a2 g with
          | (Int, _), (Int, _) -> Bool, g
          | _, _ -> failwith "equal rule violated")
    | Leq (a1, a2) ->
        (match check_expr a1 g, check_expr a2 g with
          | (Int, _), (Int, _) -> Bool, g
          | _, _ -> failwith "leq rule violated")
    | And (b1, b2) ->
        (match check_expr b1 g, check_expr b2 g with
          | (Bool, _), (Bool, _) -> Bool, g
          | _, _ -> failwith "and rule violated")
    | Or (b1, b2) ->
        (match check_expr b1 g, check_expr b2 g with
          | (Bool, _), (Bool, _) -> Bool, g
          | _, _ -> failwith "or rule violated")
    | Not b1 ->
        (match check_expr b1 g with
          | Bool, _ -> Bool, g
          | _ -> failwith "not rule violated")
    | Print (com) ->
        (match check_expr com g with
          | Bool, _ -> Unit, g
          | Int, _ -> Unit, g
          | _ -> failwith "print rule violated")
    | Seq (c1, c2) ->
        (match check_expr c1 g with
          | (Unit, g') -> 
              (match check_expr c2 g' with
                | (Unit, g'') -> Unit, g''
                | _ -> failwith "Seq rule violated")
          | _ -> failwith "Seq rule violated")
    | ESeq c1 ->
        (match check_expr c1 g with
          | Unit, g' -> Unit, g'
          | _ -> failwith "ESeq rule violated")
    | Paren c ->
        (match check_expr c g with
          | Int, g' -> Int, g'
          | Bool, g' -> Bool, g'
          | Unit, g' -> Unit, g')
    | If (b, c1, c2) ->
        (match check_expr b g, check_expr c1 g, check_expr c2 g with
          | (Bool,_), (Unit,_), (Unit,_) -> Unit, g
          | _ -> failwith "If statement rule violated")
    | While (b, c) ->
        (match check_expr b g, check_expr c g with
          | (Bool, _), (Unit, _) -> Unit, g
          | _ -> failwith "while statement rule violated")
    | Assign (v, n) ->
        (match check_expr n g with
          | (Int, g) -> Unit, (Assoc.update v Int g)
          | _ -> failwith "assign rule violated")
  )


let check_prog (p : prog) : unit =
  match p with
  | Skip -> ()
  | _ -> (match check_expr p Assoc.empty with
    | Unit, _ -> ()
    | _, _ -> failwith "did not check expression all the way")
