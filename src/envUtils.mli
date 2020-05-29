open ImpAST

(* invalidates
 * paramters are : [variable] [alpha] [delta] *)
val invalidate : string -> int Assoc.context -> string list Assoc.context ->
    int Assoc.context

(* update dependants to dependencies 
 * parameters are : [variable] [expression] [beta] [delta] *)
val declare_beta : string -> expr -> string list Assoc.context -> string list
    Assoc.context

(* update dependencies to dependants 
 * parameters are : [variable] [expression] [beta] [delta] *)
val declare_delta : string -> expr -> string list Assoc.context -> string list
    Assoc.context
