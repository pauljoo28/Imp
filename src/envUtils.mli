open ImpAST

(* invalidates
 * paramters are : [variable] [alpha] [delta] *)
val invalidate : string -> int Assoc.context -> string list Assoc.context ->
    int Assoc.context

(* validates
 * paramters are : [variable] [alpha] [beta] *)
val update : string -> int Assoc.context -> string list Assoc.context ->
    int Assoc.context

(* update dependants to dependencies 
 * parameters are : [variable] [expression] [beta] *)
val declare_beta : string -> expr -> string list Assoc.context -> string list
    Assoc.context

(* update dependencies to dependants 
 * parameters are : [variable] [expression] [delta] *)
val declare_delta : string -> expr -> string list Assoc.context -> string list
    Assoc.context

(* finds the intersection between two alpha contexts
 * parameters are : [first alpha] [second alpha] *)
val intersect : int Assoc.context -> int Assoc.context -> int Assoc.context

