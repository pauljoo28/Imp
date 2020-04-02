
%{
open ImpAST
open Str

exception ParseException of string
%}

(* Tokens *)

%token EOF
%token <int> NUM
%token <string> ID
%token SKIP
%token TRUE
%token FALSE
%token PRINT
%token MULT
%token PLUS
%token MINUS
%token EQUAL
%token NOT
%token LEQ
%token OR
%token AND
%token SEQ
%token LPAREN
%token RPAREN
%token IF
%token ELSE
%token THEN
%token END
%token ASSIGN

(* Precedences *)
%left PLUS MINUS
%left MULT

(* After declaring associativity and precedence, we need to declare what
   the starting point is for parsing the language.  The following
   declaration says to start with a rule (defined below) named [prog].
   The declaration also says that parsing a [prog] will return an OCaml
   value of type [ImpAST.prog]. *)

%start main
%type <ImpAST.prog> main

(* The following %% ends the declarations section of the grammar definition. *)

%%

main:
  | c = com; EOF;
    { c }

com:
  | SKIP;
    { Skip }
  | PRINT; a = aexp;
    { APrint(a) }
  | PRINT; b = bexp;
    { BPrint(b) }
  | c1 = com; SEQ; c2 = com;
    { Seq (c1, c2) }
  | c = com; SEQ;
    { ESeq c }
  | LPAREN; c = com; RPAREN;
    { CParen c }
  | IF; b = bexp; THEN; c1 = com; ELSE; c2 = com; END;
    { If (b, c1, c2) }
  | v = ID; ASSIGN; a = aexp;
    { Assign (v, a) }

bexp:
  | TRUE;
    { True }
  | FALSE;
    { False }
  | n1 = aexp; EQUAL; n2 = aexp;
   { Equal (n1, n2) }
  | n1 = aexp; LEQ; n2 = aexp;
   { Leq (n1, n2) }
  | NOT; b = bexp;
   { Not (b) }
 | b1 = bexp; OR; b2 = bexp;
   { Or (b1, b2) }
 | b1 = bexp; AND; b2 = bexp;
   { And (b1, b2) }
  | LPAREN; b = bexp; RPAREN;
    { BParen b }

aexp:
  | n = NUM;
    { Num(n) }
  | v = ID;
    { Var v }
  | n1 = aexp; PLUS; n2 = aexp;
     { Plus (n1, n2) }
  | n1 = aexp; MINUS; n2 = aexp;
     { Minus (n1, n2) }
  | n1 = aexp; MULT; n2 = aexp;
     { Mult (n1, n2) }
  | LPAREN; a = aexp; RPAREN;
    { AParen a }

%%
