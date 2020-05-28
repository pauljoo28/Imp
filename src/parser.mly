
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
%token WHILE
%token UPDATE
%token DECLARE

(* Precedences *)
%left SEQ
%left PRINT

%left ASSIGN

%left AND OR

%left EQUAL LEQ
%left PLUS MINUS
%left MULT

%left NOT

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
  | c = expr; EOF;
    { c }

expr:
  | c1 = expr; SEQ; c2 = expr;
    { Seq (c1, c2) }
  | c = expr; SEQ;
    { ESeq c }
  | LPAREN; c = expr; RPAREN;
    { Paren c }
  | SKIP;
    { Skip }
  | PRINT; a = expr;
    { Print(a) }
  | IF; b = expr; THEN; c1 = expr; END; ELSE; THEN; c2 = expr; END;
    { If (b, c1, c2) }
  | v = ID; ASSIGN; a = expr;
    { Assign (v, a) }
  | WHILE; b = expr; THEN; c = expr; END;
    { While (b, c) }
  | TRUE;
    { True }
  | FALSE;
    { False }
  | n1 = expr; EQUAL; n2 = expr;
   { Equal (n1, n2) }
  | n1 = expr; LEQ; n2 = expr;
   { Leq (n1, n2) }
  | NOT; b = expr;
   { Not (b) }
  | b1 = expr; OR; b2 = expr;
    { Or (b1, b2) }
  | b1 = expr; AND; b2 = expr;
    { And (b1, b2) }
  | n = NUM;
    { Num(n) }
  | v = ID;
    { Var v }
  | n1 = expr; PLUS; n2 = expr;
     { Plus (n1, n2) }
  | n1 = expr; MINUS; n2 = expr;
     { Minus (n1, n2) }
  | n1 = expr; MULT; n2 = expr;
     { Mult (n1, n2) }
  | v = ID; DECLARE; a = expr;
    { Declare (v, a) }
  | UPDATE; v = ID;
    { Update v }

%%
