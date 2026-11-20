%{
open Lang
%}

%token <int> INT
%token <string> ID
%token <string> STRING
%token TYPE_INT TYPE_BOOL TYPE_FUN TYPE_STRING
%token TRUE FALSE
%token NOT EQUALS
%token TIMES PLUS
%token LPAREN RPAREN
%token COLON SEMICOLON COMMA PERIOD ARROW
%token LET IN
%token IF THEN ELSE
%token FIRST SECOND
%token FUN FIX
%token INPUT PRINT
%token CONCAT
%token RAND
%token COMMENT
%token EOF


%nonassoc IN
%nonassoc ELSE
%nonassoc ARROW
%right FIRST
%right SECOND
%right NOT
%left SEMICOLON
%left EQUALS
%left PLUS
%left TIMES
%left CONCAT
%left LPAREN
%left RPAREN
%left COMMENT


%start <Lang.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

arith_type:
  | TYPE_INT { INT }
  | TYPE_BOOL { BOOL }
  | TYPE_STRING { STRING }
  | LPAREN; t1 = arith_type; COMMA t2 = arith_type; RPAREN { PAIR(t1,t2) }
  | TYPE_FUN; LPAREN; t1 = arith_type; ARROW; t2 = arith_type; RPAREN { FUN(t1,t2) }
  ;

expr:
  | i    = INT    { Int i }
  | x    = ID     { Var x }
  | s    = STRING { String(String.sub s 1 ((String.length s) - 2)) }
  | TRUE          { Bool true }
  | FALSE         { Bool false }
  | LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN { Pair(e1, e2) }
  | NOT; e = expr { Not(e) }
  | e1 = expr; EQUALS; e2 = expr { Equal(e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Mult(e1, e2) }
  | e1 = expr; PLUS; e2 = expr { Plus(e1, e2) }
  | e1 = expr; CONCAT; e2 = expr { Concat(e1, e2) }
  | FIRST; e = expr { First(e) }
  | SECOND; e = expr { Second(e) }
  | RAND; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN { Rand(e1, e2) }
  | LET; x = ID; COLON; t = arith_type; EQUALS; e1 = expr; IN; e2 = expr { Let (t, x, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | FUN; t1 = arith_type; x = ID; COLON; t2 = arith_type; ARROW; e = expr { Fun(t1, t2, x, e) }
  | FIX; t1 = arith_type; f = ID; PERIOD; x = ID; COLON; t2 = arith_type; ARROW; e = expr { Fix(t1, t2, f, x, e) }
  | INPUT { Input } (* Input() *)
  | COMMENT; e = expr { e } | e = expr; COMMENT { e } (* Comment *)
  | PRINT; LPAREN; e = expr; RPAREN { Print(e) } (* Print() *)
  | LPAREN; t = arith_type; RPAREN; e = expr { Cast(t, e) } (* Casting *)
  | e1 = expr; LPAREN; e2 = expr; RPAREN { App(e1, e2) } (* App() *)
  | LPAREN; e = expr; RPAREN { e } (* Containing an expression *)
  | e1 = expr; SEMICOLON; e2 = expr { Break(e1, e2) } (* Breaking an expression *)
  ;