{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let string = '"'[^'"']*'"'
let comment = "#"[^'\n']+'\n'

(* Need to figure out how to lex different types into their equivalent arith_type value *)

rule read =
  parse
  | white { read lexbuf }
  | "True" { TRUE }
  | "False" { FALSE }
  | "!" { NOT }
  | "*" { TIMES }
  | "+" { PLUS }
  | "^" { CONCAT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | ":" { COLON }
  | ";" { SEMICOLON }
  | "," { COMMA }
  | "." { PERIOD }
  | "->" { ARROW }
  | "#" { COMMENT }
  | "Let" { LET }
  | "=" { EQUALS }
  | "In" { IN }
  | "If" { IF }
  | "Then" { THEN }
  | "Else" { ELSE }
  | "First" { FIRST }
  | "Second" { SECOND }
  | "Fun" { FUN }
  | "Fix" { FIX }
  | "Input()" { INPUT }
  | "Print" { PRINT }
  | "Rand" { RAND }
  | "int" { TYPE_INT }
  | "bool" { TYPE_BOOL }
  | "fun" { TYPE_FUN }
  | "string" { TYPE_STRING }
  | id { ID (Lexing.lexeme lexbuf) }
  | string { STRING (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | comment { COMMENT }
  | eof { EOF }