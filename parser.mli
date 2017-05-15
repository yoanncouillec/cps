type token =
  | LPAREN
  | RPAREN
  | PLUS
  | TIMES
  | EQUAL
  | SEMICOL
  | VAR
  | COMMA
  | IF
  | THEN
  | ELSE
  | LBRACKET
  | RBRACKET
  | WHILE
  | LESS
  | THROW
  | CALLCC
  | GREAT
  | COLONEQUAL
  | END
  | TRY
  | WITH
  | RAISE
  | BREAK
  | PLUSPLUS
  | MINUSMINUS
  | INTEGER of (int)
  | IDENT of (string)
  | STRING of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.instruction list
