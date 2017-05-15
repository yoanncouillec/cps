{
  open Parser
  let nl = ref 1;;
  let string_chars s = String.sub s 1 ((String.length s)-2) ;;
}
rule token = parse
  | [' ' '\t'] { token lexbuf }
  | "//" [^ '\n']* { token lexbuf }
  | '\n' { nl := !nl + 1; token lexbuf  }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACKET }
  | "}" { RBRACKET }
  | "+" { PLUS }
  | "*" { TIMES }
  | "var" { VAR }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "callcc" { CALLCC }
  | "throw" { THROW }
  | "try" { TRY }
  | "with" { WITH }
  | "raise" { RAISE }
  | "end" { END }
  | "break" { BREAK }
  | ":=" { COLONEQUAL }
  | "<" { LESS }
  | ">" { GREAT }
  | "=" { EQUAL }
  | "++" { PLUSPLUS }  
  | "--" { MINUSMINUS }  
  | ";" { SEMICOL }
  | "," { COMMA }
  | '"' [^ '"']* '"' {STRING (string_chars (Lexing.lexeme lexbuf)) }
  | ['a'-'z']+ { IDENT (Lexing.lexeme lexbuf) }
  | ['0'-'9']+ { INTEGER (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
