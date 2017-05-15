%{
  open Ast;;
%}

%token LPAREN RPAREN PLUS TIMES EQUAL SEMICOL VAR COMMA IF 
%token THEN ELSE LBRACKET RBRACKET WHILE LESS THROW CALLCC 
%token GREAT COLONEQUAL END TRY WITH RAISE BREAK PLUSPLUS MINUSMINUS
%token<int> INTEGER
%token<string> IDENT STRING
%token EOF
%left PLUS
%left TIMES
%left LESS
%left GREAT
%left EQUAL
%start program
%type <Ast.instruction list> program
%%

program:
 instructions EOF { $1 }

instructions:
| instruction { [$1] }
| instruction instructions { $1::$2 }

instruction:
| SEMICOL { AstEmpty }
| declaration { AstDeclaration($1) }
| command { AstCommand($1) }

command:
| assignment SEMICOL { $1 }
| call SEMICOL { $1 }
| alternative { $1 }
| loop_while { $1 }
| CALLCC IDENT LBRACKET instructions RBRACKET { AstCallcc($2,$4) }
| THROW IDENT SEMICOL { AstThrow($2) }
| TRY if_instructions WITH IDENT if_instructions { AstTryWith($2,$4,$5) }
| RAISE IDENT SEMICOL { AstRaise($2) }
| BREAK { AstBreak }
| IDENT PLUSPLUS { AstPlusPlus $1 }
| IDENT MINUSMINUS { AstMinusMinus $1 }

declaration:
| declaration_var SEMICOL { $1 }

declaration_var:
| VAR variables { AstDeclarationVariable($2) }

variables:
| variable { [$1] }
| variable COMMA variables { $1::$3 }

assignment:
| variables COLONEQUAL value { AstAssignment($1,$3) }

value:
| expression { AstValueExpression($1) }
| STRING { AstValueString($1) }

call:
| IDENT { AstCall($1,[]) }
| IDENT arguments { AstCall($1,$2) }

arguments:
| STRING { [AstValueString $1] }
| expression  { [AstValueExpression $1] }
| expression arguments { (AstValueExpression $1)::$2 }

alternative:
| IF expression if_instructions ELSE if_instructions { AstAlternative($2,$3,$5) }
| IF expression if_instructions { AstAlternative($2,$3,[]) }

if_instructions:
| LBRACKET instructions RBRACKET { $2 }

loop_while:
| WHILE expression LBRACKET instructions RBRACKET { AstWhile($2,$4) }


variable:
| IDENT { $1 }

expression:
| IDENT { AstVariable($1) }
| INTEGER { AstEntier($1) }
| expression PLUS expression { AstAddition ($1, $3)  }
| expression TIMES expression { AstMultiplication ($1, $3)  }
| expression LESS expression { AstLess ($1, $3) }
| expression GREAT expression { AstGreat ($1, $3) }
| expression EQUAL expression { AstEqual ($1, $3) }
| LPAREN expression RPAREN { $2 }
;
