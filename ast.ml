type expression =
  | AstVariable of string
  | AstEntier of int
  | AstAddition of expression * expression
  | AstMultiplication of expression * expression
  | AstLess of expression * expression
  | AstGreat of expression * expression
  | AstEqual of expression * expression

type value = 
    AstValueExpression of expression
  | AstValueString of string

type instruction = 
  | AstEmpty
  | AstDeclaration of declaration
  | AstCommand of command

and declaration = 
  | AstDeclarationVariable of string list

and command = 
  | AstAssignment of ( string list ) * value
  | AstCall of string * ( value list )
  | AstAlternative of expression * instruction list * instruction list
  | AstWhile of expression * instruction list
  | AstCallcc of string * instruction list
  | AstThrow of string
  | AstTryWith of instruction list * string * instruction list
  | AstRaise of string
  | AstBreak
  | AstPlusPlus of string
  | AstMinusMinus of string
