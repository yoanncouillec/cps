open Ast;;
open Printf;;
open Lexer;;

exception NotDeclared of string * string;;

type environment = ((string * env_value) list)

and continuation = environment -> environment

and env_value =
  | EnvUndefined
  | EnvString of string 
  | EnvInteger of int
  | EnvPrimitive of (env_value list -> unit)
  | EnvContinuation of (environment -> environment)
  | EnvLoopContinuation of env_value list

let rec string_of_env = function
    [] -> ""
  | (x,value)::[] -> "(" ^ x ^ "," ^ (string_of_env_value value) ^ ")"
  | (x,value)::rest -> "(" ^ x ^ "," ^ (string_of_env_value value) ^ ");" 
      ^ (string_of_env rest)

and string_of_env_value = function
  | EnvUndefined -> "Undefinded"
  | EnvString s -> s
  | EnvInteger n -> string_of_int n
  | EnvPrimitive p -> "*prim*"
  | EnvContinuation cont -> "*cont*"
  | EnvLoopContinuation l -> "*loop*"

let rec getEnvLastContinuationLoop env = 
  match getEnv env "*loop*" with
    EnvLoopContinuation (hd::tl) -> hd
  | _ -> raise (failwith "getLastContLoop : not found")

and addEnvContinuationLoop env cont =
  try
    match getEnv env "*loop*" with
	EnvLoopContinuation l -> 
	  setEnv env "*loop*" (EnvLoopContinuation (EnvContinuation(cont)::l))
      | _ -> raise (failwith "addEnvContinuationLoop")
  with NotDeclared (s1,s2) ->
    ("*loop*" , (EnvLoopContinuation [EnvContinuation cont])) :: env
	
and removeEnvLastContinuationLoop env =
  match getEnv env "*loop*" with
      EnvLoopContinuation (hd::tl) -> 
	setEnv env "*loop*" (EnvLoopContinuation tl)
    | _ -> env	

and getEnv env x =
  match env with
    | [] -> raise (NotDeclared ("getEnv",x))
    | (x',value)::rest ->
	if (x' = x) then
	  value
	else
	  getEnv rest x

and setEnv env x value =
  match env with
    | [] -> raise (NotDeclared ("setEnv",x))
    | (x',value')::rest ->
	if (x' = x) then
	  ((x,value)::rest)
	else
	  ((x',value')::(setEnv rest x value))

and addVarsEnv env = function
    [] -> env
  | x::rest -> (x,EnvUndefined)::(addVarsEnv env rest)

let rec eval_expression env = function
    | AstVariable v -> begin match getEnv env v with
	  EnvInteger n -> n
	| _ -> raise (failwith ("\"" ^ v ^ "\" is not an integer"))
      end
    | AstEntier e -> e
    | AstAddition (e1,e2) -> (eval_expression env e1) +  (eval_expression env e2)
    | AstMultiplication (e1,e2) -> (eval_expression env e1) * (eval_expression env e2)
    | AstLess (e1,e2) -> if ((eval_expression env e1) < (eval_expression env e2)) then begin 1 end else begin 0 end
    | AstGreat (e1,e2) -> if ((eval_expression env e1) > (eval_expression env e2)) then begin 1 end else begin 0 end
    | AstEqual (e1,e2) -> if ((eval_expression env e1) = (eval_expression env e2)) then begin 1 end else begin 0 end

let eval_value env = function
    AstValueExpression e -> (EnvInteger (eval_expression env e))
  | AstValueString s -> (EnvString s)    

let rec eval_values env = function
    [] -> []
  | value::rest -> (eval_value env value) :: (eval_values env rest)

let rec c env cont = function
    AstAssignment (l,value) -> 
      let v = eval_value env value in
      let rec fp env' = function
	  [] -> (cont env')
	| x :: rest -> fp (setEnv env' x v) rest
      in
	fp env l
  | AstCall (f, args) -> 
      begin
	match getEnv env f with
	    EnvPrimitive (p) -> p (eval_values env args) ; cont env
	  | _ -> raise (failwith "Not a primitive")
      end
  | AstAlternative (e, cs1, cs2) -> 
      (if ((eval_expression env e) = 0) then
	    (cs env cont cs2)
	  else
	    (cs env cont cs1))
  | AstWhile (e, cs1) ->
      let rec k env' = 
	if ((eval_expression env' e) = 0) then
	  (cont env') 
	else 
	  (cs env' k cs1);
      in
	(k (addEnvContinuationLoop env cont))
  | AstCallcc (s,cs1) -> 
      cs ((s,(EnvContinuation cont))::env) cont cs1
  | AstThrow s -> 
      begin 
	match getEnv env s with
	    EnvContinuation cont2 -> (cont2 env)
	  | _ -> raise (failwith "Not a continuation")
      end
  | AstTryWith (cs1,ex,cs2) -> 
      let cont' = fun env -> cs env cont cs2 in
	cs ((ex,(EnvContinuation cont'))::env) cont cs1
  | AstRaise ex -> 
      begin
	match getEnv env ex with
	    EnvContinuation cont' -> cont' env
	  | _ -> raise (failwith "Not a continuation")
      end
  | AstBreak -> 
      begin match getEnvLastContinuationLoop env with
	  EnvContinuation cont -> cont (removeEnvLastContinuationLoop env)
	| _ -> raise (failwith "Need a continuation")
      end
  | AstPlusPlus x -> 
      begin match getEnv env x with
	  EnvInteger n -> cont (setEnv env x (EnvInteger (n + 1)))
	| _ -> raise (failwith "++ : not an integer")
      end
  | AstMinusMinus x -> 
      begin match getEnv env x with
	  EnvInteger n -> cont (setEnv env x (EnvInteger (n - 1)))
	| _ -> raise (failwith "-- : not an integer")
      end
	
and d env = function
  | AstDeclarationVariable l -> addVarsEnv env l

and cs env cont = function
    | [] -> (cont env)
    | AstEmpty::rest -> cs env cont rest
    | AstDeclaration(dec)::rest -> cs (d env dec) cont rest
    | AstCommand(com)::rest -> c env (fun env -> cs env cont rest) com

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let l = 
    try 
      Parser.program Lexer.token lexbuf 
    with Parsing.Parse_error -> (
      print_string "Error at line ";
      print_int !nl;
      print_newline ();
      [];) in

  let print_arg = function
      EnvInteger n -> print_int n
    | EnvString s -> print_string s
    | _ -> raise (failwith "can only print integer or string") in

  let rec print = (fun args -> match args with
		       [] -> print_newline()
		     | arg::[] -> print_arg arg; print_newline()
		     | arg::rest -> print_arg arg; print_string " "; print rest) in
  let r0 = [("print",EnvPrimitive(print));
	    ("nl",EnvPrimitive(fun args -> 
				 match args with 
				     [] -> print_newline() 
				   | _ -> raise (failwith "nl needs 0 argument")))] in
  let env = cs r0 (fun x -> x) l in
    print_string (string_of_env env);
    print_newline();
    flush stdout
;;
