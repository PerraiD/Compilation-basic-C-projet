{
	open Patterns

	open Lexing
	type arbuste_exception = string * Lexing.position
	exception ArbusteError of arbuste_exception
	let error message pos = raise (ArbusteError (message, pos))
	let print (m,p) =
	Printf.eprintf "Error line %d character %d: %s\n" p.pos_lnum (p.pos_bol + 1) m
}

let impr = "Print "
let const = "Const"
let var_int = ('0'|['1'-'9']['0'-'9']*)
let var_double =  ('0'|['1'-'9']['0'-'9']*)('.'['0'-'9']+)?
let var_string = ['a'-'z' 'A'-'Z' '0'-'9' '_' '?' '!' ':' ',' '.' '%']*
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let b_type = "String" | "Double" | "Integer" | "Single" | "Byte" | "Short" | "LongInt" | "UByte" | "UShort" | "UInteger" | "ULongInt" | "Integer Ptr" | "Byte Ptr" | "ZString Ptr"

rule basic = parse
	impr as ip {PRINT ip}
	| ';' {SEMICOLON}
	| ',' {COMMA}
	| '\"' {DOUBLEQUOTE}
	| '\\' {BS}
	| '(' {LPAREN}
	| ')' {RPAREN}
	
	| '+' {PLUS}
	| '-' {MINUS}
	| '*' {MUL}
	| '/' {DIV}
    
    | const {CONST}

	| var_int as i {VAR_INT i}
	| var_double as d {VAR_DOUBLE d}
	| var_string as ch {VAR_STRING ch}
	
	| ident as id {IDENT id}

	| ' ' | '\t' | '\n' {basic lexbuf}
	| _ as c {CHAR c}
	| eof {EOF}


