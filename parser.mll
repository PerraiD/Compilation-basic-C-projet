{
	open Patterns
}

let impr = "Print"
let var_int = ('0'|['1'-'9']['0'-'9']*)
let var_double =  ('0'|['1'-'9']['0'-'9']*)('.'['0'-'9']+)?
let var_string = '\"'['a'-'z' 'A'-'Z' '0'-'9' '_' '?' '!' ':' ',' '.' '%']*'\"'
let var_sub_string = ('\"'['a'-'z' 'A'-'Z' '0'-'9' '_' '?' '!' ':' ',' '.' '%']* (['a'-'z' 'A'-'Z' '0'-'9' '_' '?' '!' ':' ',' '.' '%']*'\\')* ['a'-'z' 'A'-'Z' '0'-'9' '_' '?' '!' ':' ',' '.' '%']*'\"')
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let b_type = "String" | "Double" | "Integer" | "Single" | "Byte" | "Short" | "LongInt" | "UByte" | "UShort" | "UInteger" | "ULongInt" | "Integer Ptr" | "Byte Ptr" | "ZString Ptr"
let type_string = "String"
let type_double = "Double"
let type_integer = "Integer"
let type_single = "Single"

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
	
	| var_int as i {INT i}
	| var_double as d {DOUBLE d}
	| var_sub_string as sstr {SUB_STRING sstr}
	| var_string as str {STRING str}
	
	| "Dim" {DIM}
	| "As" {AS}
	| type_string as str {TYPE_STRING str}
	| type_double as dbl {TYPE_DOUBLE dbl}
	| type_integer as itg {TYPE_INT itg}
	| type_single as sng {TYPE_CHAR sng}
	
	| ident as id {IDENT id}
	
	| '\n' {EOL}

	| ' ' | '\t' | '\n' {basic lexbuf}
	| _ as c {CHAR c}
	| eof {EOF}



