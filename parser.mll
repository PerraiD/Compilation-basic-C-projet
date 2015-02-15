{
	open Patterns
}

let impr = "Print "
let var_int = ('0'|['1'-'9']['0'-'9']*)
let var_double =  ('0'|['1'-'9']['0'-'9']*)('.'['0'-'9']+)?
let chaine = ['a'-'z' 'A'-'Z' '0'-'9' '_' '?' '!' ':' ',' '.' '%']*
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let b_type = "String" | "Double" | "Integer" | "Single" | "Byte" | "Short" | "LongInt" | "UByte" | "UShort" | "UInteger" | "ULongInt" | "Integer Ptr" | "Byte Ptr" | "ZString Ptr"

rule basic = parse
	impr as ip {PRINT ip}
	| ';' {SEMICOLON}
	| ',' {COMMA}
	| '\"' {DOUBLEQUOTE}
	| '\\' {BS}
	| '(' {LPARENTHESE}
	| ')' {RPARENTHESE}
	
	| '+' {PLUS}
	| '-' {MINUS}
	| '*' {TIMES}
	| '/' {OBELUS}
	
	| var_int as i {INT i}
	| var_double as d {DOUBLE d}
	| chaine as ch {CHAINE ch}
	
	| ident as id {IDENT id}

	| ' ' | '\t' | '\n' {basic lexbuf}
	| _ as c {CHAR c}
	| eof {EOF}


