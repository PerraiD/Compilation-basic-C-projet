{
	open Patterns
}

let impr = "Print "
let var_int = ('0'|['1'-'9']['0'-'9']*)
let var_double =  ('0'|['1'-'9']['0'-'9']*)('.'['0'-'9']+)?
let chaine = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '.']*

rule basic = parse
	impr as ip {PRINT ip}
	| ';' {SEMICOLON}
	| '\"' {DOUBLEQUOTE}
	| var_int as i {VAR_INT i}
	| var_double as d {VAR_DOUBLE d}
	| chaine as ch {CHAINE ch}

	| ' ' | '\t' | '\n' {basic lexbuf}
	| _ as c {CHAR c}
	| eof {EOF}


