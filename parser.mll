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
	
	| "<" {LT}
	| ">" {GT}
	| "=" {EQ}
	| "!=" {NE}
	| "<=" {LE}
	| ">=" {GE}

	| "If" {IF}
	| "Then" {THEN}
	| "End If" {ENDIF}

	| var_int as i {INT i}
	| var_double as d {DOUBLE d}
	| var_sub_string as sstr {SUB_STRING sstr}
	| var_string as str {STRING str}
	
	| "Dim" {DIM}
	| "As" {AS}
	| const {CONST}
	| type_string as str {TYPE_STRING str}
	| type_double as dbl {TYPE_DOUBLE dbl}
	| type_integer as itg {TYPE_INT itg}
	| type_single as sng {TYPE_CHAR sng}
	
	| ident as id {IDENT id}
	
	| '\n' {EOL}

	| ' ' | '\t' | '\n' {basic lexbuf}
	| _ as c {CHAR c}
	| eof {EOF}

