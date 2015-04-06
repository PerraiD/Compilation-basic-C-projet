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

let func_name = ['a'-'z' 'A'-'Z']* "()"
let inclu = "#include [\' <] ['a'-'z' 'A'-'Z' '_' '.']* [\' >]"
let var_int = ('0'|['1'-'9']['0'-'9']*)
let var_double =  ('0'|['1'-'9']['0'-'9']*)('.'['0'-'9']+)?
let var_string = '\"'['a'-'z' 'A'-'Z' '0'-'9' '_' '?' '!' ':' ',' '.' '%']*'\"'
let var_sub_string = ('\"'['a'-'z' 'A'-'Z' '0'-'9' '_' '?' '!' ':' ',' '.' '%']* (['a'-'z' 'A'-'Z' '0'-'9' '_' '?' '!' ':' ',' '.' '%']*'\\')* ['a'-'z' 'A'-'Z' '0'-'9' '_' '?' '!' ':' ',' '.' '%']*'\"')
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let b_type =  "Byte" | "Short" | "LongInt" | "UByte" | "UShort" | "UInteger" | "ULongInt" | "Integer Ptr" | "Byte Ptr" | "ZString Ptr"
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
	| '{' {ACOLLEFT}
	| '}' {ACOLRIGHT}
	| '=' {EQ}
	
	| "<" {LT}
	| ">" {GT}
	| "=" {EQ}
	| "!=" {NE}
	| "<=" {LE}
	| ">=" {GE}

	| "If" {IF}
	| "Then" {THEN}
	| "Else" {ELSE}
	| "ElseIf" {ELSEIF}
	| "End If" {ENDIF}
	| "Function" {FUNCTION}
	| "End Function" {END_FUNC}
	| "Sub" {SUB}
	| "End Sub" {END_SUB}
	| "While" {WHILE}
	| "Wend" {WEND}

	| "For" {FOR}
	| "To" {TO}
	| "Step" {STEP}
	
	| "Until" {UNTIL}
	| "Do" {DO}
	| "Loop" {LOOP}
	| "Next" {NEXT}

	| func_name as fn {FUNC_NAME fn}
	| inclu as i {INCLUDE i}


	| var_int as i {INT i}
	| var_double as d {DOUBLE d}
	| var_sub_string as sstr {SUB_STRING sstr}
	| var_string as str {STRING str}
	
	| "Dim" {DIM}
	| "As" {AS}
	| const {CONST}
	
	| "True"{TRUE}
	| "False"{FALSE}

	| type_string {TYPE_STRING}
	| type_double {TYPE_DOUBLE}
	| type_integer  {TYPE_INT}
	| type_single {TYPE_CHAR}
	

	| ident as id {IDENT id}
	
	| '\n' {EOL}

	| ' ' | '\t' | '\n' {basic lexbuf}
	| _ as c {CHAR c}
	| eof {EOF}

