{
	open Patterns
	open Lexing

	(** Increments the lexing buffer line number counter.*)
	let incr_line lexbuf =
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <-
			{pos with pos_lnum = pos.pos_lnum + 1; pos_bol = 0}
}


let impr = "Print "
let const = "Const"

let com = '\'' ['a'-'z' 'A'-'Z' '0'-'9' '_' '?' '!' ':' ',' '.' '%' ' ' '\'']* '\n'
let coms = "/\'" ['a'-'z' 'A'-'Z' '0'-'9' '_' '?' '!' ':' ',' '.' '%' ' ' '\'' '\n']* "\'/"
let inclu = "#include" ['\'' '<'] ['a'-'z' 'A'-'Z' '_' '.']* ['\'' '>']
let var_int = ('0'|['1'-'9']['0'-'9']*)
let var_double =  ('0'|['1'-'9']['0'-'9']*)('.'['0'-'9']+)?
let var_string = '\"'['a'-'z' 'A'-'Z' '0'-'9' '_' '?' '!' ':' ',' '.' '%' ' ' '\'']*'\"'
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let type_string = "String"
let type_double = "Double"
let type_integer = "Integer"
let type_single = "Single"

rule basic = parse
	impr as ip {PRINT ip}
	| ';' {SEMICOLON}
	| ':' {COLON}
	| ',' {COMMA}
	| '\\' {BS}
	| '(' {LPAREN}
	| ')' {RPAREN}
	
	| '+' {PLUS}
	| '-' {MINUS}
	| '*' {MUL}
	| '/' {DIV}
	| '{' {ACOLLEFT}
	| '}' {ACOLRIGHT}
	| '=' {AFFECT}
	
	| "<" {LT}
	| ">" {GT}
	| "==" {EQ}
	| "!=" {NE}
	| "<=" {LE}
	| ">=" {GE}

	| "If" {IF}
	| "Then" {THEN}
	| "Else" {ELSE}
	| "ElseIf" {ELSEIF}
	| "End If" {ENDIF}
	
	| "Declare" {DECLARE}
	| "Function" {FUNCTION}
	| "Return" {RETURN}
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

	| com as c {LCOM c}
	| coms as c {MCOM c}
	
	| inclu as i {INCLUDE i}

	| var_int as i {INT i}
	| var_double as d {DOUBLE d}
	| var_string as str {STRING str}
	
	| "Dim" {DIM}
	| "As" {AS}
	| const {CONST}
	
	| "True" {TRUE}
	| "False" {FALSE}

	| type_string {TYPE_STRING}
	| type_double {TYPE_DOUBLE}
	| type_integer  {TYPE_INT}
	| type_single {TYPE_CHAR}
	
	| ident as id {IDENT id}
	
	| '\n' {incr_line lexbuf; basic lexbuf}
	| ' ' | '\t' | '\r' {basic lexbuf}
	| _ as c {CHAR c}
	| eof {EOF}
