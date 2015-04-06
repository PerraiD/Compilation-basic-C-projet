%{	
	open Arbre

	let x = {struct_fonc= []; struct_instr=[]; struct_import=[]}
	let add_fonc a = x.struct_fonc <- x.struct_fonc@a
	let add_import a = x.struct_import <-x.struct_import@a
	let add_instr a = x.struct_instr <-x.struct_instr@a

	let rec ajout_fin(l,e) = match l with
		| [] -> [e]
		| p:: r -> p::ajout_fin(r,e);;	
%}

%token EOF
%token EOL

/*(*var type*)*/
%token <char> CHAR
%token <string> PRINT
%token <string> INT
%token <string> DOUBLE
%token <string> STRING
%token <string> SUB_STRING
%token BS

%left BS

/*(* CONDITION *)*/
%token ACOLLEFT
%token ACOLRIGHT

/*(* declaration *)*/
%token <string> INCLUDE
%token DIM
%token AS
%token CONST
%token <string> IDENT
%token TYPE_INT
%token TYPE_DOUBLE
%token TYPE_STRING
%token TYPE_CHAR

/*(*Fonction*)*/
%token FUNCTION
%token <string> FUNC_NAME
%token END_FUNC
%token SUB
%token END_SUB

/*(*structure conditionnelle*)*/
%token IF
%token THEN
%token ELSE
%token ELSEIF
%token ENDIF
%token WHILE
%token WEND

%token DO
%token UNTIL
%token LOOP
%token NEXT

%token FOR
%token STEP
%token TO

/*(* maths *)*/
%token PLUS
%token MINUS
%token MUL
%token DIV

%token LT
%token GT
%token EQ
%token NE 
%token LE
%token GE

%token TRUE
%token FALSE

%left LT GT EQ NE LE GE
%left PLUS MINUS
%left MUL DIV

/*(* signes *)*/
%token SEMICOLON
%token COMMA
%token DOUBLEQUOTE
%token LPAREN
%token RPAREN

%start main
%type <Arbre.t_prog> main

%%

main:
	| prog EOF {set_prog_list x.struct_instr x.struct_fonc x.struct_import}
;

prog:
	| import prog  {add_import $1;set_prog_list x.struct_instr x.struct_fonc x.struct_import}
	| functions prog {add_fonc $1;set_prog_list x.struct_instr x.struct_fonc x.struct_import}
	| instr prog {add_instr $1;set_prog_list x.struct_instr  x.struct_fonc x.struct_import}
	| {set_prog_list x.struct_instr x.struct_fonc x.struct_import}
;

import :
	| INCLUDE {[Include($1)]}
;

instr:	
	| IF condition THEN instr else_block ENDIF instr {If($2)::Then::($4@$5@EndIf::$7)}	
	
	| DO WHILE condition instr LOOP instr {Do::($4@Loop::DoWhile($3)::$6)}
	| DO UNTIL condition instr LOOP instr {Do::($4@Loop::Until($3)::$6)}
	
	| DO instr LOOP WHILE condition instr {Do::($2@Loop::DoWhile($5)::$6)}
	| DO instr LOOP UNTIL condition instr {Do::($2@Loop::Until($5)::$6)}
	
	| WHILE condition instr WEND instr {While($2)::($3@Wend::$5)}
	
	/*(* il peut ne pas y avoir de untilORwhile (voir doc. Do...Loop), à voir si on implémente ou pas (pas sûr que possible en C) *)*/

	| FOR IDENT EQ var_val TO borne_condition STEP math_signe var_val instr NEXT IDENT instr {For(Ident $2,Empty,Empty,Equal,$4,To,$6,Step,$8,$9)::($10@Next::$13)}   
	| FOR IDENT AS types EQ var_val TO borne_condition STEP math_signe var_val instr NEXT IDENT instr {For(Ident $2,As,$4,Equal,$6,To,$8,Step,$10,$11)::($12@Next::$15)}
	
	| PRINT IDENT instr {Print($2)::$3}
	
	| EOL instr {Empty::$2}
	| {[Empty]}
;

else_block :
	| ELSEIF condition THEN instr else_block {ElseIf($2)::Then::($4@$5)}
	| ELSE instr {Else::$2}
	| {[Empty]}
;

functions :
	| SUB FUNC_NAME EOL fonc_instr {Function($2,"void")::$4}
	| END_SUB {[Empty]}
	| END_FUNC {[Empty]}
;

fonc_instr:
	| PRINT IDENT EOL fonc_instr {PrintFonc($2)::$4}
	| {[Empty]}
	
operateur:
	|EQ {Equal}
	|LT {Lesser}
	|GT {Greater}
	|NE {Notequal} 
	|LE {Lessequal} 
	|GE {Greaterequal} 
;

condition:
	|TRUE {Conditionnelle(True,Empty,Empty)}
	|FALSE {Conditionnelle(False,Empty,Empty)}
	|borne_condition operateur borne_condition {Conditionnelle($1,$2,$3)}
	|borne_condition {Conditionnelle($1,Empty,Empty)}
;


math_signe:
	|PLUS{Minus}
	|MINUS{Plus}


borne_condition:
	|IDENT {Ident $1}
	|INT {Integer $1}
	|DOUBLE {Double $1}


var_val:
	|INT {Integer $1}
	|DOUBLE {Double $1}

types:
	TYPE_INT {Tint "int "}
	| TYPE_DOUBLE {Tdouble "double "}
	| TYPE_STRING {Tstring "char* "}
	| TYPE_CHAR {Tchar "char "}
;
