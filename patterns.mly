%{	
	open Arbre

	let x = {struct_fonc= []; struct_instr=[]; struct_import=[]}
	let add_fonc a = x.struct_fonc <- x.struct_fonc@a
	let add_import a = x.struct_import <- x.struct_import@a
	let add_instr a = x.struct_instr <- x.struct_instr@a

	let rec ajout_fin(l,e) = match l with
		| [] -> [e]
		| p:: r -> p::ajout_fin(r,e);;	
%}

%token EOF

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
%token AFFECT

/*(*Fonction*)*/
%token DECLARE
%token FUNCTION
%token RETURN
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
%token COLON
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
	| prog import {add_import $2;set_prog_list x.struct_instr x.struct_fonc x.struct_import}
	| prog fonctions {add_fonc $2;set_prog_list x.struct_instr x.struct_fonc x.struct_import}
	| prog contenu {add_instr $2;set_prog_list x.struct_instr  x.struct_fonc x.struct_import}
	| {set_prog_list x.struct_instr x.struct_fonc x.struct_import}
;

import :
	| INCLUDE {[Include($1)]}
;

instr :	
	| instr DIM AS types enum_identMult {$1@(DimMult($5, $4)::[Empty])}
	| instr DIM enum_ident {$1@$3}
	| DIM AS types enum_identMult {DimMult($4, $3)::[Empty]}
	| DIM enum_ident {$2@[Empty]}
	
	| instr IDENT AFFECT operation multAffect {$1@(Affect($2, $4)::$5)}
	| IDENT AFFECT operation multAffect {Affect($1, $3)::($4@[Empty])}
	
	| instr IF condition THEN contenu else_block ENDIF {$1@(If($3)::Then::($5@$6@[EndIf]))}
	| IF condition THEN contenu else_block ENDIF {If($2)::Then::($4@$5@EndIf::[Empty])}
	/*
	| instr DO WHILE condition contenu LOOP {$1@(Do::($5@Loop::DoWhile($4)::[Empty]))}
	| instr DO UNTIL condition contenu LOOP {$1@(Do::($4@Loop::DoWhile($3)::[Empty]))}
	*/
	| instr DO contenu LOOP WHILE condition {$1@(Do::($3@Loop::DoWhile($6)::[Empty]))}
	| instr DO contenu LOOP UNTIL condition {$1@(Do::($3@Loop::DoWhile($6)::[Empty]))}
	| DO contenu LOOP WHILE condition {Do::($2@Loop::DoWhile($5)::[Empty])}
	| DO contenu LOOP UNTIL condition {Do::($2@Loop::DoWhile($5)::[Empty])}
	
	| instr WHILE condition contenu WEND {$1@(While($3)::($4@[Wend]))}
	| WHILE condition contenu WEND {While($2)::($3@Wend::[Empty])}

	| instr FOR IDENT AFFECT var_val TO terminal STEP math_signe var_val contenu NEXT IDENT {$1@(For(Ident $3,Empty,Empty,Equal,$5,To,$7,Step,$9,$10)::($11@[Next]))}   
	| instr FOR IDENT AS types AFFECT var_val TO terminal STEP math_signe var_val contenu NEXT IDENT {$1@(For(Ident $3,As,$5,Equal,$7,To,$9,Step,$11,$12)::($13@[Next]))}
	| FOR IDENT AFFECT var_val TO terminal STEP math_signe var_val contenu NEXT IDENT {For(Ident $2,Empty,Empty,Equal,$4,To,$6,Step,$8,$9)::($10@Next::[Empty])}   
	| FOR IDENT AS types AFFECT var_val TO terminal STEP math_signe var_val contenu NEXT IDENT {For(Ident $2,As,$4,Equal,$6,To,$8,Step,$10,$11)::($12@Next::[Empty])}
	
	| instr PRINT terminal {$1@(Print($3)::[Empty])}
	| PRINT terminal {Print($2)::[Empty]}
	
	/*| {[Empty]}*/
;

contenu :
	| instr {$1}
	| {[Empty]}
;

multAffect :
	| COLON IDENT AFFECT operation multAffect {Affect($2, $4)::$5}
	| {[Empty]}
;

else_block :
	| ELSEIF condition THEN instr else_block {ElseIf($2)::Then::($4@$5)}
	| ELSE instr {Else::$2}
	| {[Empty]}
;

fonctions :
	| SUB IDENT args contenu_fonc END_SUB {Sub($2)::($3@$4@[EndSub])}
	| FUNCTION IDENT args AS types contenu_fonc RETURN terminal END_FUNC {Function($2,return_type($5))::($3@$6@Return($8)::[EndFunc])}
	| DECLARE FUNCTION IDENT args AS types {[Empty]}
;

args :
	| LPAREN enum_args RPAREN {$2}
	| LPAREN RPAREN {DeclE::[Empty]}
	| {DeclE::[Empty]}
;

enum_args :
	| IDENT AS types COMMA enum_args {Decl($1, $3)::$5}
	| IDENT AS types {Declf($1, $3)::[Empty]}
;

enum_identMult :
	| IDENT COMMA enum_identMult {$1::$3}
	| IDENT {[$1]}
;

enum_ident :
	| IDENT AS types COMMA enum_ident {Decl($1, $3)::$5}
	| IDENT AS types {Decl($1, $3)::[Empty]}
;

fonc_instr :
	| fonc_instr DIM AS types enum_identMult {$1@(DimMult_fonc($5, $4)::[Empty])}
	| fonc_instr DIM enum_ident_fonc {$1@$3}
	| DIM AS types enum_identMult {DimMult_fonc($4, $3)::[Empty]}
	| DIM enum_ident_fonc {$2@[Empty]}	
	
	| fonc_instr IDENT AFFECT operation multAffect_fonc {$1@(Affect_fonc($2, $4)::$5)}
	| IDENT AFFECT operation multAffect_fonc {Affect_fonc($1, $3)::($4@[Empty])}
	
	| fonc_instr IF condition THEN fonc_instr else_block_fonc ENDIF {$1@(If_fonc($3)::Then_fonc::($5@$6@[EndIf_fonc]))}
	| IF condition THEN fonc_instr else_block_fonc ENDIF {If_fonc($2)::Then_fonc::($4@$5@EndIf_fonc::[Empty])}
	/*
	| fonc_instr DO WHILE condition fonc_instr LOOP {$1@(Do_fonc::($4@Loop_fonc::DoWhile_fonc($3)::[Empty]))}
	| fonc_instr DO UNTIL condition fonc_instr LOOP {$1@(Do_fonc::($4@Loop_fonc::DoWhile_fonc($3)::[Empty]))}
	*/
	| fonc_instr DO fonc_instr LOOP WHILE condition {$1@(Do_fonc::($3@Loop_fonc::DoWhile_fonc($6)::[Empty]))}
	| fonc_instr DO fonc_instr LOOP UNTIL condition {$1@(Do_fonc::($3@Loop_fonc::DoWhile_fonc($6)::[Empty]))}
	| DO fonc_instr LOOP WHILE condition {Do_fonc::($2@Loop_fonc::DoWhile_fonc($5)::[Empty])}
	| DO fonc_instr LOOP UNTIL condition {Do_fonc::($2@Loop_fonc::DoWhile_fonc($5)::[Empty])}
	
	| fonc_instr WHILE condition fonc_instr WEND {$1@(While_fonc($3)::($4@[Wend_fonc]))}
	| WHILE condition fonc_instr WEND {While_fonc($2)::($3@Wend_fonc::[Empty])}

	| fonc_instr FOR IDENT AFFECT var_val TO borne_condition STEP math_signe var_val fonc_instr NEXT IDENT {$1@(For_fonc(Ident $3,Empty,Empty,Equal,$5,To,$7,Step,$9,$10)::($11@[Next_fonc]))}   
	| fonc_instr FOR IDENT AS types AFFECT var_val TO borne_condition STEP math_signe var_val fonc_instr NEXT IDENT {$1@(For_fonc(Ident $3,As,$5,Equal,$7,To,$9,Step,$11,$12)::($13@[Next_fonc]))}
	| FOR IDENT AFFECT var_val TO borne_condition STEP math_signe var_val fonc_instr NEXT IDENT {For_fonc(Ident $2,Empty,Empty,Equal,$4,To,$6,Step,$8,$9)::($10@Next_fonc::[Empty])}   
	| FOR IDENT AS types AFFECT var_val TO borne_condition STEP math_signe var_val fonc_instr NEXT IDENT {For_fonc(Ident $2,As,$4,Equal,$6,To,$8,Step,$10,$11)::($12@Next_fonc::[Empty])}
	
	| fonc_instr PRINT terminal {$1@(Print_fonc($3)::[Empty])}
	| PRINT terminal {Print_fonc($2)::[Empty]}
	
	/*| {[Empty]}*/
;

contenu_fonc :
	| fonc_instr {$1}
	| {[Empty]}
;

enum_ident_fonc :
	| IDENT AS types COMMA enum_ident_fonc {Decl($1, $3)::$5}
	| IDENT AS types {Decl($1, $3)::[Empty]}
;

multAffect_fonc :
	| COLON IDENT AFFECT operation multAffect_fonc {Affect_fonc($2, $4)::$5}
	| {[Empty]}
;

else_block_fonc :
	| ELSEIF condition THEN fonc_instr else_block_fonc {ElseIf_fonc($2)::Then_fonc::($4@$5)}
	| ELSE fonc_instr {Else_fonc::$2}
	| {[Empty]}
;

operation :
	| LPAREN operation RPAREN {"("^$2^")"}
	| operation math_signe operation {$1^(return_math $2)^$3}
	| terminal {return_terminal $1}
;

terminal :
	| IDENT {Ident $1}
	| INT {Integer $1}
	| DOUBLE {Double $1}
	| STRING {String $1}
	| TRUE {True}
	| FALSE {False}
;
	
operateur :
	| EQ {Equal}
	| LT {Lesser}
	| GT {Greater}
	| NE {Notequal} 
	| LE {Lessequal} 
	| GE {Greaterequal} 
;

condition :
	| TRUE {Conditionnelle(True,Empty,Empty)}
	| FALSE {Conditionnelle(False,Empty,Empty)}
	| borne_condition operateur borne_condition {Conditionnelle($1,$2,$3)}
;


math_signe :
	| PLUS {Plus}
	| MINUS {Minus}
	| DIV {Div}
	| MUL {Mul}
;

borne_condition :
	| IDENT {Ident $1}
	| INT {Integer $1}
	| DOUBLE {Double $1}
;

var_val :
	| INT {Integer $1}
	| DOUBLE {Double $1}
;

types :
	TYPE_INT {Tint "int "}
	| TYPE_DOUBLE {Tdouble "double "}
	| TYPE_STRING {Tstring "char* "}
	| TYPE_CHAR {Tchar "char "}
;
