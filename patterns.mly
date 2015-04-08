%{	
	open Arbre

	let x = {struct_fonc= []; struct_instr=[]; struct_import=[]}
	let add_fonc a = x.struct_fonc <- a@x.struct_fonc
	let add_import a = x.struct_import <- a@x.struct_import
	let add_instr a = x.struct_instr <- a@x.struct_instr

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
	| import prog  {add_import $1;set_prog_list x.struct_instr x.struct_fonc x.struct_import}
	| fonctions prog {add_fonc $1;set_prog_list x.struct_instr x.struct_fonc x.struct_import}
	| instr prog {add_instr $1;set_prog_list x.struct_instr  x.struct_fonc x.struct_import}
	| {set_prog_list x.struct_instr x.struct_fonc x.struct_import}
;

import :
	| INCLUDE {[Include($1)]}
;

instr :	
	| DIM AS types enum_identMult instr {DimMult($4, $3)::$5}
	| DIM enum_ident instr {$2@$3}
	
	| IDENT AFFECT terminal multAffect instr {Affect($1, $3)::($4@$5)}
	
	| IF condition THEN instr else_block ENDIF instr {If($2)::Then::($4@$5@EndIf::$7)}
	
	| DO WHILE condition instr LOOP instr {Do::($4@Loop::DoWhile($3)::$6)}
	| DO UNTIL condition instr LOOP instr {Do::($4@Loop::Until($3)::$6)}
	
	| DO instr LOOP WHILE condition instr {Do::($2@Loop::DoWhile($5)::$6)}
	| DO instr LOOP UNTIL condition instr {Do::($2@Loop::Until($5)::$6)}
	
	| WHILE condition instr WEND instr {While($2)::($3@Wend::$5)}

	| FOR IDENT AFFECT var_val TO terminal STEP math_signe var_val instr NEXT IDENT instr {For(Ident $2,Empty,Empty,Equal,$4,To,$6,Step,$8,$9)::($10@Next::$13)}   
	| FOR IDENT AS types AFFECT var_val TO terminal STEP math_signe var_val instr NEXT IDENT instr {For(Ident $2,As,$4,Equal,$6,To,$8,Step,$10,$11)::($12@Next::$15)}
	
	| PRINT terminal instr {Print($2)::$3}
	
	| {[Empty]}
;

multAffect :
	| COLON IDENT AFFECT terminal multAffect {Affect($2, $4)::$5}
	| {[Empty]}
;

else_block :
	| ELSEIF condition THEN instr else_block {ElseIf($2)::Then::($4@$5)}
	| ELSE instr {Else::$2}
	| {[Empty]}
;

fonctions :
	| SUB IDENT args fonc_instr END_SUB {Sub($2)::($3@$4@[EndSub])}
	| FUNCTION IDENT args AS types fonc_instr RETURN terminal END_FUNC {Function($2,return_type($5))::($3@$6@Return($8)::[EndFunc])}
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
	| DIM AS types enum_identMult fonc_instr {DimMult_fonc($4, $3)::$5}
	| DIM enum_ident_fonc fonc_instr {$2@$3}
	
	| IDENT AFFECT terminal multAffect_fonc fonc_instr {Affect_fonc($1, $3)::($4@$5)}
	
	| IF condition THEN fonc_instr else_block_fonc ENDIF fonc_instr {If_fonc($2)::Then_fonc::($4@$5@EndIf_fonc::$7)}
	
	| DO WHILE condition fonc_instr LOOP fonc_instr {Do_fonc::($4@Loop_fonc::DoWhile_fonc($3)::$6)}
	| DO UNTIL condition fonc_instr LOOP fonc_instr {Do_fonc::($4@Loop_fonc::Until_fonc($3)::$6)}
	
	| DO fonc_instr LOOP WHILE condition fonc_instr {Do_fonc::($2@Loop_fonc::DoWhile_fonc($5)::$6)}
	| DO fonc_instr LOOP UNTIL condition fonc_instr {Do_fonc::($2@Loop_fonc::Until_fonc($5)::$6)}
	
	| WHILE condition fonc_instr WEND fonc_instr {While_fonc($2)::($3@Wend_fonc::$5)}

	| FOR IDENT AFFECT var_val TO terminal STEP math_signe var_val fonc_instr NEXT IDENT fonc_instr {For_fonc(Ident $2,Empty,Empty,Equal,$4,To,$6,Step,$8,$9)::($10@Next_fonc::$13)}   
	| FOR IDENT AS types AFFECT var_val TO terminal STEP math_signe var_val fonc_instr NEXT IDENT fonc_instr {For_fonc(Ident $2,As,$4,Equal,$6,To,$8,Step,$10,$11)::($12@Next_fonc::$15)}
	
	| PRINT terminal fonc_instr {Print_fonc($2)::$3}
	
	| {[Empty]}
;

enum_ident_fonc :
	| IDENT AS types COMMA enum_ident_fonc {Decl($1, $3)::$5}
	| IDENT AS types {Decl($1, $3)::[Empty]}
;

multAffect_fonc :
	| COLON IDENT AFFECT terminal multAffect_fonc {Affect_fonc($2, $4)::$5}
	| {[Empty]}
;

else_block_fonc :
	| ELSEIF condition THEN fonc_instr else_block_fonc {ElseIf_fonc($2)::Then_fonc::($4@$5)}
	| ELSE fonc_instr {Else_fonc::$2}
	| {[Empty]}
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
	| terminal operateur terminal {Conditionnelle($1,$2,$3)}
	| terminal {Conditionnelle($1,Empty,Empty)}
;


math_signe :
	| PLUS {Plus}
	| MINUS {Minus}
;

/*borne_condition :
	| IDENT {Ident $1}
	| INT {Integer $1}
	| DOUBLE {Double $1}
	| STRING {String $1}
;*/

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
