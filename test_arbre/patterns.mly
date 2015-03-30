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
%token <int> INT
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
%token <string> TYPE_INT
%token <string> TYPE_DOUBLE
%token <string> TYPE_STRING
%token <string> TYPE_CHAR

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
	| IF IDENT condition IDENT instr {If(Ident $2,$3,Ident $4)::$5}
	| IF IDENT instr {If(Ident $2,Empty,Empty)::$3}
	| THEN instr {Then::$2}
	| ELSE instr {Else::$2}
	| ELSEIF IDENT condition IDENT instr {ElseIf(Ident $2,$3,Ident $4)::$5}
	| ELSEIF IDENT instr {ElseIf(Ident $2,Empty,Empty)::$3}
	| ENDIF instr {EndIf::$2}
	
	| WHILE IDENT condition IDENT instr {While(Ident $2,$3,Ident $4)::$5}
	| WHILE IDENT instr {While(Ident $2,Empty,Empty)::$3}
	| WEND instr {Wend::$2}

	| DO UNTIL IDENT condition IDENT instr {While(Ident $3,$4,Ident $5)::$6}
	| DO UNTIL IDENT instr {While(Ident $3,Empty,Empty)::$4}

	| DO instr {Do::$2} 
	| UNTIL IDENT condition IDENT instr {Until(Ident $2,$3,Ident $4)::$5}
	| UNTIL IDENT instr {Until(Ident $2,Empty,Empty)::$3}
	| LOOP instr {Loop::$2}

	| PRINT IDENT instr {Print($2)::$3}
	
	| EOL instr {Empty::$2}
	| {[Empty]}
;


functions :
	| SUB FUNC_NAME EOL fonc_instr { Function($2,"void")::$4}
	| FUNCTION FUNC_NAME AS types fonc_instr EOL  {[Function($2,$4)]}
	| END_SUB {[Empty]}
	| END_FUNC {[Empty]}
;

fonc_instr:
	| PRINT IDENT EOL fonc_instr {PrintFonc($2)::$4}
	| {[Empty]}
	
condition:
	|EQ {Equal}
	|LT {Lesser}
	|GT {Greater}
	|NE {Notequal} 
	|LE {Lessequal} 
	|GE {Greaterequal} 
;

types:
	TYPE_INT {"int"}
	| TYPE_DOUBLE {"double"}
	| TYPE_STRING {"char*"}
	| TYPE_CHAR {"char"}
;
