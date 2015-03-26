%{	
	open Arbre
	let oc = open_out "prog.c"
	let () = output_string oc "#include <stdio.h>\n"
	let () = output_string oc "int main(){\n\n\t"

	let x = {struct_fonc= []; struct_instr=[]; struct_import=[]}
	let add_fonc a = x.struct_fonc <- x.struct_fonc@a
	let add_import a = x.struct_import <-x.struct_import@a
	let add_instr a = x.struct_instr <-x.struct_instr@a


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
%token FUNCTION
%token DIM
%token AS
%token CONST
%token <string> IDENT
%token <string> TYPE_INT
%token <string> TYPE_DOUBLE
%token <string> TYPE_STRING
%token <string> TYPE_CHAR

/*(*condition*)*/
%token IF
%token THEN
%token ELSE
%token ELSEIF
%token ENDIF

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


prog: 
| import  {set_prog_list x.struct_instr x.struct_fonc x.struct_import}
| functions  {set_prog_list x.struct_instr x.struct_fonc x.struct_import}
| instr {set_prog_list x.struct_instr x.struct_fonc x.struct_import} 
| import prog  {set_prog_list x.struct_instr x.struct_fonc x.struct_import}
| functions prog {set_prog_list x.struct_instr x.struct_fonc x.struct_import}
| instr prog {set_prog_list x.struct_instr  x.struct_fonc x.struct_import} 


import :
|{Empty}

instr:
| PRINT IDENT {add_instr [Print($2)]}
| IF IDENT condition IDENT {add_instr [If(Ident $2,$3,Ident $4)]} 

functions :
 |FUNCTION {add_fonc [Function]}

condition:
|EQ {Equal}
|LT {Lesser}
|GT {Greater}