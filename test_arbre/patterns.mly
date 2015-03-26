%{	
	open Arbre
	let oc = open_out "prog.c"
	let () = output_string oc "#include <stdio.h>\n"
	let () = output_string oc "int main(){\n\n\t"
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
| instr EOF {set_ast_list [$1]}



instr:
| PRINT IDENT{Print($2)}
| IF IDENT condition IDENT {If(Ident $2,$3,Ident $4)} 


condition:
|EQ {Equal}
|LT {Lesser}
|GT {Greater}