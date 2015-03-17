%{
	let oc = open_out "prog.c"
	let () = output_string oc "#include <stdio.h>\n"
	let () = output_string oc "int main(){\n\n\t"
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
%type <unit> main

%%

main:
	instructions EOF {output_string oc ($1^"\n}")}
;

instructions:
	instructions affichage {$1^$2}
	| instructions declaration {$1^$2}
	| instructions condition {$1^$2}
	| declaration {$1}
	| affichage {$1}
	| condition {$1}
;

condition:
	IF THEN ENDIF {"if{\n\t\n}"}
;

test:
	INT signe INT {$1^$2^$3}
;

signe:
	LT {"<"}
	| GT {">"}
	| EQ {"="}
	| NE {"!="}
	| LE {"<="}
	| GE {">="}
;

declaration:
	DIM AS types liste_variables EOL {$3^" "^$4^";"}
	| DIM variables EOL {$2}
;

variables:
	variables COMMA IDENT AS types {$1^$5^" "^$3^";\n\t"}
	| IDENT AS types {$3^" "^$1^";\n\t"}
;

liste_variables:
	liste_variables COMMA IDENT {$1^", "^$3}
	| IDENT {$1}
;

affichage:
	PRINT contenu COMMA {$2}
	| PRINT contenu EOL {$2^"printf(\"\\n\");\n\t"}
;

contenu:
	contenu formule {$1^"printf(\" %f\",("^$2^")*1.0);"}
	| contenu IDENT {$1}
	| contenu chaine {$1^"printf("^$2^");"}
	| contenu SEMICOLON {$1}
	| {""}
;

chaine:
	chaine BS chaine {$1^"\\\\"^$3}
	/*| STRING {$1}*/
	| SUB_STRING {$1}
;

formule:
	formule PLUS formule {$1^"+"^$3}
	| formule MINUS formule {$1^"-"^$3}
	| formule MUL formule {$1^"*"^$3}
	| formule DIV formule {$1^"/"^$3}
	| LPAREN formule RPAREN {"("^$2^")"}
	| INT {$1}
	| DOUBLE {$1}
;


/*var_type:*/


types_var:
	INT {$1}
	| DOUBLE {$1}
	| STRING {$1}
	| SUB_STRING {$1}
;

types:
	TYPE_INT {"int"}
	| TYPE_DOUBLE {"double"}
	| TYPE_STRING {"char*"}
	| TYPE_CHAR {"char"}
;

