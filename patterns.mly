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

/*(*var value*)*/
%token <string> VAR_INT
%token <string> VAR_DOUBLE
%token <string> VAR_STRING

/*(* declaration *)*/
%token DIM
%token AS
%token CONST
%token <string> IDENT
%token <string> TYPE_INT
%token <string> TYPE_DOUBLE
%token <string> TYPE_STRING
%token <string> TYPE_CHAR

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
	instructions EOF {output_string oc "\n}"}
;

instructions:
	instructions affichage {}
	| instructions declaration {}
	| declaration {}
	| affichage {}
;

declaration:
	DIM AS types liste_variables EOL {output_string oc ($3^" "^$4^";")}
	| DIM variables EOL {output_string oc ($2)}
	| CONST IDENT EQ types_var EOL {output_string oc("#define "^$2^" "^$4^";")}
	| CONST IDENT AS types EQ types_var EOL {output_string oc("static const "^$4^" "^$2^" = "^$6^";")}	
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
	| PRINT contenu EOL {$2; output_string oc ("printf(\"\\n\");\n\t")}
;

contenu:
	contenu formule {$1; output_string oc ("printf(\" %f\",("^$2^")*1.0);")}
	| contenu IDENT {$1}
	| contenu chaine {$1; output_string oc ("printf("^$2^");")}
	| contenu SEMICOLON {$1}
	| {}
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

