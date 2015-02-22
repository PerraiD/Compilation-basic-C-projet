%{
	let oc = open_out "prog.c"
	let () = output_string oc "#include <stdio.h>\n"
	let () = output_string oc "int main(){\n\n\t"
%}

%token EOF

%token <char> CHAR
%token <string> PRINT
%token <string> INT
%token <string> DOUBLE
%token <string> STRING
%token BS

%left BS

/*(* declaration *)*/
%token <string> DIM
%token <string> As
%token <string> IDENT

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
	/*| instructions declaration {}*/
	| {}
;

/*declaration:
	{}
;*/

affichage:
	PRINT contenu COMMA {$2}
	| PRINT contenu {$2; output_string oc ("printf(\"\\n\");\n\t")}
;

contenu:
	contenu formule {$1; output_string oc ("printf(\" %f\",("^$2^")*1.0);")}
	| contenu IDENT {$1}
	| contenu DOUBLEQUOTE chaine DOUBLEQUOTE {$1; output_string oc ("printf(\""^$3^"\");")}
	| contenu SEMICOLON {$1}
	/*| contenu COMMA {$1; output_string oc ("printf(\"%14s\",\"\");")}*/
	| {}
;

chaine:
	chaine BS chaine {$1^"\\\\"^$3}
	| types {$1}
;

formule:
	formule PLUS formule {$1^"+"^$3}
	| formule MINUS formule {$1^"-"^$3}
	| formule MUL formule {$1^"*"^$3}
	| formule DIV formule {$1^"/"^$3}
	| LPAREN formule RPAREN {"("^$2^")"}
	| types {$1}
;

/*operateur:
	PLUS {"+"}
	| MINUS {"-"}
	| MUL {"*"}
	| DIV {"/"}
;*/

types:
	INT {$1}
	| DOUBLE {$1}
	| STRING {$1}
;
