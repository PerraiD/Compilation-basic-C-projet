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
%token <string> CHAINE
%token BS

/*(* declaration *)*/
%token <string> DIM
%token <string> As
%token <string> IDENT

/*(* maths *)*/
%token PLUS
%token MINUS
%token TIMES
%token OBELUS

/*(* signes *)*/
%token SEMICOLON
%token COMMA
%token DOUBLEQUOTE
%token LPARENTHESE
%token RPARENTHESE

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
	| contenu DOUBLEQUOTE types DOUBLEQUOTE {$1; output_string oc ("printf(\""^$3^"\");")}
	| contenu DOUBLEQUOTE types BS types DOUBLEQUOTE {$1; output_string oc ("printf(\""^$3^"\\\\"^$5^"\");")}
	| contenu SEMICOLON {$1}
	/*| contenu COMMA {$1; output_string oc ("printf(\"%14s\",\"\");")}*/
	| {}
;

formule:
	formule operateur formule {$1^$2^$3}
	| LPARENTHESE formule RPARENTHESE {"("^$2^")"}
	| types {$1}
;

operateur:
	PLUS {"+"}
	| MINUS {"-"}
	| TIMES {"*"}
	| OBELUS {"/"}
;

types:
	INT {$1}
	| DOUBLE {$1}
	| CHAINE {$1}
;
