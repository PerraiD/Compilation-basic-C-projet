%{
	let oc = open_out "prog.c"
	let () = output_string oc "#include <stdio.h>\n"
	let () = output_string oc "int main(){\n"
%}

%token EOF

%token <char> CHAR
%token <string> PRINT
%token <string> VAR_INT
%token <string> VAR_DOUBLE
%token <string> CHAINE

%token SEMICOLON
%token DOUBLEQUOTE

%start main
%type <unit> main

%%

main:
  instructions EOF {output_string oc "\n}"}
;

instructions:
  instructions declaration {}
  | instructions affichage {}
  | {}
;

declaration:
	{}
;

affichage:
	PRINT contenu SEMICOLON {$2}
	| PRINT contenu {$2; output_string oc ("printf(\"\\n\");\n")}
;

contenu:
	contenu VAR_INT {$1; output_string oc ("printf(\" %d \","^$2^");")}
	| contenu VAR_DOUBLE {$1; output_string oc ("printf(\" %lf \","^$2^");")}
	| contenu DOUBLEQUOTE CHAINE DOUBLEQUOTE {$1; output_string oc ("printf(\""^$3^"\");")}
	| contenu SEMICOLON {$1}
	| {}
;

