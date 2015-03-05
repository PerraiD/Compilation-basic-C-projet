%{
	let oc = open_out "prog.c"
	let () = output_string oc "#include <stdio.h>\n"
	let () = output_string oc "int main(){\n\n\t"
%}

%token EOF

/*(*var type*)*/
%token <char> CHAR
%token <string> PRINT
%token <string> INT
%token <string> DOUBLE
%token <string> STRING
%token CONST
%token BS

%left BS

/*(*var value*)*/
%token <string> VAR_INT
%token <string> VAR_DOUBLE
%token <string> VAR_STRING

/*(* declaration *)*/
%token <string> DIM
%token <string> As
%token <string> IDENT
%token CONST 
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
	| contenu declaration {$2}
	| {}
;

chaine:
	chaine BS chaine {$1^"\\\\"^$3}
	| var_value {$1}
;

formule:
	formule PLUS formule {$1^"+"^$3}
	| formule MINUS formule {$1^"-"^$3}
	| formule MUL formule {$1^"*"^$3}
	| formule DIV formule {$1^"/"^$3}
	| LPAREN formule RPAREN {"("^$2^")"}
	| var_value {$1}
;

/*operateur:
	PLUS {"+"}
	| MINUS {"-"}
	| MUL {"*"}
	| DIV {"/"}
;*/
declaration:
	CONST var_value {output_string oc (" "^$2)}

var_value:
	VAR_INT {$1}
	| VAR_DOUBLE {$1}
	| VAR_STRING {$1}

/*var_type:*/

;
