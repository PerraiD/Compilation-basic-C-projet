
let oc = open_out "prog.c"
let indent = ref 0
let indent_func = ref 0
let includeprintf = ref false
let nameFunc = ref ""
let typeFunc = ref ""

let hash_table = Hashtbl.create 100;;
let hash_table_fonc = Hashtbl.create 100;;

let incr v = v:=!v+1;;
let decr v = v:=!v-1;;

type t_decl = {
	_name : string;
	_type : string;
};;

type t_terminal =
		| Ident of string
		| Integer of string
		| Double of string
		| Char of string
		| String of string
		| True
		| False
		| As
		| To
		| Step 
		| Empty
;;

type  t_operateur = 
		| Lesser 
		| Greater 
		| Equal
		| Notequal
		| Lessequal
		| Greaterequal
		| Empty
;;

type t_condition=
		| Conditionnelle of t_terminal * t_operateur * t_terminal
;;		

type t_math = 
		| Minus
		| Plus
		| Mul
		| Div
;;

type  t_type = 
		| Tint  of string
		| Tdouble of string
		| Tstring of string
		| Tchar of string
		| Empty
;;

type t_instr =
		| SCom of string
		| MCom of string
	
		| DimMult of string list * t_type
		| Decl of string * t_type
		| Affect of string * string
		| Print of t_terminal
		| If of t_condition
		| ElseIf of t_condition
		| Then 
		| Else	
		| EndIf
		
		| While of t_condition
		| Wend
		| Do
		| Loop
		
		| DoWhile of t_condition
		| For of t_terminal * t_terminal * t_type * t_operateur * t_terminal * t_terminal * t_terminal *  t_terminal * t_math* t_terminal
		| Next
		
		| Empty
;;

type t_fonc =
		| Sub of string
		| EndSub
		| Function of string * string
		| Return of t_terminal
		| EndFunc
		
		| DimMult_fonc of string list * t_type
		| Decl of string * t_type
		| Declf of string * t_type
		| DeclE
		| Affect_fonc of string * string
		| Print_fonc of t_terminal
		
		| If_fonc of t_condition
		| ElseIf_fonc of t_condition
		| Then_fonc 
		| Else_fonc	
		| EndIf_fonc
		
		| While_fonc of t_condition
		| Wend_fonc
		| Do_fonc
		| Loop_fonc
		
		| DoWhile_fonc of t_condition
		| For_fonc of t_terminal * t_terminal * t_type * t_operateur * t_terminal * t_terminal * t_terminal *  t_terminal * t_math* t_terminal
		| Next_fonc
		
		| Empty
;;

type t_import=
		| Include of string
;;

type t_prog = {
  mutable struct_instr :  t_instr list;
  mutable struct_fonc :   t_fonc list;
  mutable struct_import:   t_import list;
};;

let set_prog_list a b c = {struct_instr = a ; struct_fonc = b; struct_import = c};;

let indentation = function () -> for i=0 to !indent do output_string oc "\t" done;;
let indentation_fonc = function () -> for i=1 to !indent_func do output_string oc "\t" done;;

let return_type = function
	| Tint(v) -> v;
	| Tdouble(v) -> v;
	| Tstring(v) -> v;
	| Tchar(v) -> v;
	| Empty -> "";
;;

let return_terminal = function
	| Ident(id) -> id;
	| Double(v) -> v;
	| Integer(v) -> v;
	| Char(v) -> v;
	| String(v) -> v;
	| True -> "true";
	| False -> "false";
	| As -> "";
	| To -> "";
	| Step -> "";
	| Empty -> "";
;;

let return_math = function 
	| Minus -> "-";
	| Plus -> "+";
	| Mul -> "*";
	| Div -> "/";
;;

let rec hasher = function
	| (t, hd::[]) -> Hashtbl.add hash_table hd (return_type t);
	| (t, hd::tl) -> Hashtbl.add hash_table hd (return_type t); hasher(t, tl);
	| (t, []) -> ();
;;

let verifType typ retour = match Hashtbl.find hash_table_fonc retour with
	| x when x = typ -> ();
	| _ -> print_string("[Erreur] La fonction \""^(!nameFunc)^"\" devrait retourner : "^typ^"\n");
;;

let rec print_terminal t = match t with 
	| Ident(id) -> output_string oc (id);
	| Double(v) -> output_string oc (v);
	| Integer(v) -> output_string oc (v);
	| Char(v) -> output_string oc (v);
	| String(v) -> output_string oc (v);
	| True -> output_string oc ("true");
	| False -> output_string oc ("false");
	| Empty -> ();
	| _ -> ();
	
and printf_terminal t = match t with 
	| Ident(id) -> print_ident id hash_table;
	| Double(v) -> output_string oc ("printf(\"%lf\", "^v^");\n");
	| Integer(v) -> output_string oc ("printf(\"%d\", "^v^");\n");
	| Char(v) -> output_string oc ("printf(\"%c\", "^v^");\n");
	| String(v) -> output_string oc ("printf(\"%s\", "^v^");\n");
	| Empty -> ();
	| _ -> ();
	
and printf_terminal_fonc t = match t with 
	| Ident(id) -> print_ident id hash_table_fonc;
	| Double(v) -> output_string oc ("printf(\"%lf\", "^v^");\n");
	| Integer(v) -> output_string oc ("printf(\"%d\", "^v^");\n");
	| Char(v) -> output_string oc ("printf(\"%c\", "^v^");\n");
	| String(v) -> output_string oc ("printf(\"%s\", "^v^");\n");
	| Empty -> ();
	| _ -> ();
	
and print_ident id hash = match Hashtbl.find hash id with
	|"int " -> output_string oc ("printf(\"%d\","^id^");\n");
    |"double "-> output_string oc ("printf(\"%lf\","^id^");\n");
    |"char* "-> output_string oc ("printf(\"%s\","^id^");\n");
	| _ -> ();

and  print_operateur = function
	| Lesser -> output_string oc("<");
	| Greater ->  output_string oc(">");
	| Equal -> output_string oc("==");
	| Notequal -> output_string oc("!=");
	| Lessequal ->  output_string oc("<=");
	| Greaterequal -> output_string oc(">=");
	| Empty -> ();

and print_condition = function 
	| Conditionnelle(t_terma, t_ope ,t_termb) -> output_string oc ("("); print_terminal(t_terma);print_operateur(t_ope);print_terminal(t_termb); output_string oc (")");
	
and print_math = function 
	| Minus -> output_string oc ("-");
	| Plus -> output_string oc ("+");
	| Mul -> output_string oc ("*");
	| Div -> output_string oc ("/");

and print_type = function 
	| Tint(v) -> output_string oc (v);
	| Tdouble(v) -> output_string oc (v);
	| Tstring(v) -> output_string oc (v);
	| Tchar(v) -> output_string oc (v);
	| Empty -> ();
;;

let rec print_instr structprog = match structprog with
	| SCom(com)::tl -> 	indentation ();
						output_string oc ("// "^(String.sub com 1 ((String.length com)-1)));
						print_instr tl;
	
	| MCom(com)::tl -> 	indentation ();
						output_string oc ("/* "^(String.sub com 2 ((String.length com)-4))^"*/");
						print_instr tl;
	
	| DimMult(vars, typ)::tl -> hasher(typ, vars);
								indentation ();
								output_string oc ((return_type typ)^(String.concat ", " vars)^";\n"); 
								print_instr tl;
								
	| Decl(id, typ)::tl -> 	Hashtbl.add hash_table id (return_type typ); 
							indentation (); 
							output_string oc ((return_type typ)^id^";\n"); 
							print_instr tl;
							
	| Affect(id, v)::tl -> 	indentation();
							output_string oc (id^" = "^v^";\n");
							print_instr tl;
							
	| If(cond)::tl -> 	indentation ();
						incr(indent);
						output_string oc ("if ");
						print_condition(cond);
						print_instr tl;
						
	| ElseIf(cond)::tl -> 	decr(indent);
							indentation ();
							incr(indent);
							output_string oc ("}else if ");
							print_condition(cond);
							print_instr tl;
							
	| Then::tl -> 	output_string oc(" { \n"); 
					print_instr tl;
					
	| Else::tl-> 	decr(indent);
					indentation ();
					output_string oc("} else { \n");
					incr(indent);
					print_instr tl;
					
	| EndIf::tl -> 	decr(indent);
					indentation ();
					output_string oc("}\n");
					print_instr tl;
	
	| While(cond)::tl -> 	indentation (); 
							incr(indent);
							output_string oc ("while ");
							print_condition(cond);
							output_string oc ("{ \n"); 
							print_instr tl;
							
	| Wend::tl -> 	decr(indent);
					indentation (); 
					output_string oc("}\n"); 
					print_instr tl;
	
	| Do::tl -> 	indentation (); 
					incr(indent);
					output_string oc("do {\n"); 
					print_instr tl;
							
	| DoWhile(cond)::tl -> 	output_string oc ("while ");
							print_condition(cond);
							output_string oc (";\n\n"); 
							print_instr tl;
							
	| Loop::tl -> 	decr(indent);
					indentation ();
					output_string oc ("} ");
					print_instr tl;
	
	| For(t_a,t_b,typ,t_ope,t_d,t_e,t_f,t_g,math,t_i)::tl -> 	indentation (); 
																incr(indent); 
																output_string oc("for (");
																print_type(typ);
																print_terminal(t_a);
																print_operateur(t_ope);
																print_terminal(t_d);
																output_string oc (";");
																print_terminal(t_a);
																output_string oc (" < "); 
																print_terminal(t_f);
																output_string oc (";");
																print_terminal(t_a);
																print_math(math);
																output_string oc("=");
																print_terminal(t_i);
																output_string oc("){\n");
																print_instr tl;
																
	| Next::tl -> 	decr(indent);
					indentation (); 
					output_string oc ("}\n"); 
					print_instr tl;

	| Print(print)::tl -> 	indentation (); 
							printf_terminal(print); 
							print_instr tl;
							
	| Empty::tl -> print_instr tl;
	| [] -> ();
;;

let rec print_fonc structprog = match structprog with
	| DimMult_fonc(vars, typ)::tl -> 	hasher(typ, vars); 
										indentation_fonc (); 
										output_string oc ((return_type typ)^(String.concat ", " vars)^";\n"); 
										print_fonc tl;
								
	| Decl(id, typ)::tl -> Hashtbl.add hash_table_fonc id (return_type typ);
								output_string oc ((return_type typ)^id^", ");
								print_fonc tl;
							
	| Declf(id, typ)::tl -> Hashtbl.add hash_table_fonc id (return_type typ);
							output_string oc ((return_type typ)^id^") {\n");
							print_fonc tl;
							
	| DeclE::tl -> 	output_string oc (") {\n");
					print_fonc tl;
					
	| Affect_fonc(id, v)::tl -> indentation_fonc ();
								output_string oc (id^" = "^v^";\n");
								print_fonc tl;
	
	| Sub(nom)::tl -> 	indentation_fonc ();
						incr(indent_func);
						output_string oc ("void "^nom^"(");
						print_fonc tl;
						
	| EndSub::tl -> 	decr(indent_func);
						indentation_fonc ();
						output_string oc ("}\n\n");
						print_fonc tl;
						
	| Function(nom,type_retour)::tl -> 	typeFunc := type_retour;
										nameFunc := nom;
										indentation_fonc ();
										incr(indent_func);
										output_string oc (type_retour^nom^"(");
										print_fonc tl;
										
	| Return(retour)::tl -> verifType !typeFunc (return_terminal retour);
							indentation_fonc ();
							output_string oc ("return ");
							print_terminal(retour);
							output_string oc (";\n");
							print_fonc tl;
							
	| EndFunc::tl -> 	decr(indent_func);
						indentation_fonc ();
						output_string oc ("}\n\n");
						print_fonc tl;
						
	| Print_fonc(print)::tl -> 	indentation_fonc ();
								printf_terminal_fonc(print);
								print_fonc tl;
								
	| If_fonc(cond)::tl -> 	indentation_fonc (); 
							incr(indent_func);
							output_string oc ("if ");
							print_condition(cond); 
							print_fonc tl;
						
	| ElseIf_fonc(cond)::tl -> 	decr(indent_func);
								indentation_fonc (); 
								incr(indent_func);
								output_string oc ("}else if ");
								print_condition(cond); 
								print_fonc tl;
							
	| Then_fonc::tl -> 	output_string oc(" { \n"); 
						print_fonc tl;
					
	| Else_fonc::tl-> 	decr(indent_func);
						indentation_fonc (); 
						output_string oc("} else { \n"); 
						incr(indent);
						print_fonc tl;
					
	| EndIf_fonc::tl -> decr(indent_func);
						indentation_fonc (); 
						output_string oc("}\n"); 
						print_fonc tl;
	
	| While_fonc(cond)::tl -> 	indentation_fonc (); 
								incr(indent_func);
								output_string oc ("while ");
								print_condition(cond);
								output_string oc ("{ \n"); 
								print_fonc tl;
							
	| Wend_fonc::tl -> 	decr(indent_func);
						indentation_fonc (); 
						output_string oc("}\n"); 
						print_fonc tl;
	
	| Do_fonc::tl -> 	indentation_fonc (); 
						incr(indent_func);
						output_string oc("do {\n"); 
						print_fonc tl;
							
	| DoWhile_fonc(cond)::tl -> output_string oc ("while ");
								print_condition(cond);
								output_string oc (";\n\n"); 
								print_fonc tl;
							
	| Loop_fonc::tl -> 	decr(indent_func);
						indentation_fonc (); 
						output_string oc ("} "); 
						print_fonc tl;
	
	| For_fonc(t_a,t_b,typ,t_ope,t_d,t_e,t_f,t_g,math,t_i)::tl -> 	indentation_fonc (); 
																	incr(indent_func);
																	output_string oc("for (");
																	print_type(typ);
																	print_terminal(t_a);
																	print_operateur(t_ope);
																	print_terminal(t_d);
																	output_string oc (";");
																	print_terminal(t_a);
																	output_string oc (" < "); 
																	print_terminal(t_f);
																	output_string oc (";");
																	print_terminal(t_a);
																	print_math(math);
																	output_string oc("=");
																	print_terminal(t_i);
																	output_string oc("){\n");
																	print_fonc tl;
																
	| Next_fonc::tl -> 	decr(indent_func);
						indentation_fonc (); 
						output_string oc ("}\n"); 
						print_fonc tl;
								
	| Empty::tl -> print_fonc tl;
	| [] -> ();
;;
	
let rec print_import structprog = match structprog with
	| Include(print)::tl -> output_string oc print; print_import tl;
	| [] -> if (!includeprintf=true) then output_string oc ("#include <stdio.h>\n\n");
;;

let rec lookingForIncludes structprog = match structprog with
	| Print(print)::tl-> includeprintf:=true; lookingForIncludes tl;
	| [] -> ();
	| _::tl -> lookingForIncludes tl;
;;

let print_prog prog =
	lookingForIncludes prog.struct_instr;
	print_import prog.struct_import;
	print_fonc prog.struct_fonc;
	output_string oc "int main(){\n\n";
	print_instr prog.struct_instr;
	output_string oc "\n}\n\n";
;;
