
let oc = open_out "prog.c"
let indent = ref 1

type t_terminal =
		| Ident of string 
		| Integer of string
		| Double of string
		| True
		| False
		| As
		| To
		| Step 
		| Empty


type  t_operateur = 
		| Lesser 
		| Greater 
		| Equal
		| Notequal
		| Lessequal
		| Greaterequal
		| Empty

type t_condition=
		| Conditionelle of t_terminal * t_operateur * t_terminal
		

type t_math = 
		| Minus
		| Plus


type  t_type = 
		| Tint  of string
		| Tdouble of string
		| Tstring of string
		| Tchar of string
		| Empty

type t_instr = 
		| Print of string
		| If of t_condition
		| ElseIf of t_condition
		| Then 
		| Else	
		| EndIf
		
		| While of t_condition
		| Wend
		| Do
		| Loop
		
		| Until of t_condition
		| For of t_terminal * t_terminal * t_type * t_operateur * t_terminal * t_terminal * t_terminal *  t_terminal * t_math* t_terminal
		| Next
		
		| Empty


type t_fonc = 
		| Function of string * string
		| PrintFonc of string
		| Empty

type t_import=
		| Include of string

type t_prog = {
  mutable struct_instr :  t_instr list;
  mutable struct_fonc :   t_fonc list;
  mutable struct_import:   t_import list;
}
  

let set_prog_list a b c = {struct_instr = a ; struct_fonc = b; struct_import = c};;

let indentation = function () -> for i=0 to !indent-1 do output_string oc "\t" done;;

let rec print_terminal t = match t with 
	| Ident(id) -> output_string oc (id);
	| Double(v) -> output_string oc (v);
	| Integer(v) -> output_string oc (v);
	| Empty -> ()
	

and  print_operateur  = function
	| Lesser -> output_string oc("<")
	| Greater ->  output_string oc(">")
	| Equal -> output_string oc("=")
	| Notequal -> output_string oc("!=")
	| Lessequal ->  output_string oc("<=")
	| Greaterequal -> output_string oc(">=")
	| Empty -> ()

and print_condition = function 
	| Conditionelle(t_terma, t_ope ,t_termb) -> output_string oc ("("); print_terminal(t_terma);print_operateur(t_ope);print_terminal(t_termb); output_string oc (")");
	
and print_math = function 
	| Minus -> output_string oc ("-")
	| Plus -> output_string oc ("+")

and print_type =function 
	| Tint (v) ->output_string oc (v)
	| Tdouble(v) ->output_string oc (v)
	| Tstring(v) ->output_string oc (v)
	| Tchar(v) ->output_string oc (v)
	| Empty -> ()
;;

let rec  print_instr structprog = match structprog with
	| If(cond)::tl -> indentation (); indent:=!indent+1; output_string oc ("if ");print_condition(cond); print_instr tl
	| ElseIf(cond)::tl -> indent:=!indent-1; indentation (); indent:=!indent+1; output_string oc ("}else if ");print_condition(cond); print_instr tl
	| Then::tl -> output_string oc(" { \n"); print_instr tl;
	| Else::tl-> indent:=!indent-1; indentation (); output_string oc("} else { \n"); print_instr tl
	| EndIf::tl -> indent:=!indent-1; indentation (); output_string oc("}\n"); print_instr tl
	
	| While(cond)::tl -> output_string oc ("while ");print_condition(cond);output_string oc ("{ \n"); print_instr tl
	| Wend::tl -> output_string oc("}\n"); print_instr tl
	
	| Do::tl -> output_string oc("Do {\n"); print_instr tl
	| Until(cond)::tl -> output_string oc ("while ");print_condition(cond); output_string oc ("\n"); print_instr tl
	| Loop::tl -> output_string oc ("} "); print_instr tl
	
	|For(t_a,t_b,typ,t_ope,t_d,t_e,t_f,t_g,math,t_i)::tl->output_string oc("for (");print_type(typ);print_terminal(t_a);print_operateur(t_ope);print_terminal(t_d);output_string oc (";");print_terminal(t_a);output_string oc (" < "); print_terminal(t_f);output_string oc (";");
	 print_terminal(t_a);print_math(math);output_string oc("=");print_terminal(t_i);output_string oc("){\n");print_instr tl;
	|Next::tl -> (); print_instr tl;

	| Print(print)::tl-> indentation (); output_string oc ("printf(\""^print^"\");\n"); print_instr tl
	| Empty::tl-> print_instr tl
	| [] -> ()
;;

let rec print_fonc structprog = match structprog with
	| Function(nom,typ)::tl -> output_string oc (typ^" "^nom^"{\n"); print_fonc tl
	| PrintFonc(print)::tl-> output_string oc ("printf(\""^print^"\");\n"); print_fonc tl
	| Empty::tl -> print_fonc tl
	| [] -> output_string oc ("\n")
;;
	
let rec print_import structprog = match structprog with
	| Include(print)::tl -> output_string oc print; print_import tl
	| [] -> ()
;;

let print_prog prog = 
	print_import prog.struct_import;
	output_string oc "int main(){\n\n";
	print_instr prog.struct_instr;
	output_string oc "\n}\n\n";
	print_fonc prog.struct_fonc
;;
