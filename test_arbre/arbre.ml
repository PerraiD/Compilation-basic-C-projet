
let oc = open_out "prog.c"
let indent = 1

type  t_cond = 
		| Lesser 
		| Greater 
		| Equal
		| Notequal
		| Lessequal
		| Greaterequal
		| Empty


type t_terminal =
		| Ident of string 
		| Empty


type t_instr = 
		| Print of string
		| If of t_terminal * t_cond * t_terminal
		| ElseIf of t_terminal * t_cond * t_terminal
		| Then 
		| Else 
		| EndIf
		| While of t_terminal * t_cond * t_terminal
		| Wend
		| Do
		| Loop
		| Until of t_terminal * t_cond * t_terminal
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
  

let set_prog_list a b c = {struct_instr = a ; struct_fonc = b; struct_import = c} 


let rec print_terminal t = match t with 
	| Ident(id) -> output_string oc (id);
	| Empty -> ()
	

and  print_cond  = function
	| Lesser -> output_string oc("<")
	| Greater ->  output_string oc(">")
	| Equal -> output_string oc("=")
	| Notequal -> output_string oc("!=")
	| Lessequal ->  output_string oc("<=")
	| Greaterequal -> output_string oc(">=")
	| Empty -> ()

	
let rec  print_instr structprog = match structprog with
	| If(term_a,cond,term_b)::tl -> output_string oc ("if ");print_terminal(term_a);print_cond(cond);print_terminal(term_b); print_instr tl
	| ElseIf(term_a,cond,term_b)::tl -> output_string oc ("else if ");print_terminal(term_a);print_cond(cond);print_terminal(term_b); print_instr tl
	| Then::tl-> output_string oc(" { \n"); print_instr tl
	| Else::tl->  output_string oc("} Else { \n"); print_instr tl
	| EndIf::tl -> output_string oc("}\n"); print_instr tl
	
	| While(term_a,cond,term_b)::tl -> output_string oc ("While ");print_terminal(term_a);print_cond(cond);print_terminal(term_b);output_string oc ("{ \n"); print_instr tl
	| Wend::tl -> output_string oc("}\n"); print_instr tl
	
	| Do::tl -> output_string oc("Do {\n"); print_instr tl
	| Until(term_a,cond,term_b)::tl -> output_string oc ("While ");print_terminal(term_a);print_cond(cond);print_terminal(term_b); output_string oc ("\n"); print_instr tl
	| Loop::tl -> output_string oc ("} "); print_instr tl

	| Print(print)::tl->  output_string oc ("printf(\""^print^"\");\n");for i=0 to indent-1 do output_string oc "\t" done ; print_instr tl
	| Empty::tl-> print_instr tl
	| [] -> ()

let rec print_fonc structprog = match structprog with
	| Function(nom,typ)::tl -> output_string oc (typ^" "^nom^"{\n"); print_fonc tl
	| PrintFonc(print)::tl-> output_string oc ("printf(\""^print^"\");\n"); print_fonc tl
	| Empty::tl -> print_fonc tl
	| [] -> output_string oc ("}\n")
	
let rec print_import structprog = match structprog with
	| Include(print)::tl -> output_string oc print; print_import tl
	| [] -> ()
	

let print_prog prog = 
	print_import prog.struct_import;
	output_string oc "int main(){\n\n";
	for i=0 to indent-1 do output_string oc "\t" done;
	print_instr prog.struct_instr;
	output_string oc "\n}\n\n";
	print_fonc prog.struct_fonc

