
let oc = open_out "prog.c"
let indent = 1

type  t_cond = 
		| Lesser 
		| Greater 
		| Equal
		| Empty


type t_terminal =
		| Ident of string 
		


type t_instr = 
		| Print of string
		| If of t_terminal * t_cond * t_terminal 
		| Then 
		| Else 
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



let rec  print_instri structprog = match structprog with
	| If(ast_a,cond,ast_b)::tl -> print_string("If ");print_terminal(ast_a);print_cond(cond);print_terminal(ast_b); print_instri tl
	| Then::tl-> print_string("Then"); print_instri tl
	| Else::tl->  print_string("Else"); print_instri tl
	| Print(print)::tl-> print_string(print^"\n"); print_instri tl
	| Empty::tl -> print_instri tl
	| [] -> ()
	

and print_terminal t = match t with 
	| Ident(id) -> print_string(id);
	

and  print_cond  = function
	| Lesser -> print_string("<")
	| Greater ->  print_string(">")
	| Equal -> print_string("=")

	
let rec  print_instr structprog = match structprog with
	| Print(print)::tl-> output_string oc ("printf(\""^print^"\");\n"); for i=0 to indent-1 do output_string oc "\t" done; print_instr tl
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

