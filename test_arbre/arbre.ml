
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

type t_import=
		| Include of string

type t_prog = {
  mutable struct_instr :  t_instr list;
  mutable struct_fonc :   t_fonc list;
  mutable struct_import:   t_import list;
}
  

let set_prog_list a b c = {struct_instr = a ; struct_fonc = b; struct_import = c} 



let rec  print_instr structprog = match structprog with
	| If(ast_a,cond,ast_b)::tl -> print_string("If ");print_terminal(ast_a);print_cond(cond);print_terminal(ast_b); print_instr tl
	| Then::tl-> print_string("Then"); print_instr tl
	| Else::tl->  print_string("Else"); print_instr tl
	| Print(print)::tl-> print_string(print^"\n"); print_instr tl
	| [] -> print_string(" ")
	

and print_terminal t = match t with 
	| Ident(id) -> print_string(id);
	

and  print_cond  = function
	| Lesser -> print_string("<")
	| Greater ->  print_string(">")
	| Equal -> print_string("=")


let rec print_fonc structprog = match structprog with
	| Function(a,b)::tl -> print_string("Fonction"); print_fonc tl
	| PrintFonc(print)::tl-> print_string(print^"\n"); print_fonc tl
	| [] -> print_string("vide \n")
	
let rec print_import structprog = match structprog with
	| Include(print)::tl -> print_string(print^"\n"); print_import tl
	| [] -> print_string("vide \n")
	


let rec  print_fi structprog = match structprog with
	| Print(print)::tl-> output_string oc ("printf(\""^print^"\");\n"); for i=0 to indent-1 do output_string oc "\t" done; print_fi tl
	| [] -> ()

let rec print_fil structprog = match structprog with
	| Function(nom,typ)::tl -> output_string oc (typ^" "^nom^"{\n"); print_fil tl
	| PrintFonc(print)::tl-> output_string oc ("printf(\""^print^"\");\n}\n"); print_fil tl
	| [] -> ()
	
let rec print_f structprog = match structprog with
	| Include(print)::tl -> output_string oc print; print_f tl
	| [] -> ()
	


let print_prog prog = 
	print_import prog.struct_import;
	print_instr prog.struct_instr;
	print_fonc prog.struct_fonc;
	print_f prog.struct_import;
	output_string oc "int main(){\n\n";
	for i=0 to indent-1 do output_string oc "\t" done;
	print_fi prog.struct_instr;
	output_string oc "\n}\n\n";
	print_fil prog.struct_fonc

