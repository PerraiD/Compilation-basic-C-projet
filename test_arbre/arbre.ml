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
		| Function

type t_import=
	|Empty

type t_prog = {
  mutable struct_instr :  t_instr list;
  mutable struct_fonc :   t_fonc list;
  mutable struct_import:   t_import list;
}
  

let set_prog_list a b c = {struct_instr = a ; struct_fonc = b; struct_import = c} 



let rec  print_instr structprog = match structprog with
	| If(ast_a,cond,ast_b)::tl -> print_string("If ");print_terminal(ast_a);print_cond(cond);print_terminal(ast_b); print_instr tl
	| Then::tl-> print_string("Then"); print_instr tl
	| Else::tl->  print_string("ELse"); print_instr tl
	| Print(print)::tl->  print_string(print^"\n"); print_instr tl
	| [] -> print_string(" ")

and print_terminal t = match t with 
	| Ident(id) -> print_string(id);
	

and  print_cond  = function
	| Lesser -> print_string("<")
	| Greater ->  print_string(">")
	| Equal -> print_string("=")


let rec print_fonc structprog = match structprog with
	| Function::tl -> print_string("Fonction"); print_fonc tl
	| [] -> print_string("\n")

let print_prog prog = 
	  print_instr prog.struct_instr;
	  print_fonc prog.struct_fonc

