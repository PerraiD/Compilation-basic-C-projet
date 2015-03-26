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
		|Empty

type t_prog = {
  struct_prog :   t_instr list;
  struct_fonc :   t_fonc list;  
}



let set_prog_list  d c = {struct_prog = d ; struct_fonc = c}



let rec  print_instr structprog = match structprog with
	| If(ast_a,cond,ast_b)::tl -> print_string("If ");print_terminal(ast_a);print_cond(cond);print_terminal(ast_b); print_instr tl
	| Then::tl-> print_string("Then"); print_instr tl
	| Else::tl->  print_string("ELse"); print_instr tl
	| Print(print)::tl->  print_string(print); print_instr tl
	| [] -> print_string("")

and print_terminal t = match t with 
	| Ident(id) -> print_string(id);
	

and  print_cond  = function
	| Lesser -> print_string("<")
	| Greater ->  print_string(">")
	| Equal -> print_string("=")


let print_prog prog = 
	  print_instr prog.struct_prog;