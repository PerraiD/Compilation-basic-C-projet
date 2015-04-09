(** Main *)

let parse input_file = 
let lexbuf = Lexing.from_channel (open_in input_file) in
Parser.main Lexer.basic lexbuf


let () =
if Array.length Sys.argv < 2
then print_endline "Please provide a BASIC source file as argument" else
let input_file = Sys.argv.(1) in
	if not (Sys.file_exists input_file)
	then print_endline "This file does not exist" else
	let ast = parse input_file in 
		Arbre.print_prog ast
