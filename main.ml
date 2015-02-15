(** Main *)
let () =
    if Array.length Sys.argv < 2
      then print_endline "Please provide a BASIC source file as argument" else
      let input_file = Sys.argv.(1) in
        if not (Sys.file_exists input_file)
          then print_endline "This file does not exist" else
          let lexbuf = Lexing.from_channel (open_in input_file) in
          Patterns.main Parser.basic lexbuf
          