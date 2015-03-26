module Operator = struct
type t =
	| PLUS | MINUS | MUL | DIV | Or | And
	| LT | GT | EQ
	| Print | Read
	| Seq | Let | In | If | Branch
	| Func | Param | Eval
	| Cast
let to_string = function
	| PLUS -> "+"
	| MINUS -> "-"
	| MUL -> "*"
	| DIV -> "/"
	| EQ -> "="
	| LT -> "<"
	| GT -> ">"
	| Or -> "or"
	| And -> "and"
	| Print -> "print"
	| Read -> "read"
	| Seq -> "seq"
	| Let -> "let"
	| In -> "in"
	| If -> "if"
	| Branch -> "branch"
	| Func -> "func"
	| Param -> "param"
	| Eval -> "eval"
	| Cast -> "cast"
let print opr = print_string (to_string opr)
let print_endline opr = print opr; print_newline ()
end
module Operand = struct
type t =
	| Void
	| EndParam
	| Stdout
	| Stdin
	| ToString
	| ToInt
	| ToBool
	| String of string
	| INT of int
	| Bool of bool
	| Ident of string
let to_string = function
	| Void -> "void"
	| EndParam -> "end_param"
	| Stdout -> "stdout"
	| Stdin -> "stdin"
	| ToString -> "string"
	| ToInt -> "int"
	| ToBool -> "Bool"
	| String x -> x
	| INT x -> string_of_int x
	| Bool b -> string_of_bool b
	| Ident x -> x
let print opd = print_string (to_string opd)
let print_endline opd = print opd; print_newline ()
end

type t = {data : ast}
and ast = Operand of Operand.t | Operation of Operator.t * t * t

let to_string ast = match ast.data with
	| Operand opd -> Operand.to_string opd
	| Operation (opn, _, _) -> Operator.to_string opn
let print ast =
	let rec print' tab ast = match (tab, ast.data) with
	| (tab, Operand x) -> print_string tab; Operand.print_endline x
	| (tab, Operation (x, y, z)) ->
	print_string tab; Operator.print_endline x;
	print' (tab ^ " ") y;
	print' (tab ^ " ") z
in
print' "" ast
