type parserstate
type parser_error = UnexpectedToken of Lexer.token * string | UnexpectedEOF

type binop =
    | Add
    | Sub
    | Mul
    | Div
    | Assign
    | Eql
    | Neql
    | More
    | Less
    | EqMore
    | EqLess
    | Not

type ast_node =
    | Number of int
    | Unary of binop * ast_node
    | Binary of binop * ast_node * ast_node
    | Statement of string * ast_node list
    | Block of ast_node list
    | Call of string * ast_node list
    | Var of string
    | If of ast_node * ast_node * ast_node option
    | Let of string * ast_node
    | Assign of string * ast_node
    | Label of string
    | FuncDef of string * string list * ast_node
    | Return of ast_node option

val parser_init : Lexer.token list -> parserstate
(** Generates parserstate from token list *)

val parse_all : parserstate -> (ast_node list, parser_error) result
(** Generates AST from provided parserstate *)

val parser_report : parser_error -> unit
(** Prints error message from parser. *)

val string_of_ast : ast_node -> string
(** Stringify ast node *)

val print_parserstate : parserstate -> unit
(** Prints current parser token stream for debugging *)
