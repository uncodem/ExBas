type parserstate
type parser_error = UnexpectedToken of Lexer.token * string | UnexpectedEOF | UnexpectedNode of string * Lexer.token_pos

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
    | And 
    | Or

type ast_node =
    | Number of int
    | String of string
    | Bool of bool
    | Float of float
    | Unary of binop * ast_node
    | Binary of binop * ast_node * ast_node
    | Statement of string * ast_node list
    | Block of ast_node list
    | Call of string * ast_node list
    | Index of ast_node * ast_node
    | Var of string
    | If of ast_node * ast_node * ast_node option
    | Let of string * ast_node
    | Assign of string * ast_node
    | Label of string
    | FuncDef of string * string list * ast_node
    | Return of ast_node option
    | While of ast_node * ast_node
    | For of ast_node * ast_node * ast_node * ast_node
    | Goto of string
    | Yield of ast_node
    | Dim of string * ast_node

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
