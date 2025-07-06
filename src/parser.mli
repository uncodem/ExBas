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
    | Program of ast_node list
    | Number of int
    | String of string
    | Bool of bool 
    | Float of float
    | Unary of binop * ast_node
    | Binary of binop * ast_node * ast_node
    | Statement of string * ast_node list * Lexer.token_pos
    | Block of ast_node list
    | Call of string * ast_node list
    | Index of ast_node * ast_node
    | Var of string
    | If of ast_node * ast_node * ast_node option * Lexer.token_pos option
    | Let of string * ast_node * Lexer.token_pos
    | Assign of ast_node * ast_node * Lexer.token_pos option
    | Label of string * Lexer.token_pos
    | FuncDef of string * (string * string) list * string * ast_node * Lexer.token_pos
    | Return of ast_node option * Lexer.token_pos
    | While of ast_node * ast_node * Lexer.token_pos
    | For of ast_node * ast_node * ast_node * ast_node * Lexer.token_pos
    | Goto of string * Lexer.token_pos
    | Yield of ast_node option * Lexer.token_pos
    | Dim of string * ast_node list * string * Lexer.token_pos

val parser_init : Lexer.token list -> parserstate
(** Generates parserstate from token list *)

val parse_all : parserstate -> (ast_node, parser_error) result
(** Generates AST from provided parserstate *)

val parser_report : parser_error -> unit
(** Prints error message from parser. *)

val string_of_ast : ast_node -> string
(** Stringify ast node *)

val print_parserstate : parserstate -> unit
(** Prints current parser token stream for debugging *)
