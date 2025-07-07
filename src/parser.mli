type parserstate

type parser_error =
    | UnexpectedToken of Lexer.token * string
    | UnexpectedEOF
    | UnexpectedNode of string * Lexer.token_pos

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
        (** Root node of the AST. List of statements [stmts]*)
    | Number of int  (** Leaf node of type int. *)
    | String of string  (** Leaf node of type string. *)
    | Bool of bool  (** Leaf node of type bool. *)
    | Float of float  (** Leaf node of type float. *)
    | Unary of binop * ast_node
        (** Node for unary expressions (!,-). [op, right]*)
    | Binary of binop * ast_node * ast_node
        (** Node for binary expressions (+,-,*,/, ...) [op, left, right]*)
    | Statement of string * ast_node list * Lexer.token_pos
        (** Node for statement-level function calls. [funcname, params]*)
    | Block of ast_node list
        (** Node for storing statements. Simply a list of statements. [stmts]*)
    | Call of string * ast_node list
        (** Node for expression-level function calls. [funcname, params] *)
    | Index of ast_node * ast_node  (** Indexing node. [left, right] *)
    | Var of string  (** Leaf node for variables. [varname] *)
    | If of ast_node * ast_node * ast_node option * Lexer.token_pos option
        (** Node for ifs. Could either be statement or expression.
            [cond, if, else]*)
    | Let of string * ast_node * Lexer.token_pos
        (** Node for variable declaration. [varname, varval] *)
    | Assign of ast_node * ast_node * Lexer.token_pos option
        (** Node for assignment on Var or Index. Could either be statement or
            expression. [left, right] *)
    | Label of string * Lexer.token_pos
        (** Node for denoting labels. [labelname] *)
    | FuncDef of
        string * (string * string) list * string * ast_node * Lexer.token_pos
        (** Node for function definition. [funcname, paramlist, returntype] *)
    | Return of ast_node option * Lexer.token_pos
        (** Node for denoting returns. [retexpr_opt] *)
    | While of ast_node * ast_node * Lexer.token_pos
        (** Node for denoting while loops. [cond, body] *)
    | For of ast_node * ast_node * ast_node * ast_node * Lexer.token_pos
        (** Node for denoting for loops. [base, dest, step, body] *)
    | Goto of string * Lexer.token_pos
        (** Node for denoting gotos. [labelname] *)
    | Yield of ast_node option * Lexer.token_pos
        (** Node for denoting yields. [yieldexpr_opt] *)
    | Dim of string * ast_node list * string * Lexer.token_pos
        (** Node for denoting dims. [varname, sizes, typing] *)

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
