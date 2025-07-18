type token_pos = int

type token =
    | Ident of string * token_pos
    | Number of int * token_pos
    | Float of float * token_pos
    | Oper of string * token_pos
    | LParen of token_pos
    | RParen of token_pos
    | Comma of token_pos
    | Illegal of string * token_pos
    | EndStmt of token_pos
    | BeginBlock of token_pos
    | EndBlock of token_pos
    | LBracket of token_pos
    | RBracket of token_pos
    | If of token_pos
    | Then of token_pos
    | Else of token_pos
    | ElseIf of token_pos
    | Let of token_pos
    | Sub of token_pos
    | Colon of token_pos
    | Return of token_pos
    | While of token_pos
    | For of token_pos
    | Step of token_pos
    | String of string * token_pos
    | True of token_pos
    | False of token_pos
    | Goto of token_pos
    | And of token_pos
    | Or of token_pos
    | Yield of token_pos
    | Dim of token_pos
    | SemiStmt of token_pos

val lexer_init : string -> token list
(** Tokenize source code, into list of tokens *)

val convert_token : token -> token
(** Convert Idents into keywords *)

val print_token : token -> unit
(** Print debug representation of token *)

val errorstring_of_token : token -> string
(** Stringifies token for error messages *)
