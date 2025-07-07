type node_type =
    | T_int
    | T_float
    | T_string
    | T_bool
    | T_array of node_type
    | T_none
    | T_any

type typed_node = { kind : node_type; node : Parser.ast_node }

type checker_error =
    | MismatchedTypes of node_type * node_type * Lexer.token_pos
    | ExpectedType of node_type * node_type * Lexer.token_pos
    | ExpectedEither of node_type * node_type * node_type * Lexer.token_pos
    | OnlyAllowed of node_type list * Lexer.token_pos
    | InvalidType of string * Lexer.token_pos
    | AnyNotAllowed of Lexer.token_pos
    | DisallowedFuncDef of Lexer.token_pos
    | VarRedefinition of string * Lexer.token_pos
    | LabelRedefinition of string * Lexer.token_pos
    | FuncRedefinition of string * Lexer.token_pos
    | DisallowedYield of Lexer.token_pos
    | UndefinedIdentifier of string * Lexer.token_pos
    | DisallowedReturn of Lexer.token_pos
    | DisallowedTypes of node_type list * Lexer.token_pos
    | IncorrectArity of string * Lexer.token_pos
    | MismatchedFuncArgs of string * node_type list * node_type list * Lexer.token_pos

type checker_state

val checker_init : Parser.ast_node -> (typed_node, checker_error) result
(** Starts the checker with an initialized environment *)

val annotate_node :
  checker_state -> Parser.ast_node -> (typed_node, checker_error) result
(** Annotates all the nodes of an ast recursively *)

val typeof_node : typed_node -> node_type
(** Extracts node type information *)

val nodeof_node : typed_node -> Parser.ast_node
(** Extracts parent ast_node *)

val checker_report : checker_error -> unit
(** Reports checker error *)
