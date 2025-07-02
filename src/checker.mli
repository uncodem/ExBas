type node_type =
    | T_int
    | T_float
    | T_string
    | T_bool
    | T_array of node_type
    | T_none
    | T_any

type typed_node = {
    kind: node_type;
    node: Parser.ast_node;
}

type checker_error =
    | MismatchedTypes of node_type * node_type
    | ExpectedType of node_type * node_type

val annotate_node : Parser.ast_node -> (typed_node, checker_error) result
(** Annotates all the nodes of an ast recursively *)

val typeof_node : typed_node -> node_type
(** Extracts node type information *)

val nodeof_node : typed_node -> Parser.ast_node
(** Extracts parent ast_node *)
