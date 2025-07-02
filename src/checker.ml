type node_type =
    | T_int
    | T_float
    | T_string
    | T_none
    | T_any

type typed_node = {
    kind: node_type;
    node: Parser.ast_node;
}



