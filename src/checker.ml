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

(* TODO: Implement stringified errors *)
type checker_error =
    | MismatchedTypes of node_type * node_type
    | ExpectedType of node_type * node_type

let typeof_node {kind; _} = kind
let nodeof_node {node; _} = node

let ( let* ) r f =
    match r with
    | Ok x -> f x
    | Error e -> Error e

let rec eql_types t1 t2 =
    match t1, t2 with
    | T_any, _ | _, T_any -> true
    | T_int, T_int | T_string, T_string | T_float, T_float | T_bool, T_bool -> true
    | T_array x, T_array y -> eql_types x y 
    | _ -> false

let rec iter_result f lst =
    match lst with
    | [] -> Ok []
    | x :: xs ->
        let* _ = f x in
        iter_result f xs

let is_arithmetic_op = function
    | Parser.Add | Parser.Sub | Parser.Mul | Parser.Div -> true
    | _ -> false

let is_logic_op = function
    | Parser.And | Parser.Or -> true
    | _ -> false

let is_comparison_op = function
    | Parser.More | Parser.Less | Parser.EqLess | Parser.EqMore | Parser.Eql | Parser.Neql -> true
    | _ -> false

let rec annotate_node node = 
    match node with
    | Parser.Number _ -> Ok ({ kind = T_int; node })
    | Parser.String _ -> Ok ({ kind = T_string; node })
    | Parser.Bool _ -> Ok ({ kind = T_bool; node})
    | Parser.Float _ -> Ok ({ kind = T_float; node })
    | Parser.Var _ -> Ok ({ kind = T_any; node })
    | Parser.Call (_, params) ->
        let* _ = iter_result annotate_node params in
        Ok ({ kind = T_any; node })
    | Parser.Statement (_, params, _) ->
        let* _ = iter_result annotate_node params in
        Ok ({ kind = T_none; node })
    | Parser.Binary (op, left_node, right_node) ->
        let* left = annotate_node left_node in
        let* right = annotate_node right_node in
        if eql_types (typeof_node left) (typeof_node right) then 
            let node_kind = (
                match op with
                | x when is_arithmetic_op x -> left.kind 
                | x when is_logic_op x -> T_bool
                | x when is_comparison_op x -> T_bool
                | _ -> assert false
            ) in
            Ok ({kind = node_kind; node})
        else
            Error (MismatchedTypes (typeof_node left, typeof_node right))
    | Parser.If _ -> annotate_if node
    | Parser.Assign (_, _, Some _)
    | Parser.For _ | Parser.While _ | Parser.FuncDef _ | Parser.Dim _ | Parser.Let _ 
    | Parser.Goto _ | Parser.Return _ | Parser.Label _ -> Ok ({kind = T_none; node})
    | Parser.Program stmts -> 
        let* _ = iter_result annotate_node stmts in
        Ok ({kind = T_none; node})
    | _ -> Ok ({ kind = T_any; node})

and annotate_if node =
    match node with
    | Parser.If (cond_node, tblock, opt_else, opt_pos) ->
        let if_type = (match opt_pos with Some _ -> T_none | _ -> T_any) in
        let* cond = annotate_node cond_node in
        if not (eql_types cond.kind T_bool) then Error (ExpectedType (T_bool, cond.kind))
        else  
            let* tnode = annotate_node tblock in
            (match opt_else with
            | Some else_node -> 
                let* enode = annotate_node else_node in
                if if_type = T_none then Ok { kind = T_none; node }
                else if eql_types tnode.kind enode.kind then Ok { kind = tnode.kind; node }
                else Error (MismatchedTypes (typeof_node tnode, typeof_node enode))
            | None -> Ok ({kind = if_type; node}))
    | _ -> assert false
