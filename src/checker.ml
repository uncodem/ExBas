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
    | Parser.Unary _ -> annotate_unary node
    | Parser.If _ -> annotate_if node
    | Parser.Assign _ -> annotate_assign node
    | Parser.Let _ -> annotate_let node
    | Parser.Dim _ -> annotate_dim node
    | Parser.For _ | Parser.While _ | Parser.FuncDef _  
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

and annotate_assign node =
    match node with
    | Parser.Assign (_, valnode, opt_pos) ->
        let* v = annotate_node valnode in
        if Option.is_some opt_pos then
            Ok ({kind = T_none; node})
        else 
            Ok ({kind = typeof_node v; node})
    | _ -> assert false

and annotate_let node = 
    match node with
    | Parser.Let (_, valnode, _) ->
        let* _ = annotate_node valnode in
        Ok ({kind = T_none; node})
    | _ -> assert false

and annotate_dim node =
    match node with
    | Parser.Dim (_, sizenode, _) ->
        let* size = annotate_node sizenode in
        if eql_types (typeof_node size) T_int then Ok({kind = T_none; node})
        else Error (ExpectedType (T_int, typeof_node size))
    | _ -> assert false

and annotate_unary node =
    match node with 
    | Parser.Unary (op, right) ->
        let u_type = if op = Parser.Not then T_bool else T_int in
        let* rnode = annotate_node right in
        if eql_types (typeof_node rnode) u_type then Ok ({kind=u_type; node})
        else Error (ExpectedType (u_type, typeof_node rnode))
    | _ -> assert false

