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

(* TODO: Implement stringified errors and allow line numbers *)
type checker_error =
    | MismatchedTypes of node_type * node_type * Lexer.token_pos
    | ExpectedType of node_type * node_type * Lexer.token_pos
    | ExpectedEither of node_type * node_type * node_type * Lexer.token_pos
    | OnlyAllowed of node_type list * Lexer.token_pos
    | InvalidType of string * Lexer.token_pos
    | AnyNotAllowed of Lexer.token_pos
    | DisallowedFuncDef of Lexer.token_pos

type checker_state = {
    mutable current_line: Lexer.token_pos;
}

let rec string_of_node_type = function
    | T_int -> "T_int"
    | T_float -> "T_float"
    | T_string -> "T_string"
    | T_bool -> "T_bool"
    | T_any -> "T_any"
    | T_none -> "T_none"
    | T_array x -> "T_array of " ^ string_of_node_type x

(* Function will be used for type annotations, we don't allow arrays and T_none in annotations *)
let node_type_of_string = function
    | "int" -> Some T_int
    | "float" -> Some T_float
    | "string" -> Some T_string
    | "any" -> Some T_any
    | "bool" -> Some T_bool
    | _ -> None

(* let rec gen_arr_typing idx typing =
    match idx with
    | 1 -> typing
    | 0 -> assert false
    | _ -> T_array (gen_arr_typing (idx - 1) typing) *)

let checker_report err = 
    match err with
    | MismatchedTypes (a, b, line) -> 
        print_endline ("checker: MismatchedTypes " ^ string_of_node_type a ^ " and " ^ string_of_node_type b ^ " in line " ^ string_of_int line)
    | ExpectedType (a, b, line) ->
        print_endline ("checker: ExpectedType " ^ string_of_node_type a ^ ", but got " ^ string_of_node_type b ^ " in line " ^ string_of_int line)
    | ExpectedEither (a, b, c, line) ->
        print_endline ("checker: ExpectedEither " ^ string_of_node_type a ^ " or " ^ string_of_node_type b ^ 
            ". Got " ^ string_of_node_type c ^ " in line " ^ string_of_int line)
    | OnlyAllowed (types, line) ->
        let alltypes = String.concat ", " (List.map string_of_node_type types) in
        print_endline ("checker: OnlyAllowed " ^ alltypes ^ ". in line " ^ string_of_int line)
    | AnyNotAllowed line ->
        print_endline ("checker: AnyNotAllowed on line " ^ string_of_int line)
    | InvalidType (typestr, line) ->
        print_endline ("checker: InvalidType '" ^ typestr ^ "' on line " ^ string_of_int line)
    | DisallowedFuncDef line -> 
        print_endline ("checker: DisallowedFuncDef sub definitions are not allowed inside blocks in line " ^ string_of_int line)


let typeof_node {kind; _} = kind
let nodeof_node {node; _} = node

let ( let* ) r f =
    match r with
    | Ok x -> f x
    | Error e -> Error e

let rec eql_types t1 t2 = 
    match t1, t2 with
    | T_any, _ | _, T_any -> true
    | T_array x, T_array y -> eql_types x y
    | _ when t1 = t2 -> true
    | _ -> false

let rec iter_result f lst =
    match lst with
    | [] -> Ok []
    | x :: xs ->
        let* _ = f x in
        iter_result f xs

let rec iter_result_acc f lst acc =
    match lst with
    | [] -> Ok (List.rev acc)
    | hd :: tl ->
        let* r = f hd in
        iter_result_acc f tl (r :: acc)

let is_arithmetic_op = function
    | Parser.Add | Parser.Sub | Parser.Mul | Parser.Div -> true
    | _ -> false

let is_logic_op = function
    | Parser.And | Parser.Or -> true
    | _ -> false

let is_comparison_op = function
    | Parser.More | Parser.Less | Parser.EqLess | Parser.EqMore | Parser.Eql | Parser.Neql -> true
    | _ -> false

let rec decide_if_any atype btype =
    match atype, btype with
    | T_any, x | x, T_any -> x
    | _ when eql_types atype btype -> atype
    | T_array x, T_array y -> T_array (decide_if_any x y)
    | _ -> assert false (* Shouldn't be called if the types are not equivalent *)

let rec annotate_node state node = 
    match node with
    | Parser.Number _ -> Ok ({ kind = T_int; node })
    | Parser.String _ -> Ok ({ kind = T_string; node })
    | Parser.Bool _ -> Ok ({ kind = T_bool; node})
    | Parser.Float _ -> Ok ({ kind = T_float; node })
    | Parser.Var _ -> Ok ({ kind = T_any; node })
    | Parser.Call (_, params) ->
        let* _ = iter_result (annotate_node state) params in
        Ok ({ kind = T_any; node })
    | Parser.Statement (_, params, line) ->
        state.current_line <- line;
        let* _ = iter_result (annotate_node state) params in
        Ok ({ kind = T_none; node })
    | Parser.Binary (op, left_node, right_node) ->
        let* left = annotate_node state left_node in
        let* right = annotate_node state right_node in
        if eql_types (typeof_node left) (typeof_node right) then 
            let bool_op = (is_logic_op op) || (is_comparison_op op) in
            let node_kind = (
                match op with
                | x when is_arithmetic_op x -> decide_if_any (typeof_node left) (typeof_node right)
                | _ when bool_op -> T_bool
                | _ -> assert false
            ) in
            if bool_op && (left.kind = T_any || right.kind = T_any) then Error (AnyNotAllowed state.current_line)
            else Ok ({kind = node_kind; node})
        else
            Error (MismatchedTypes (typeof_node left, typeof_node right, state.current_line))
    | Parser.Unary _ -> annotate_unary state node
    | Parser.If _ -> annotate_if state node
    | Parser.Assign _ -> annotate_assign state node
    | Parser.Let _ -> annotate_let state node
    | Parser.Dim _ -> annotate_dim state node 
    | Parser.Index _ -> annotate_index state node
    | Parser.Block _ -> annotate_block state node
    | Parser.Return (opt_expr, line) -> (
        state.current_line <- line;
        match opt_expr with
        | Some expr -> 
            let* ret_node = annotate_node state expr in
            Ok ({kind = typeof_node ret_node; node}) (* While technically a statement, FuncDef needs this to have a type*)
        | _ -> Ok ({kind = T_none; node}))
    | Parser.Yield (expr, line) ->
        state.current_line <- line;
        let* exnode = annotate_node state expr in
        Ok ({kind = typeof_node exnode; node}) (* While this is a statement node, Block needs this to have a type *)
    | Parser.For _ -> annotate_for state node
    | Parser.While _ -> annotate_while state node 
    | Parser.FuncDef (_, _, body, line) -> 
        state.current_line <- line;
        let* _ = annotate_block state body in
        Ok ({kind = T_none; node})
    | Parser.Goto _ | Parser.Label _ -> Ok ({kind = T_none; node})
    | Parser.Program stmts -> 
        let* _ = iter_result (annotate_node state) stmts in
        Ok ({kind = T_none; node})

and annotate_if state node =
    match node with
    | Parser.If (cond_node, tblock, opt_else, opt_pos) ->
        let if_type = (match opt_pos with Some _ -> T_none | _ -> T_any) in
        (if Option.is_some opt_pos then
            state.current_line <- Option.get opt_pos
            else ());
        let* cond = annotate_node state cond_node in
        if not (eql_types cond.kind T_bool) then Error (ExpectedType (T_bool, cond.kind, state.current_line))
        else  
            let* tnode = annotate_node state tblock in
            (match opt_else with
            | Some else_node -> 
                let* enode = annotate_node state else_node in
                if if_type = T_none then Ok { kind = T_none; node }
                else if eql_types tnode.kind enode.kind then Ok { kind = tnode.kind; node }
                else Error (MismatchedTypes (typeof_node tnode, typeof_node enode, state.current_line))
            | None -> Ok ({kind = if_type; node}))
    | _ -> assert false

and annotate_assign state node =
    match node with
    | Parser.Assign (_, valnode, opt_pos) ->
        (if Option.is_some opt_pos then
            state.current_line <- Option.get opt_pos
            else ());
        let* v = annotate_node state valnode in
        if Option.is_some opt_pos then
            Ok ({kind = T_none; node})
        else 
            Ok ({kind = typeof_node v; node})
    | _ -> assert false

and annotate_let state node = 
    match node with
    | Parser.Let (_, valnode, pos) ->
        state.current_line <- pos;
        let* _ = annotate_node state valnode in
        Ok ({kind = T_none; node})
    | _ -> assert false

and annotate_dim state node =
    match node with
    | Parser.Dim (_, sizes, typestr, pos) -> 
        state.current_line <- pos;
        let* sizenodes = iter_result_acc (annotate_node state) sizes [] in
        let validate acc y = acc && (eql_types y.kind T_int) in
        let is_valid = List.fold_left validate true sizenodes in
        if is_valid then 
            let arrtype = node_type_of_string typestr in
            (match arrtype with
            | Some _ -> Ok ({kind = T_none; node})
            | None -> Error (InvalidType (typestr, state.current_line)))
        else Error (OnlyAllowed ([T_int], state.current_line))
    | _ -> assert false 

and annotate_unary state node =
    match node with 
    | Parser.Unary (op, right) ->
        let u_type = if op = Parser.Not then T_bool else T_int in
        let* rnode = annotate_node state right in
        if eql_types (typeof_node rnode) u_type then Ok ({kind=u_type; node})
        else Error (ExpectedType (u_type, typeof_node rnode, state.current_line))
    | _ -> assert false

and annotate_index state node =
    match node with
    | Parser.Index (var, idx) ->
        let* left = annotate_node state var in
        let* right = annotate_node state idx in
        if eql_types (typeof_node right) T_int then (
            match typeof_node left with
            | T_array x -> Ok ({kind = x; node})
            | T_string -> Ok ({kind = T_int; node}) (* The VM has no char type, it returns an int for whenever a string is indexed *)
            | T_any -> Ok ({kind = T_any; node}) (* Temporary measure *)
            | _ -> Error (ExpectedEither (T_array T_any, T_string, typeof_node left, state.current_line)))
        else Error (ExpectedType (T_int, typeof_node right, state.current_line))
    | _ -> assert false

and annotate_for state node =
    match node with
    | Parser.For (base, dest, step, body, pos) ->
        state.current_line <- pos;
        let* basenode = annotate_node state base in
        let* destnode = annotate_node state dest in
        let* stepnode = annotate_node state step in
        let* bodynode = annotate_node state body in
        let valid_type node = eql_types (typeof_node node) T_int || eql_types (typeof_node node) T_float in
        if valid_type basenode && 
           valid_type destnode && 
           valid_type stepnode then 
           if (typeof_node bodynode) = T_none then Ok ({kind = T_none; node})
           else Error (ExpectedType (T_none, typeof_node bodynode, state.current_line))
        else Error (OnlyAllowed ([T_int; T_float], state.current_line))
    | _ -> assert false 


and annotate_while state node =
    match node with
    | Parser.While (cond, body, pos) ->
        state.current_line <- pos;
        let* condnode = annotate_node state cond in
        let* bodynode = annotate_node state body in
        if eql_types (typeof_node condnode) T_bool then
            if eql_types (typeof_node bodynode) T_none then
                Ok ({kind = T_none; node})
            else Error (ExpectedType (T_none, typeof_node bodynode, state.current_line))
        else Error (ExpectedType (T_bool, typeof_node condnode, state.current_line))
    | _ -> assert false

and annotate_block state node =
    let block_type = ref T_none in
    let aux a =
        match nodeof_node a with
        | Parser.Yield _ -> 
            if !block_type = T_none then 
                let _ = block_type := typeof_node a in Ok ()
            else 
                if eql_types (!block_type) (typeof_node a) then Ok()
                else Error (MismatchedTypes (!block_type, typeof_node a, state.current_line))
        | _ -> Ok () 
    in
    match node with
    | Parser.Block stmts ->
        let nested_fdef = List.exists (function | Parser.FuncDef _ -> true | _ -> false) stmts in
        if nested_fdef then Error (DisallowedFuncDef state.current_line)
        else
        let* tstmts = iter_result_acc (annotate_node state) stmts [] in
        let* _ = iter_result aux tstmts in
        Ok ({kind = !block_type; node})
    | _ -> assert false 

let checker_init ast =
    let state = { current_line = 0; } in
    annotate_node state ast

