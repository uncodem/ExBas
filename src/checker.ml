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
    | MismatchedFuncArgs of
        string * node_type list * node_type list * Lexer.token_pos
    | SyntaxError of string * Lexer.token_pos

type envtype =
    | Subroutine of node_type * node_type list (* return, param types *)
    | Variable of node_type (* type *)

type checker_state = {
    mutable current_line : Lexer.token_pos;
    mutable scopes : (string, envtype) Hashtbl.t list;
    mutable in_block : int;
    labels : string list;
    func_defs : (string, envtype) Hashtbl.t;
    mutable return_type : node_type option;
  }

let ( let* ) r f =
    match r with
    | Ok x -> f x
    | Error e -> Error e

let rec collect_labels node acc =
    match node with
    | Parser.Label (name, line) ->
        if List.mem name acc then Error (LabelRedefinition (name, line))
        else Ok (name :: acc)
    | Parser.While (_, b, _) -> collect_labels b acc
    | Parser.For (_, _, _, b, _) -> collect_labels b acc
    | Parser.FuncDef (_, _, _, b, _) -> collect_labels b acc
    | Parser.Program b | Parser.Block b -> iter_collect_labels b acc
    | _ -> Ok acc

and iter_collect_labels lst acc =
    match lst with
    | [] -> Ok acc
    | hd :: tl ->
        let* acc = collect_labels hd acc in
        iter_collect_labels tl acc

let rec find_var scopes vname =
    match scopes with
    | [] -> None
    | hd :: tl ->
        let res = Hashtbl.find_opt hd vname in
        if Option.is_none res then find_var tl vname else res

let new_scope state = state.scopes <- Hashtbl.create 32 :: state.scopes

let del_scope state =
    match state.scopes with
    | [] -> failwith "checker: Attempted to delete scope with empty scope stack"
    | _ :: tl -> state.scopes <- tl

let rec gen_arr_typing idx typing =
    match idx with
    | 0 -> typing
    | _ -> T_array (gen_arr_typing (idx - 1) typing)

let def_var state name vartype =
    if Option.is_some (find_var state.scopes name) then
      Error (VarRedefinition (name, state.current_line))
    else
      match state.scopes with
      | [] -> failwith "checker: Attempted to define var with empty scope stack"
      | current :: _ ->
          Hashtbl.add current name (Variable vartype);
          Ok ()

let def_func state name rtype params =
    if Hashtbl.mem state.func_defs name then
      Error (FuncRedefinition (name, state.current_line))
    else (
      Hashtbl.add state.func_defs name (Subroutine (rtype, params));
      Ok ())

let rec string_of_node_type = function
    | T_int -> "int"
    | T_float -> "float"
    | T_string -> "string"
    | T_bool -> "bool"
    | T_any -> "any"
    | T_none -> "none"
    | T_array x -> "array of " ^ string_of_node_type x

let rec node_type_of_string = function
    | "int" -> Some T_int
    | "float" -> Some T_float
    | "string" -> Some T_string
    | "any" -> Some T_any
    | "bool" -> Some T_bool
    | "none" -> Some T_none
    | "_" -> None
    | s when s.[0] = '_' -> (
        let child =
            node_type_of_string (String.sub s 1 (String.length s - 1))
        in
        match child with
        | Some x -> Some (T_array x)
        | None -> None)
    | _ -> None

let checker_report err =
    match err with
    | MismatchedTypes (a, b, line) ->
        print_endline
          ("checker: MismatchedTypes " ^ string_of_node_type a ^ " and "
         ^ string_of_node_type b ^ " in line " ^ string_of_int line)
    | ExpectedType (a, b, line) ->
        print_endline
          ("checker: ExpectedType " ^ string_of_node_type a ^ ", but got "
         ^ string_of_node_type b ^ " in line " ^ string_of_int line)
    | ExpectedEither (a, b, c, line) ->
        print_endline
          ("checker: ExpectedEither " ^ string_of_node_type a ^ " or "
         ^ string_of_node_type b ^ ". Got " ^ string_of_node_type c
         ^ " in line " ^ string_of_int line)
    | OnlyAllowed (types, line) ->
        let alltypes =
            String.concat ", " (List.map string_of_node_type types)
        in
        print_endline
          ("checker: OnlyAllowed " ^ alltypes ^ ". in line "
         ^ string_of_int line)
    | AnyNotAllowed line ->
        print_endline ("checker: AnyNotAllowed on line " ^ string_of_int line)
    | InvalidType (typestr, line) ->
        print_endline
          ("checker: InvalidType '" ^ typestr ^ "' on line "
         ^ string_of_int line)
    | DisallowedFuncDef line ->
        print_endline
          ("checker: DisallowedFuncDef sub definitions are not allowed inside \
            blocks in line " ^ string_of_int line)
    | VarRedefinition (vname, line) ->
        print_endline
          ("checker: VarRedefinition redefinition of var " ^ vname ^ " in line "
         ^ string_of_int line)
    | LabelRedefinition (label, line) ->
        print_endline
          ("checker: LabelRedefinition redefinition of label " ^ label
         ^ " in line " ^ string_of_int line)
    | FuncRedefinition (func, line) ->
        print_endline
          ("checker: FuncRedefinition redefinition of sub " ^ func ^ " in line "
         ^ string_of_int line)
    | DisallowedYield line ->
        print_endline
          ("checker: DisallowedYield yield only allowed in blocks in line "
         ^ string_of_int line)
    | UndefinedIdentifier (name, line) ->
        print_endline
          ("checker: UndefinedIdentifier " ^ name ^ " in line "
         ^ string_of_int line)
    | DisallowedReturn line ->
        print_endline
          ("checker: DisallowedReturn return only allowed in function bodies. \
            Line " ^ string_of_int line)
    | DisallowedTypes (types, line) ->
        let types = String.concat " " (List.map string_of_node_type types) in
        print_endline
          ("checker: DisallowedTypes types " ^ types ^ " in line "
         ^ string_of_int line)
    | IncorrectArity (fname, line) ->
        print_endline
          ("checker: IncorrectArity wrong number of arguments in function call \
            for " ^ fname ^ " in line " ^ string_of_int line)
    | MismatchedFuncArgs (fname, expected, got, line) ->
        let etypes =
            String.concat ", " (List.map string_of_node_type expected)
        in
        let gtypes = String.concat ", " (List.map string_of_node_type got) in
        Printf.printf
          "checker: MismatchedFuncArgs in call to %s. Expected (%s), got (%s) \
           in line %d\n"
          fname etypes gtypes line
    | SyntaxError (name, line) ->
        print_endline
          ("checker: SyntaxError. " ^ name ^ " in line " ^ string_of_int line)

let typeof_node { kind; _ } = kind
let nodeof_node { node; _ } = node

let rec eql_types t1 t2 =
    match (t1, t2) with
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
    | Parser.Add | Parser.Sub | Parser.Mul | Parser.Div | Parser.Mod -> true
    | _ -> false

let is_logic_op = function
    | Parser.And | Parser.Or -> true
    | _ -> false

let is_comparison_op = function
    | Parser.More | Parser.Less | Parser.EqLess | Parser.EqMore | Parser.Eql
    | Parser.Neql ->
        true
    | _ -> false

let rec decide_if_any atype btype =
    match (atype, btype) with
    | T_any, x | x, T_any -> x
    | _ when eql_types atype btype -> atype
    | T_array x, T_array y -> T_array (decide_if_any x y)
    | _ ->
        assert false (* Shouldn't be called if the types are not equivalent *)

let validate_base state = function
    | Parser.Assign (Parser.Var _, _, _) | Parser.Var _ -> Ok ()
    | Parser.Assign (Parser.Index _, _, _) | _ ->
        Error
          (SyntaxError
             ( "Assignment only to non-array variables allowed in for loop.",
               state.current_line ))

let rec annotate_node state node =
    match node with
    | Parser.Number _ -> Ok { kind = T_int; node }
    | Parser.String _ -> Ok { kind = T_string; node }
    | Parser.Bool _ -> Ok { kind = T_bool; node }
    | Parser.Float _ -> Ok { kind = T_float; node }
    | Parser.Var vname -> (
        let res = find_var state.scopes vname in
        match res with
        | Some (Variable t) -> Ok { kind = t; node }
        | _ -> Error (UndefinedIdentifier (vname, state.current_line)))
    | Parser.Call _ -> annotate_call state node
    | Parser.Statement (_, _, line) ->
        state.current_line <- line;
        let* _ = annotate_call state node in
        Ok { kind = T_none; node }
    | Parser.Binary (op, left_node, right_node) ->
        let current_line = state.current_line in
        let* left = annotate_node state left_node in
        let* right = annotate_node state right_node in
        if eql_types (typeof_node left) (typeof_node right) then
          let bool_op = is_logic_op op || is_comparison_op op in
          let node_kind =
              match op with
              | x when is_arithmetic_op x ->
                  decide_if_any (typeof_node left) (typeof_node right)
              | _ when bool_op -> T_bool
              | _ -> assert false
          in
          if bool_op && (left.kind = T_any || right.kind = T_any) then
            Error (AnyNotAllowed current_line)
          else Ok { kind = node_kind; node }
        else
          Error
            (MismatchedTypes (typeof_node left, typeof_node right, current_line))
    | Parser.Unary _ -> annotate_unary state node
    | Parser.If _ -> annotate_if state node
    | Parser.Assign _ -> annotate_assign state node
    | Parser.Let _ -> annotate_let state node
    | Parser.Dim _ -> annotate_dim state node
    | Parser.Index _ -> annotate_index state node
    | Parser.Block _ -> annotate_block state node
    | Parser.Return (Some expr, line) -> (
        state.current_line <- line;
        let* rnode = annotate_node state expr in
        match state.return_type with
        | Some ret_type ->
            if eql_types ret_type (typeof_node rnode) then
              Ok { kind = typeof_node rnode; node }
            else
              Error
                (MismatchedTypes
                   (ret_type, typeof_node rnode, state.current_line))
        | None -> Error (DisallowedReturn state.current_line))
    | Parser.Return (None, line) -> (
        state.current_line <- line;
        match state.return_type with
        | Some rtype ->
            if eql_types rtype T_none then Ok { kind = T_none; node }
            else Error (MismatchedTypes (rtype, T_none, state.current_line))
        | None -> Error (DisallowedReturn state.current_line))
    | Parser.Yield (expr, line) ->
        state.current_line <- line;
        if state.in_block = 0 then Error (DisallowedYield state.current_line)
        else if Option.is_some expr then
          let* exnode = annotate_node state (Option.get expr) in
          Ok { kind = typeof_node exnode; node }
          (* While this is a statement node, Block needs this to have a type *)
        else Ok { kind = T_none; node }
    | Parser.For _ -> annotate_for state node
    | Parser.While _ -> annotate_while state node
    | Parser.FuncDef _ -> annotate_funcdef state node
    | Parser.Goto (label, line) ->
        state.current_line <- line;
        if List.mem label state.labels then Ok { kind = T_none; node }
        else Error (UndefinedIdentifier (label, state.current_line))
    | Parser.Program stmts ->
        let* _ = iter_result (annotate_node state) stmts in
        Ok { kind = T_none; node }
    | Parser.Label _ -> Ok { kind = T_none; node }

and annotate_if state node =
    let current_line = state.current_line in
    match node with
    | Parser.If (cond_node, tblock, opt_else, opt_pos) -> (
        let if_type =
            match opt_pos with
            | Some _ -> T_none
            | _ -> T_any
        in
        if Option.is_some opt_pos then state.current_line <- Option.get opt_pos
        else ();
        let* cond = annotate_node state cond_node in
        if not (eql_types cond.kind T_bool) then
          Error (ExpectedType (T_bool, cond.kind, state.current_line))
        else
          let* tnode = annotate_node state tblock in
          match opt_else with
          | Some else_node ->
              let* enode = annotate_node state else_node in
              if if_type = T_none then Ok { kind = T_none; node }
              else if eql_types tnode.kind enode.kind then
                Ok { kind = tnode.kind; node }
              else
                Error
                  (MismatchedTypes
                     (typeof_node tnode, typeof_node enode, current_line))
          | None -> Ok { kind = if_type; node })
    | _ -> assert false

and annotate_assign state node =
    match node with
    | Parser.Assign (left, right, opt_pos) ->
        (match opt_pos with
        | Some line -> state.current_line <- line
        | None -> ());
        let* lhs = annotate_node state left in
        let* rhs = annotate_node state right in
        if eql_types (typeof_node lhs) (typeof_node rhs) then
          if Option.is_some opt_pos then Ok { kind = T_none; node }
          else Ok { kind = decide_if_any lhs.kind rhs.kind; node }
        else Error (MismatchedTypes (lhs.kind, rhs.kind, state.current_line))
    | _ -> assert false

and annotate_let state node =
    match node with
    | Parser.Let (vname, right, pos) ->
        state.current_line <- pos;
        let* var = annotate_node state right in
        let* _ = def_var state vname (typeof_node var) in
        Ok { kind = T_none; node }
    | _ -> assert false

and annotate_dim state node =
    match node with
    | Parser.Dim (name, sizes, typestr, pos) ->
        state.current_line <- pos;
        let* sizenodes = iter_result_acc (annotate_node state) sizes [] in
        let is_valid =
            List.for_all (fun x -> eql_types x.kind T_int) sizenodes
        in
        if is_valid then
          let arrtype = node_type_of_string typestr in
          match arrtype with
          | Some T_none | Some (T_array _) ->
              Error
                (OnlyAllowed
                   ( [ T_int; T_float; T_bool; T_string; T_any ],
                     state.current_line ))
          | Some t ->
              let* _ =
                  def_var state name (gen_arr_typing (List.length sizes) t)
              in
              Ok { kind = T_none; node }
          | None -> Error (InvalidType (typestr, state.current_line))
        else Error (OnlyAllowed ([ T_int ], state.current_line))
    | _ -> assert false

and annotate_unary state node =
    let current_line = state.current_line in
    match node with
    | Parser.Unary (op, right) ->
        let u_type = if op = Parser.Not then T_bool else T_int in
        let* rnode = annotate_node state right in
        if eql_types (typeof_node rnode) u_type then Ok { kind = u_type; node }
        else Error (ExpectedType (u_type, typeof_node rnode, current_line))
    | _ -> assert false

and annotate_index state node =
    let current_line = state.current_line in
    match node with
    | Parser.Index (var, idx) ->
        let* left = annotate_node state var in
        let* right = annotate_node state idx in
        if eql_types (typeof_node right) T_int then
          match typeof_node left with
          | T_array x -> Ok { kind = x; node }
          | T_string ->
              Ok { kind = T_int; node }
              (* The VM has no char type, it returns an int for whenever a string is indexed *)
          | T_any -> Ok { kind = T_any; node }
          | _ ->
              Error
                (ExpectedEither
                   (T_array T_any, T_string, typeof_node left, current_line))
        else Error (ExpectedType (T_int, typeof_node right, current_line))
    | _ -> assert false

and annotate_for state node =
    match node with
    | Parser.For (base, dest, step, body, pos) ->
        state.current_line <- pos;
        let* _ = validate_base state base in
        let* basenode = annotate_node state base in
        let* destnode = annotate_node state dest in
        let* stepnode = annotate_node state step in
        let* bodynode = annotate_node state body in
        let valid_type node =
            eql_types (typeof_node node) T_int
            || eql_types (typeof_node node) T_float
        in
        if valid_type basenode && valid_type destnode && valid_type stepnode
        then
          if typeof_node bodynode = T_none then Ok { kind = T_none; node }
          else
            Error
              (ExpectedType (T_none, typeof_node bodynode, state.current_line))
        else Error (OnlyAllowed ([ T_int; T_float ], state.current_line))
    | _ -> assert false

and annotate_while state node =
    match node with
    | Parser.While (cond, body, pos) ->
        state.current_line <- pos;
        let* condnode = annotate_node state cond in
        let* bodynode = annotate_node state body in
        if eql_types (typeof_node condnode) T_bool then
          if eql_types (typeof_node bodynode) T_none then
            Ok { kind = T_none; node }
          else
            Error
              (ExpectedType (T_none, typeof_node bodynode, state.current_line))
        else
          Error
            (ExpectedType (T_bool, typeof_node condnode, state.current_line))
    | _ -> assert false

and annotate_block state node =
    let block_type = ref T_none in
    state.in_block <- state.in_block + 1;
    let aux a =
        match nodeof_node a with
        | Parser.Yield _ ->
            if !block_type = T_none then
              let _ = block_type := typeof_node a in
              Ok ()
            else if eql_types !block_type (typeof_node a) then Ok ()
            else
              Error
                (MismatchedTypes (!block_type, typeof_node a, state.current_line))
        | _ -> Ok ()
    in
    match node with
    | Parser.Block stmts ->
        new_scope state;
        let nested_fdef =
            List.exists
              (function
                | Parser.FuncDef _ -> true
                | _ -> false)
              stmts
        in
        if nested_fdef then Error (DisallowedFuncDef state.current_line)
        else
          let* tstmts = iter_result_acc (annotate_node state) stmts [] in
          let* _ = iter_result aux tstmts in
          state.in_block <- state.in_block - 1;
          del_scope state;
          Ok { kind = !block_type; node }
    | _ -> assert false

and def_func_params state names types =
    match (names, types) with
    | [], [] -> Ok ()
    | n :: ns, t :: ts ->
        let* _ = def_var state n t in
        def_func_params state ns ts
    | _ -> failwith "def_func_params: Mismatched param and type lengths"
(* Should be impossible *)

and validate_func_types state params ret_type =
    let func_ret = node_type_of_string ret_type in
    let param_types =
        List.map (fun pair -> snd pair |> node_type_of_string) params
    in
    let is_valid_paramt =
        List.for_all
          (function
            | None | Some T_none -> false
            | _ -> true)
          param_types
    in
    if not is_valid_paramt then
      Error (DisallowedTypes ([ T_none ], state.current_line))
    else
      match func_ret with
      | Some t -> Ok (t, List.map Option.get param_types)
      | None -> Error (InvalidType (ret_type, state.current_line))

and annotate_funcbody state node vnames vtypes =
    match node with
    | Parser.Block stmts ->
        new_scope state;
        let nested_fdef =
            List.exists
              (function
                | Parser.FuncDef _ -> true
                | _ -> false)
              stmts
        in
        if nested_fdef then Error (DisallowedFuncDef state.current_line)
        else
          (* We don't update in_body here to prevent yield in the top-level function body *)
          let* _ = def_func_params state vnames vtypes in
          let* _ = iter_result (annotate_node state) stmts in
          del_scope state;
          Ok { kind = T_none; node }
    | _ -> assert false

and annotate_funcdef state node =
    match node with
    | Parser.FuncDef (name, params, ret_type, body, pos) ->
        state.current_line <- pos;
        if Hashtbl.mem state.func_defs name then
          Error (FuncRedefinition (name, pos))
        else
          let* ftype, param_types = validate_func_types state params ret_type in
          state.return_type <- Some ftype;
          let* _ = def_func state name ftype param_types in
          let* _ =
              annotate_funcbody state body (List.map fst params) param_types
          in
          state.return_type <- None;
          Ok { kind = T_none; node }
    | _ -> assert false

and annotate_call state node =
    match node with
    | Parser.Statement (fname, args, _) | Parser.Call (fname, args) ->
        let func = Hashtbl.find_opt state.func_defs fname in
        if Option.is_some func then
          let func = Option.get func in
          match func with
          | Subroutine (ftype, params) ->
              if List.compare_lengths args params <> 0 then
                Error (IncorrectArity (fname, state.current_line))
              else
                let* args = iter_result_acc (annotate_node state) args [] in
                let argtypes = List.map typeof_node args in
                if List.equal eql_types params argtypes then
                  Ok { kind = ftype; node }
                else
                  Error
                    (MismatchedFuncArgs
                       (fname, params, argtypes, state.current_line))
          | _ -> assert false
        else Error (UndefinedIdentifier (fname, state.current_line))
    | _ -> assert false

let builtins =
    [
      ("str2int", T_int, [ T_string ]); ("str2float", T_float, [ T_string ]);
      ("int2str", T_string, [ T_int ]); ("int2float", T_float, [ T_int ]);
      ("float2str", T_string, [ T_float ]); ("float2int", T_int, [ T_float ]);
      ("any2int", T_int, [ T_any ]); ("any2float", T_float, [ T_any ]);
      ("any2str", T_string, [ T_any ]); ("size", T_any, [ T_int ]);
      ("print", T_none, [ T_any ]); ("input", T_string, []);
      ("any", T_any, [ T_any ]);
    ]

let checker_init ast =
    let* collected = collect_labels ast [] in
    List.iter print_endline collected;
    let state =
        {
          current_line = 0;
          scopes = [ Hashtbl.create 32 ];
          in_block = 0;
          labels = collected;
          func_defs = Hashtbl.create 32;
          return_type = None;
        }
    in
    let aux (name, rtype, params) = def_func state name rtype params in
    let* _ = iter_result aux builtins in
    annotate_node state ast
