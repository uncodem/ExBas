type parserstate = { stream : Lexer.token array; indx : int }

let peek { indx; stream } =
    if indx >= Array.length stream then None else Some stream.(indx)

let next st =
    match peek st with
    | Some t -> (Some t, { st with indx = st.indx + 1 })
    | None -> (None, st)

let print_parserstate { stream; indx } =
    let arr = Array.sub stream indx (Array.length stream - indx) in
    Array.iter Lexer.print_token arr

type parser_error = UnexpectedToken of Lexer.token * string | UnexpectedEOF | UnexpectedNode of string * Lexer.token_pos

let expect st pred fail_string =
    let tok, st' = next st in
    match tok with
    | Some tok when pred tok -> Ok (tok, st')
    | Some x -> Error (UnexpectedToken (x, fail_string))
    | None -> Error UnexpectedEOF

let expect_lparen st =
    expect st
      (function
        | Lexer.LParen _ -> true
        | _ -> false)
      "Expected LParen."

let expect_rparen st =
    expect st
      (function
        | Lexer.RParen _ -> true
        | _ -> false)
      "Expected RParen."

let expect_ident st =
    expect st
      (function
        | Lexer.Ident _ -> true
        | _ -> false)
      "Expected Identifier."

let expect_endstmt st =
    expect st
      (function
        | Lexer.EndStmt _ -> true
        | _ -> false)
      "Expected end of statement."

let expect_beginblock st =
    expect st 
      (function
        | Lexer.BeginBlock _ -> true
        | _ -> false)
      "Expected beginning of block."

let expect_endblock st =
    expect st
      (function
        | Lexer.EndBlock _ -> true
        | _ -> false)
      "Expected end of block."

let expect_else st =
    expect st
        (function 
          | Lexer.Else _ -> true
          | _ -> false)
        "Expected else."

let expect_then st =
    expect st
        (function
          | Lexer.Then _ -> true
          | _ -> false)
        "Expected then."

let expect_eql st =
    expect st 
        (function
          | Lexer.Oper ("=", _) -> true
          | _ -> false)
        "Expected =."

let expect_comma st =
    expect st 
        (function
          | Lexer.Comma _ -> true
          | _ -> false)
        "Expected comma."

let expect_step st =
    expect st
        (function
          | Lexer.Step _ -> true
          | _ -> false)
        "Expected Step."

let expect_colon st =
    expect st 
        (function
          | Lexer.Colon _ -> true 
          | _ -> false)
        "Expected Colon."

let expect_rbracket st =
    expect st
        (function
          | Lexer.RBracket _ -> true
          | _ -> false)
        "Expected right bracket."

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
    | Assign of string * ast_node * Lexer.token_pos option
    | Label of string * Lexer.token_pos
    | FuncDef of string * string list * ast_node * Lexer.token_pos
    | Return of ast_node option * Lexer.token_pos
    | While of ast_node * ast_node * Lexer.token_pos
    | For of ast_node * ast_node * ast_node * ast_node * Lexer.token_pos
    | Goto of string * Lexer.token_pos
    | Yield of ast_node option * Lexer.token_pos
    | Dim of string * ast_node list * string * Lexer.token_pos

let string_of_binop = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Assign -> "="
    | Eql -> "=="
    | Neql -> "!="
    | More -> ">"
    | Less -> "<"
    | EqMore -> ">="
    | EqLess -> "<="
    | And -> "and"
    | Or -> "or"
    | Not -> "!" (* Not necessarily binop, but here anyways *)

let binop_of_str = function
    | "+" -> Some Add
    | "-" -> Some Sub
    | "*" -> Some Mul
    | "/" -> Some Div
    | "=" -> Some Assign
    | "==" -> Some Eql
    | "!=" -> Some Neql
    | ">" -> Some More
    | "<" -> Some Less
    | ">=" -> Some EqMore
    | "<=" -> Some EqLess
    | "!" -> Some Not
    | "and" -> Some And
    | "or" -> Some Or (* Technically not needed here, but for completeness they are *)
    | _ -> None

let rec string_of_ast = function
    | Program b -> 
        let body = String.concat "\n" (List.map string_of_ast b) in
        "(Program " ^ body ^ ")"
    | Number x -> string_of_int x
    | Binary (a, x, y) ->
        "(" ^ string_of_binop a ^ " " ^ string_of_ast x ^ " " ^ string_of_ast y
        ^ ")"
    | Float x -> string_of_float x
    | Unary (a, x) -> "(" ^ string_of_binop a ^ " " ^ string_of_ast x ^ ")"
    | Statement (call, params, _) ->
        let param_strs = List.map string_of_ast params in
        let body = String.concat " " (call :: param_strs) in
        "(" ^ body ^ ")"
    | Block stmts ->
        "[" ^ String.concat " " (List.map string_of_ast stmts) ^ "]"
    | Var x -> x
    | Call (call, params) ->
        let param_strs = List.map string_of_ast params in
        let body = String.concat " " (call :: param_strs) in
        "(" ^ body ^ ")"
    | Index (var, idx) ->
        string_of_ast var ^ "[" ^ string_of_ast idx ^ "]"
    | If (cond, texpr, fexpr, _) -> 
        "(if " ^ string_of_ast cond ^ " " ^ string_of_ast texpr ^ " " ^ (
            match fexpr with
            | Some node -> string_of_ast node
            | None -> ""
        ) ^ ")"
    | Let (left, right, _) -> 
        "(let " ^ left ^ " " ^ string_of_ast right ^ ")"
    | Assign (left, right, _) ->
        "(= " ^ left ^ " " ^ string_of_ast right ^ ")"
    | Label (name, _) -> 
        ":" ^ name
    | FuncDef (name, params, body, _) ->
        let param_list = String.concat " " (name :: params) in
        "(fdef (" ^ param_list ^ ") " ^ string_of_ast body ^ ")"
    | Return (Some x, _) ->
        "(return " ^ string_of_ast x ^ ")"
    | Return (None, _) ->
        "(return)"
    | While (cond, body, _) ->
        "(while " ^ string_of_ast cond ^ " " ^ string_of_ast body ^ ")"
    | For (base, dest, step, body, _) ->
        let cont = List.map string_of_ast [base; dest; step; body] |> String.concat " " in
        "(for " ^ cont ^ ")"
    | String s -> "\"" ^ s ^ "\""
    | Bool true -> "true"
    | Bool false -> "false"
    | Goto (label, _) -> "(goto :" ^ label ^ ")"
    | Yield (body, _) -> "(yield " ^ if Option.is_some body then string_of_ast (Option.get body) else "" ^ ")"
    | Dim (name, lens, annotation, _) -> 
        let lenstring = ( 
            match lens with
            | x :: [] -> string_of_ast x
            | _ :: _ -> String.concat ", " (List.map string_of_ast lens)
            | _ -> assert false) in
        "(dim " ^ name ^ ": " ^ annotation ^ " " ^ lenstring ^ ")"

let parser_init toks = { stream = Array.of_list toks; indx = 0 }

let ( let* ) r f =
    match r with
    | Ok x -> f x
    | Error e -> Error e

let rec parse_literal st =
    let tok, st' = next st in
    match tok with
    | Some (Lexer.Number (x, _)) -> parse_postfix (Number x) st'
    | Some (Lexer.String (x, _)) -> parse_postfix (String x) st'
    | Some (Lexer.Float (x, _)) -> parse_postfix (Float x) st'
    | Some (Lexer.True _) -> parse_postfix (Bool true) st'
    | Some (Lexer.False _) -> parse_postfix (Bool false) st'
    | Some (Lexer.LParen _) ->
        let* node, st2' = parse_expr st' in
        let* _, st3' = expect_rparen st2' in
        parse_postfix node st3'
    | Some (Lexer.BeginBlock _) -> 
        let* node, st2' = parse_block st' in
        parse_postfix node st2'
    | Some (Lexer.Ident (x, _)) -> parse_postfix (Var x) st'
    | Some x ->
        Error
          (UnexpectedToken
             (x, "Expected a literal (number or parenthesized expression)."))
    | None -> Error UnexpectedEOF

and parse_expr st = parse_assignment st

and parse_postfix node st = 
    match peek st with
    | Some (Lexer.LBracket _) ->
        let _, st' = next st in
        let* idx, st2' = parse_expr st' in
        let* _, st3' = expect_rbracket st2' in
        parse_postfix (Index (node, idx)) st3'
    | Some (Lexer.LParen _) ->
        let* args, st' = parse_param_list st in
        parse_postfix (Call ((match node with Var x -> x | _ -> assert false), args)) st'
    | _ -> Ok (node, st)

and parse_block st =
    let* stmts, st' = parse_stmts st true in
    let* _, st2' = expect_endblock st' in
    Ok (Block stmts, st2')

and parse_unary st =
    match peek st with
    | Some (Lexer.Oper ("-", _)) ->
        let _, st' = next st in
        let* node, st2' = parse_unary st' in
        Ok (Unary (Sub, node), st2')
    | Some (Lexer.Oper ("!", _)) ->
        let _, st' = next st in
        let* node, st2' = parse_unary st' in
        Ok (Unary (Not, node), st2')
    | _ -> parse_literal st

and parse_factor st =
    let* left, st' = parse_unary st in
    parse_factor_loop left st'

and parse_factor_loop left st =
    match peek st with
    | Some (Lexer.Oper (c, _)) -> (
        match binop_of_str c with
        | Some ((Mul | Div) as op) ->
            let _, st' = next st in
            let* right, st2' = parse_unary st' in
            parse_factor_loop (Binary (op, left, right)) st2'
        | _ -> Ok (left, st))
    | Some _ -> Ok (left, st)
    | None -> Ok (left, st)

and parse_term st =
    let* left, st' = parse_factor st in
    parse_term_loop left st'

and parse_term_loop left st =
    match peek st with
    | Some (Lexer.Oper (c, _)) -> (
        match binop_of_str c with
        | Some ((Add | Sub) as op) ->
            let _, st' = next st in
            let* right, st2' = parse_factor st' in
            parse_term_loop (Binary (op, left, right)) st2'
        | _ -> Ok (left, st))
    | Some _ -> Ok (left, st)
    | None -> Ok (left, st)

and parse_comparison st =
    let* left, st' = parse_term st in
    parse_comparison_loop left st'

and parse_comparison_loop left st =
    match peek st with
    | Some (Lexer.Oper (c, _)) -> (
        match binop_of_str c with
        | Some ((Eql | Neql | More | Less | EqLess | EqMore) as op) ->
            let _, st' = next st in
            let* right, st2' = parse_term st' in
            parse_term_loop (Binary (op, left, right)) st2'
        | _ -> Ok (left, st))
    | Some _ -> Ok (left, st)
    | None -> Ok (left, st)

and parse_logic_loop left st =
    match peek st with
    | Some (And _) ->
        let _, st' = next st in
        let* right, st2' = parse_comparison st' in
        parse_logic_loop (Binary (And, left, right)) st2'
    | Some (Or _) ->
        let _, st' = next st in
        let* right, st2' = parse_comparison st' in
        parse_logic_loop (Binary (Or, left, right)) st2'
    | Some _ -> Ok (left, st)
    | None -> Ok (left, st)

and parse_logic st =
    let* left, st' = parse_comparison st in
    parse_logic_loop left st'

and parse_if_expr st =
    match peek st with
    | Some (Lexer.If _) ->
        let _, st' = next st in
        let* cond, st2' = parse_expr st' in
        let* _, st3' = expect_then st2' in
        let* texpr, st4' = parse_expr st3' in
        let* _, st5' = expect_else st4' in
        let* fexpr, st6' = parse_expr st5' in
        Ok (If (cond, texpr, Some fexpr, None), st6')
    | _ -> parse_logic st

and parse_assignment st =
    let* left, st' = parse_if_expr st in
    match peek st' with
    | Some (Lexer.Oper ("=", _)) -> ( 
        match left with
        | Var vname ->
            let _, st2' = next st' in
            let* rhs, st3' = parse_assignment st2' in
            Ok (Assign (vname, rhs, None), st3')
        | _ -> 
            Error (UnexpectedToken ((Option.get (peek st')), "Left side of assignment must be variable")))
    | _ -> Ok (left, st')

and parse_if_stmt st =
    let* cond, st' = parse_expr st in
    let* then_tok, st2' = expect_then st' in
    let* texpr, st3' = parse_expr st2' in
    match then_tok with
    | Lexer.Then pos -> (
        match peek st3' with
        | Some (Lexer.Else _) -> 
            let _, st4' = next st3' in
            let* fexpr, st5' = parse_expr st4' in
            Ok (If (cond, texpr, Some fexpr, Some pos), st5')
        | _ -> Ok (If (cond, texpr, None, Some pos), st3'))
    | _ -> assert false

and parse_param_list_loop st acc =
    let* expr, st' = parse_expr st in
    match peek st' with
    | Some (Lexer.Comma _) ->
        let _, st2' = next st' in
        parse_param_list_loop st2' (expr :: acc)
    | _ -> 
        Ok (expr :: acc |> List.rev, st')

and parse_param_list st =
    let* _, st' = expect_lparen st in
    match peek st' with
    | Some (Lexer.RParen _) ->
        let _, st2' = next st' in
        Ok ([], st2')
    | _ ->
        let* params, st2' = parse_param_list_loop st' [] in
        let* _, st3' = expect_rparen st2' in
        Ok (params, st3')

and parse_let_stmt st =
    let* left, st' = expect_ident st in
    match left with
    | Lexer.Ident (ident_name, pos) -> 
        let* _, st2' = expect_eql st' in
        let* right, st3' = parse_expr st2' in
        Ok (Let (ident_name, right, pos), st3')
    | _ -> assert false

and parse_sub_param_loop st acc =
    match peek st with
    | Some (Lexer.Ident (name, _)) ->  
        let _, st' = next st in (
            match peek st' with
            | Some (Lexer.Comma _) ->
                let _, st2' = next st' in
                parse_sub_param_loop st2' (name :: acc)
            | Some (Lexer.RParen _) -> Ok (List.rev (name :: acc), st')
            | None -> Error UnexpectedEOF
            | Some t -> Error (UnexpectedToken (t, "Expected either RParen or Comma")))
    | _ -> Ok (List.rev acc, st)

and parse_sub_param st =
    let* _, st' = expect_lparen st in
    let* (ident_list, st2') = parse_sub_param_loop st' [] in
    let* _, st3' = expect_rparen st2' in
    Ok (ident_list, st3')

and parse_sub st =
    let* ident, st' = expect_ident st in
    match ident with
    | Lexer.Ident (name, pos) -> 
        let* param_list, st2' = parse_sub_param st' in
        let* _, st3' = expect_beginblock st2' in
        let* body, st4' = parse_block st3' in
        Ok (FuncDef (name, param_list, body, pos), st4')
    | _ -> assert false

and parse_while st pos =
    let* cond, st' = parse_expr st in
    let* _, st2' = expect_beginblock st' in
    let* body, st2' = parse_block st2' in
    Ok (While (cond, body, pos), st2')

and parse_for_pred st base pos =
    let* _, st' = expect_comma st in
    let* dest, st2' = parse_expr st' in
    let* _, st3' = expect_step st2' in
    let* step, st4' = parse_expr st3' in
    let* _, st5' = expect_beginblock st4' in
    let* body, st6' = parse_block st5' in
    Ok (For (base, dest, step, body, pos), st6')

and parse_for st pos =
    let* base, st' = parse_expr st in
    match base with
    | Var _ -> parse_for_pred st' base pos
    | Assign (_, _, _) -> parse_for_pred st' base pos
    | _ -> Error (UnexpectedNode ("Expected assignment or var in for loop", pos))

and parse_goto st pos =
    let* _, st' = expect_colon st in
    let* ident, st2' = expect_ident st' in
    match ident with
        | Lexer.Ident (label, _) -> Ok (Goto (label, pos), st2')
        | _ -> assert false

and parse_yield_stmt st pos = 
    match peek st with
    | Some Lexer.EndStmt _ -> 
        let _, st' = next st in
        Ok (Yield (None, pos), st')
    | _ -> 
        let* body, st' = parse_expr st in
        Ok (Yield (Some body, pos), st')

and parse_dim_stmt st pos =
    let* ident, st' = expect_ident st in
    match ident with
    | Lexer.Ident (name, _) -> 
        let* _, st2' = expect_lparen st' in
        let* size, st3' = parse_param_list_loop st2' [] in
        let* _, st4' = expect_rparen st3' in
        let* _, st5' = expect_colon st4' in
        let* ident, st6' = expect_ident st5' in 
        (match ident with
        | Lexer.Ident (tname, _) -> 
            Ok (Dim (name, size, tname, pos), st6')
        | _ -> assert false)
    | _ -> assert false

and parse_stmt st =
    let tok, st' = next st in
    match tok with
    | Some (Lexer.Ident (name, pos)) -> (
        let t = peek st' in
        match t with
        | Some (Lexer.Oper ("=", _)) -> parse_assignment st 
        | Some (Lexer.LParen _) -> 
            let* param_list, st2' = parse_param_list st' in
            let* _, st3' = expect_endstmt st2' in
            Ok (Statement (name, param_list, pos), st3')
        | Some (Lexer.Colon pos) ->
            let _, st2' = next st' in
            Ok (Label (name, pos), st2')
        | _ -> Error (UnexpectedToken (Option.get t, "Expected either assignment, label, or function call")))
    | Some (Lexer.If _) -> parse_if_stmt st'
    | Some (Lexer.Let _) -> parse_let_stmt st'
    | Some (Lexer.Sub _) -> parse_sub st'
    | Some (Lexer.While pos) -> parse_while st' pos
    | Some (Lexer.For pos) -> parse_for st' pos
    | Some (Lexer.Goto pos) -> parse_goto st' pos
    | Some (Lexer.Yield pos) -> parse_yield_stmt st' pos
    | Some (Lexer.Dim pos) -> parse_dim_stmt st' pos
    | Some (Lexer.Return pos) -> (
        match peek st' with
        | Some (Lexer.EndStmt _) -> Ok (Return (None, pos), st')
        | Some _ ->
            let* expr, st2' = parse_expr st' in
            Ok (Return (Some expr, pos), st2')
        | None -> Error UnexpectedEOF)
    | Some t -> Error (UnexpectedToken (t, "Expected statement."))
    | None -> Error UnexpectedEOF

and parse_stmts st blocked =
    let rec parse_stmts_aux st acc =
        match peek st with
        | None -> Ok (List.rev acc, st)
        | Some (Lexer.EndBlock _) when blocked -> Ok (List.rev acc, st)
        | Some (Lexer.EndStmt _) ->
            let _, st' = next st in
            parse_stmts_aux st' acc
        | Some _ ->
            let* stmt, st' = parse_stmt st in
            parse_stmts_aux st' (stmt :: acc)
    in
    parse_stmts_aux st []

let parse_all st =
    let* stmts, _ = parse_stmts st false in
    Ok (Program stmts)

let parser_report = function
    | UnexpectedEOF -> print_endline "parser: UnexpectedEOF"
    | UnexpectedToken (x, fail_string) ->
        print_endline
          ("parser: UnexpectedToken "
          ^ Lexer.errorstring_of_token x
          ^ ". " ^ fail_string)
    | UnexpectedNode (msg, pos) -> print_endline ("parser: UnexpectedNode " ^ msg ^ " in line " ^ string_of_int pos)
