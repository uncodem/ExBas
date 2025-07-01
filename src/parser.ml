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
    | Number of int
    | String of string
    | Bool of bool 
    | Float of float
    | Unary of binop * ast_node
    | Binary of binop * ast_node * ast_node
    | Statement of string * ast_node list
    | Block of ast_node list
    | Call of string * ast_node list
    | Var of string
    | If of ast_node * ast_node * ast_node option
    | Let of string * ast_node
    | Assign of string * ast_node
    | Label of string
    | FuncDef of string * string list * ast_node
    | Return of ast_node option
    | While of ast_node * ast_node
    | For of ast_node * ast_node * ast_node * ast_node
    | Goto of string
    | Yield of ast_node
    | Dim of string * ast_node

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
    | Number x -> string_of_int x
    | Binary (a, x, y) ->
        "(" ^ string_of_binop a ^ " " ^ string_of_ast x ^ " " ^ string_of_ast y
        ^ ")"
    | Float x -> string_of_float x
    | Unary (a, x) -> "(" ^ string_of_binop a ^ " " ^ string_of_ast x ^ ")"
    | Statement (call, params) ->
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
    | If (cond, texpr, fexpr) -> 
        "(if " ^ string_of_ast cond ^ " " ^ string_of_ast texpr ^ " " ^ (
            match fexpr with
            | Some node -> string_of_ast node
            | None -> ""
        ) ^ ")"
    | Let (left, right) -> 
        "(let " ^ left ^ " " ^ string_of_ast right ^ ")"
    | Assign (left, right) ->
        "(= " ^ left ^ " " ^ string_of_ast right ^ ")"
    | Label name -> 
        ":" ^ name
    | FuncDef (name, params, body) ->
        let param_list = String.concat " " (name :: params) in
        "(fdef (" ^ param_list ^ ") " ^ string_of_ast body ^ ")"
    | Return (Some x) ->
        "(return " ^ string_of_ast x ^ ")"
    | Return None ->
        "(return)"
    | While (cond, body) ->
        "(while " ^ string_of_ast cond ^ " " ^ string_of_ast body ^ ")"
    | For (base, dest, step, body) ->
        let cont = List.map string_of_ast [base; dest; step; body] |> String.concat " " in
        "(for " ^ cont ^ ")"
    | String s -> "\"" ^ s ^ "\""
    | Bool true -> "true"
    | Bool false -> "false"
    | Goto label -> "(goto :" ^ label ^ ")"
    | Yield body -> "(yield " ^ string_of_ast body ^ ")"
    | Dim (name, len) -> "(dim " ^ name ^ " " ^ string_of_ast len ^ ")"

let parser_init toks = { stream = Array.of_list toks; indx = 0 }

let ( let* ) r f =
    match r with
    | Ok x -> f x
    | Error e -> Error e

let rec parse_literal st =
    let tok, st' = next st in
    match tok with
    | Some (Lexer.Number (x, _)) -> Ok (Number x, st')
    | Some (Lexer.String (x, _)) -> Ok (String x, st')
    | Some (Lexer.Float (x, _)) -> Ok (Float x, st')
    | Some (Lexer.True _) -> Ok (Bool true, st')
    | Some (Lexer.False _) -> Ok (Bool false, st')
    | Some (Lexer.LParen _) ->
        let* node, st2' = parse_expr st' in
        let* _, st3' = expect_rparen st2' in
        Ok (node, st3')
    | Some (Lexer.BeginBlock _) -> parse_block st' 
    | Some (Lexer.Ident (x, _)) -> parse_ident st' x
    | Some x ->
        Error
          (UnexpectedToken
             (x, "Expected a literal (number or parenthesized expression)."))
    | None -> Error UnexpectedEOF

and parse_expr st = parse_assignment st

and parse_ident st ident =
    match peek st with
    | Some (Lexer.LParen _) ->
        let _, st' = next st in
        let* param_list, st2' = parse_param_list_loop st' [] in
        let* _, st3' = expect_rparen st2' in
        Ok (Call (ident, param_list), st3')
    | _ -> Ok (Var ident, st)

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
        Ok (If (cond, texpr, Some fexpr), st6')
    | _ -> parse_logic st

and parse_assignment st =
    let* left, st' = parse_if_expr st in
    match peek st' with
    | Some (Lexer.Oper ("=", _)) -> ( 
        match left with
        | Var vname ->
            let _, st2' = next st' in
            let* rhs, st3' = parse_assignment st2' in
            Ok (Assign (vname, rhs), st3')
        | _ -> 
            Error (UnexpectedToken ((Option.get (peek st')), "Left side of assignment must be variable")))
    | _ -> Ok (left, st')

and parse_if_stmt st =
    let* cond, st' = parse_expr st in
    let* _, st2' = expect_then st' in
    let* texpr, st3' = parse_expr st2' in
    match peek st3' with
    | Some (Lexer.Else _) -> 
        let _, st4' = next st3' in
        let* fexpr, st5' = parse_expr st4' in
        Ok (If (cond, texpr, Some fexpr), st5')
    | _ -> Ok (If (cond, texpr, None), st3')

and parse_param_list_loop st acc =
    let* expr, st' = parse_expr st in
    match peek st' with
    | Some (Lexer.Comma _) ->
        let _, st2' = next st' in
        parse_param_list_loop st2' (expr :: acc)
    | _ -> Ok (expr :: acc |> List.rev, st')

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
    | Lexer.Ident (ident_name, _) -> 
        let* _, st2' = expect_eql st' in
        let* right, st3' = parse_expr st2' in
        Ok (Let (ident_name, right), st3')
    | _ -> assert false

and parse_ident_list_loop st acc =
    match peek st with
    | Some (Lexer.Ident (name, _)) ->  
        let _, st' = next st in (
            match peek st' with
            | Some (Lexer.Comma _) ->
                let _, st2' = next st' in
                parse_ident_list_loop st2' (name :: acc)
            | Some (Lexer.RParen _) -> Ok (List.rev (name :: acc), st')
            | None -> Error UnexpectedEOF
            | Some t -> Error (UnexpectedToken (t, "Expected either RParen or Comma")))
    | _ -> Ok (List.rev acc, st)

and parse_ident_list st =
    let* _, st' = expect_lparen st in
    let* (ident_list, st2') = parse_ident_list_loop st' [] in
    let* _, st3' = expect_rparen st2' in
    Ok (ident_list, st3')

and parse_sub st =
    let* ident, st' = expect_ident st in
    match ident with
    | Lexer.Ident (name, _) -> 
        let* param_list, st2' = parse_ident_list st' in
        let* _, st3' = expect_beginblock st2' in
        let* body, st4' = parse_block st3' in
        Ok (FuncDef (name, param_list, body), st4')
    | _ -> assert false

and parse_while st =
    let* cond, st' = parse_expr st in
    let* _, st2' = expect_beginblock st' in
    let* body, st2' = parse_block st2' in
    Ok (While (cond, body), st2')

and parse_for_pred st base =
    let* _, st' = expect_comma st in
    let* dest, st2' = parse_expr st' in
    let* _, st3' = expect_step st2' in
    let* step, st4' = parse_expr st3' in
    let* _, st5' = expect_beginblock st4' in
    let* body, st6' = parse_block st5' in
    Ok (For (base, dest, step, body), st6')

and parse_for st pos =
    let* base, st' = parse_expr st in
    match base with
    | Var _ -> parse_for_pred st' base 
    | Assign (_, _) -> parse_for_pred st' base 
    | _ -> Error (UnexpectedNode ("Expected assignment or var in for loop", pos))

and parse_goto st =
    let* _, st' = expect_colon st in
    let* ident, st2' = expect_ident st' in
    match ident with
        | Lexer.Ident (label, _) -> Ok (Goto label, st2')
        | _ -> assert false

and parse_yield_stmt st = 
    let* body, st' = parse_expr st in
    Ok (Yield body, st')

and parse_dim_stmt st =
    let* ident, st' = expect_ident st in
    match ident with
    | Lexer.Ident (name, _) -> 
        let* _, st2' = expect_lparen st' in
        let* size, st3' = parse_expr st2' in
        let* _, st4' = expect_rparen st3' in
        Ok (Dim (name, size), st4')
    | _ -> assert false

and parse_stmt st =
    let tok, st' = next st in
    match tok with
    | Some (Lexer.Ident (name, _)) -> (
        let t = peek st' in
        match t with
        | Some (Lexer.Oper ("=", _)) -> parse_assignment st 
        | Some (Lexer.LParen _) -> 
            let* param_list, st2' = parse_param_list st' in
            let* _, st3' = expect_endstmt st2' in
            Ok (Statement (name, param_list), st3')
        | Some (Lexer.Colon _) ->
            let _, st2' = next st' in
            Ok (Label name, st2')
        | _ -> Error (UnexpectedToken (Option.get t, "Expected either assignment, label, or function call")))
    | Some (Lexer.If _) -> parse_if_stmt st'
    | Some (Lexer.Let _) -> parse_let_stmt st'
    | Some (Lexer.Sub _) -> parse_sub st'
    | Some (Lexer.While _) -> parse_while st'
    | Some (Lexer.For pos) -> parse_for st' pos
    | Some (Lexer.Goto _) -> parse_goto st'
    | Some (Lexer.Yield _) -> parse_yield_stmt st'
    | Some (Lexer.Return _) -> (
        match peek st' with
        | Some (Lexer.EndStmt _) -> Ok (Return None, st')
        | Some _ ->
            let* expr, st2' = parse_expr st' in
            Ok (Return (Some expr), st2')
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
    Ok stmts

let parser_report = function
    | UnexpectedEOF -> print_endline "parser: UnexpectedEOF"
    | UnexpectedToken (x, fail_string) ->
        print_endline
          ("parser: UnexpectedToken "
          ^ Lexer.errorstring_of_token x
          ^ ". " ^ fail_string)
    | UnexpectedNode (msg, pos) -> print_endline ("parser: UnexpectedNode " ^ msg ^ " in line " ^ string_of_int pos)
