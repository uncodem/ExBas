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

type parser_error = UnexpectedToken of Lexer.token * string | UnexpectedEOF

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

type ast_node =
    | Number of int
    | Unary of binop * ast_node
    | Binary of binop * ast_node * ast_node
    | Statement of string * ast_node list
    | Block of ast_node list
    | Call of string * ast_node list
    | Var of string
    | If of ast_node * ast_node * ast_node option
    | Let of string * ast_node
    | Assign of string * ast_node

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
    | _ -> None

let rec string_of_ast = function
    | Number x -> string_of_int x
    | Binary (a, x, y) ->
        "(" ^ string_of_binop a ^ " " ^ string_of_ast x ^ " " ^ string_of_ast y
        ^ ")"
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


let parser_init toks = { stream = Array.of_list toks; indx = 0 }

let ( let* ) r f =
    match r with
    | Ok x -> f x
    | Error e -> Error e

let rec parse_literal st =
    let tok, st' = next st in
    match tok with
    | Some (Lexer.Number (x, _)) -> Ok (Number x, st')
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
    | _ -> parse_comparison st

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

and parse_stmt st =
    let tok, st' = next st in
    match tok with
    | Some (Lexer.Ident (name, _)) -> 
        let* param_list, st2' = parse_param_list st' in
        let* _, st3' = expect_endstmt st2' in
        Ok (Statement (name, param_list), st3')
    | Some (Lexer.If _) -> parse_if_stmt st'
    | Some (Lexer.Let _) -> parse_let_stmt st'
    | Some t -> Lexer.print_token t; Error (UnexpectedToken (t, "Expected Identifier or If statement."))
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
