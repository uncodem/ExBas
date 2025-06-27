type parserstate = { stream : Lexer.token array; indx : int }

let peek { indx; stream } =
    if indx >= Array.length stream then None else Some stream.(indx)

let next st =
    match peek st with
    | Some t -> (Some t, { st with indx = st.indx + 1 })
    | None -> (None, st)

let print_parserstate {stream; indx} =
    let arr = Array.sub stream indx ((Array.length stream) - indx) in
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

type binop = Add | Sub | Mul | Div

type ast_node =
    | Number of int
    | Unary of binop * ast_node
    | Binary of binop * ast_node * ast_node
    | Statement of string * ast_node list
    | Block of ast_node list
    | Call of string * ast_node list
    | Var of string

let string_of_binop = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"

let binop_of_char = function
    | '+' -> Some Add
    | '-' -> Some Sub
    | '*' -> Some Mul
    | '/' -> Some Div
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
        "[" ^ (String.concat " " (List.map string_of_ast stmts)) ^ "]"
    | Var x -> x
    | Call (call, params) ->
        let param_strs = List.map string_of_ast params in
        let body = String.concat " " ( call :: param_strs ) in
        "(" ^ body ^ ")"

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
        let* node, st2' = parse_term st' in
        let* _, st3' = expect_rparen st2' in
        Ok (node, st3')
    | Some (Lexer.BeginBlock _) -> parse_block st'
    | Some (Lexer.Ident (x, _)) -> parse_ident st' x 
    | Some x ->
        Error
          (UnexpectedToken
             (x, "Expected a literal (number or parenthesized expression)."))
    | None -> Error UnexpectedEOF

and parse_ident st ident =
    match peek st with
    | Some (Lexer.LParen _) -> 
        let _, st' = next st in
        let* (param_list, st2') = parse_param_list_loop st' [] in
        let* (_, st3') = expect_rparen st2' in
        Ok (Call (ident, param_list), st3')
    | _ ->
        Ok (Var ident, st)

and parse_block st = 
    let _, st' = next st in (* Consume beginblock *)
    let* stmts, st2' = parse_stmts st' in
    let* _, st3' = expect_endblock st2' in
    Ok (Block stmts, st3')

and parse_unary st =
    match peek st with
    | Some (Lexer.Oper ('-', _)) ->
        let _, st' = next st in
        let* node, st2' = parse_unary st' in
        Ok (Unary (Sub, node), st2')
    | _ -> parse_literal st

and parse_factor st =
    let* left, st' = parse_unary st in
    parse_factor_loop left st'

and parse_factor_loop left st =
    match peek st with
    | Some (Lexer.Oper (c, _)) -> (
        match binop_of_char c with
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
        match binop_of_char c with
        | Some ((Add | Sub) as op) ->
            let _, st' = next st in
            let* right, st2' = parse_factor st' in
            parse_term_loop (Binary (op, left, right)) st2'
        | _ -> Ok (left, st))
    | Some _ -> Ok (left, st)
    | None -> Ok (left, st)

and parse_param_list_loop st acc =
    let* expr, st' = parse_term st in
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

and parse_stmt st = 
    let* ident_tok, st' = expect_ident st in
    let token_name = 
        match ident_tok with
        | Lexer.Ident (name, _) -> name
        | _ -> assert false
    in
    let* param_list, st2' = parse_param_list st' in 
    let* _, st3' = expect_endstmt st2' in
    Ok (Statement (token_name, param_list), st3')

and parse_stmts st =
    let rec parse_stmts_aux st acc =
        match peek st with
        | None -> Ok (List.rev acc, st)
        | Some (Lexer.EndBlock _) -> Ok (List.rev acc, st)
        | Some (Lexer.EndStmt _) -> let _, st' = next st in parse_stmts_aux st' acc
        | Some _ -> 
            let* stmt, st' = parse_stmt st in
            parse_stmts_aux st' (stmt :: acc)
    in
    parse_stmts_aux st []

let parse_all st = 
    let* (stmts, _) = parse_stmts st in
    Ok (stmts)

let parser_report = function
    | UnexpectedEOF -> print_endline "parser: UnexpectedEOF"
    | UnexpectedToken (x, fail_string) ->
        print_endline
          ("parser: UnexpectedToken "
          ^ Lexer.errorstring_of_token x
          ^ ". " ^ fail_string)
