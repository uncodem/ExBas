type parserstate = {
    stream: Lexer.token array;
    indx: int;
}

let peek {indx; stream} = 
    if indx >= Array.length stream then None
    else Some stream.(indx)

let next st =
    match peek st with
        | Some t -> Some t, {st with indx = st.indx + 1}
        | None -> None, st

type parser_error =
    | UnexpectedToken of Lexer.token
    | UnexpectedEOF

let expect st pred = 
    let (tok, st') = next st in
        match tok with 
            | Some tok when pred tok -> Ok (tok, st')
            | Some x -> Error (UnexpectedToken x)
            | None -> Error UnexpectedEOF

type binop = 
    | Add
    | Sub
    | Mul
    | Div

type ast_node =
    | Number of int
    | Unary of binop * ast_node
    | Binary of binop * ast_node * ast_node
    | Statement of string * ast_node list

let is_number = function
    | Lexer.Number _ -> true
    | _ -> false

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
    | Binary (a, x, y) ->  "(" ^ (string_of_binop a) ^ " " ^ (string_of_ast x) ^ " " ^ (string_of_ast y) ^ ")"
    | Unary (a, x) -> "(" ^ (string_of_binop a) ^ " " ^ (string_of_ast x) ^ ")"
    | Statement (call, params) -> 
        let param_strs = List.map string_of_ast params in
        let body = String.concat " " (call :: param_strs) in 
        "(" ^ body ^ ")"

let parser_init toks =
    {stream = Array.of_list toks; indx = 0}

let ( let* ) r f =
    match r with
        | Ok x -> f x
        | Error e -> Error e

let rec parse_literal st =
    let tok, st' = next st in
    match tok with
        | Some (Lexer.Number (x, _)) -> Ok (Number x, st')
        | Some (Lexer.LParen _) ->
            let* (node, st2') = parse_term st' in
            let* (_, st3') = expect st2' (function Lexer.RParen _ -> true | _ -> false) in
            Ok (node, st3')
        | Some x -> Error (UnexpectedToken x)
        | None -> Error UnexpectedEOF

and parse_unary st =
    match peek st with
        | Some (Lexer.Oper ('-', _)) ->
            let _, st' = next st in 
            let* (node, st2') = parse_unary st' in
            Ok (Unary (Sub, node), st2')
        | _ -> parse_literal st

and parse_factor st = 
    let* (left, st') = parse_unary st in
    parse_factor_loop left st'

and parse_factor_loop left st = 
    match peek st with
        | Some (Lexer.Oper (c, _)) -> 
            (match binop_of_char c with 
                | Some (Mul | Div as op) -> 
                    let _, st' = next st in 
                    let* (right, st2') = parse_unary st' in parse_factor_loop (Binary (op, left, right)) st2'
                | _ -> Ok (left, st))
        | Some _ -> Ok (left, st)
        | None -> Ok (left, st)

and parse_term st = 
    let* (left, st') = parse_factor st in
    parse_term_loop left st'

and parse_term_loop left st = 
    match peek st with
        | Some (Lexer.Oper (c, _)) -> 
            (match binop_of_char c with 
                | Some (Add | Sub as op) -> 
                    let _, st' = next st in 
                    let* (right, st2') = parse_factor st' in parse_term_loop (Binary (op, left, right)) st2'
                | _ -> Ok (left, st))
        | Some _ -> Ok (left, st)
        | None -> Ok (left, st)

let parse_expr = parse_term

let is_ident t =
    match t with
        | Lexer.Ident _ -> true
        | _ -> false

let rec parse_param_list_loop st acc =
    let* (expr, st') = parse_expr st in
    match peek st' with
        | Some (Lexer.Comma _) -> let _, st2' = next st' in parse_param_list_loop st2' (expr :: acc)
        | _ -> Ok ((expr :: acc) |> List.rev, st')

let parse_param_list st = 
    let* (_, st') = expect st (function Lexer.LParen _ -> true | _ -> false) in
    match peek st' with
        | Some (Lexer.RParen _) -> 
            let (_, st2') = next st' in Ok ([], st2')
        | _ -> 
            let* (params, st2') = parse_param_list_loop st' [] in
            let* (_, st3') = expect st2' (function Lexer.RParen _ -> true | _ -> false) in
            Ok (params, st3')

let parse_stmt st = 
    let* (tok, st') = expect st is_ident in
    match tok with
        | Lexer.Ident (token_name, _) -> 
            let* (param_list, st2') = parse_param_list st' in
            Ok ((Statement (token_name, param_list)), st2')
        | _ -> assert false (* Unreachable *)


let parse_all st =
    let* (node, st') = parse_stmt st in
    match peek st' with
        | Some x -> Error (UnexpectedToken x)
        | None -> Ok node

let parser_report = function
    | UnexpectedEOF -> print_endline "parser: UnexpectedEOF"
    | UnexpectedToken x -> 
        print_endline ("parser: UnexpectedToken " ^ (Lexer.errorstring_of_token x))


