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
    | UnexpectedToken
    | UnexpectedEOF

let expect st pred = 
    let (tok, st') = next st in
        match tok with 
            | Some tok when pred tok -> Ok (tok, st')
            | Some _ -> Error UnexpectedToken
            | None -> Error UnexpectedEOF

type binop = 
    | Add
    | Sub
    | Mul
    | Div

type ast_node =
    | Number of int
    | Binary of binop * ast_node * ast_node

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

let parser_init toks =
    {stream = Array.of_list toks; indx = 0}

let ( let* ) r f =
    match r with
        | Ok x -> f x
        | Error e -> Error e

let rec parse_literal st =
    let tok, st' = next st in
    match tok with
        | Some (Lexer.Number x) -> Ok (Number x, st')
        | Some Lexer.LParen ->
            let* (node, st2') = parse_term st' in
            let* (_, st3') = expect st2' (function Lexer.RParen -> true | _ -> false) in
            Ok (node, st3')
    | _ -> Error UnexpectedToken

and parse_factor st = 
    let* (left, st') = parse_literal st in
    parse_factor_loop left st'

and parse_factor_loop left st = 
    match peek st with
        | Some (Oper c) -> 
            (match binop_of_char c with 
                | Some (Mul | Div as op) -> 
                    let _, st' = next st in 
                    let* (right, st2') = parse_literal st' in parse_factor_loop (Binary (op, left, right)) st2'
                | _ -> Ok (left, st))
        | Some _ -> Ok (left, st)
        | None -> Ok (left, st)

and parse_term st = 
    let* (left, st') = parse_factor st in
    parse_term_loop left st'

and parse_term_loop left st = 
    match peek st with
        | Some (Oper c) -> 
            (match binop_of_char c with 
                | Some (Add | Sub as op) -> 
                    let _, st' = next st in 
                    let* (right, st2') = parse_factor st' in parse_term_loop (Binary (op, left, right)) st2'
                | _ -> Ok (left, st))
        | Some _ -> Ok (left, st)
        | None -> Ok (left, st)

let parse_expr = parse_term

let parse_all st =
    let* (node, st') = parse_expr st in
    match peek st' with
        | Some _ -> Error UnexpectedToken
        | None -> Ok node

