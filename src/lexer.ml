type token =
    | Ident of string * int * int 
    | Number of int
    | Oper of char
    | Illegal of string * int * int * int

type lexerstate = {
    code: string;
    position: int;
    tokens: token list;
    line_number: int;
}

let lexer_advance {code; position; tokens; line_number} = 
    {code = code; position = position + 1; tokens = tokens; line_number = line_number}

let add_token {code; position; tokens; line_number} token =
    { code = code; position = position; tokens = token :: tokens ; line_number = line_number}

let peek {code; position; _} =
    if position < String.length code then
        Some code.[position]
    else
        None

let is_alpha = function '_' | 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_alphanum = function '0' .. '9' -> true | c -> is_alpha c
let is_oper = function '+' | '-' | '*' | '/' | '%' -> true | _ -> false

let print_token = function
    | Ident (s, x, y) -> print_endline ("Ident(" ^ (String.sub s x y) ^ ")")
    | Number x -> print_endline ("Number(" ^ (string_of_int x) ^ ")")
    | Oper x -> print_endline ("Oper(" ^ (String.make 1 x) ^ ")")
    | Illegal (s, x, y, _) -> print_endline ("Illegal(" ^ (String.sub s x y) ^ ")")

let rec lex_none ({position; line_number; _} as state) =
    match peek state with
        | Some c when Char.code c = 10 ->
            {state with line_number = line_number + 1}
            |> lexer_advance
            |> lex_none
        | Some c when Char.code c <= 32 -> lex_none (lexer_advance state)
        | Some c when is_oper c -> 
            Oper c
            |> add_token state
            |> lexer_advance
            |> lex_none
        | Some c when is_digit c -> lex_number state 0
        | Some c when is_alpha c -> lex_ident state position 0
        | Some _ -> lex_illegal state position 0 line_number 
        | None -> state.tokens

and lex_illegal state start len line_num =
    match peek state with
        | Some c when Char.code c <= 32 -> lex_none (add_token state (Illegal (state.code, start, len, line_num)))
        | None -> lex_none (add_token state (Illegal (state.code, start, len, line_num)))
        | _ -> lex_illegal (lexer_advance state) start (len+1) line_num

and lex_ident state start len =
    match peek state with
        | Some c when is_alphanum c -> lex_ident (lexer_advance state) start (len+1)
        | _ -> lex_none (add_token state (Ident (state.code, start, len)))

and lex_number state acc =
    match peek state with
        | Some c when is_digit c -> 
            let digit = Char.code c - Char.code '0' in
            let new_acc = acc * 10 + digit in
            lex_number (lexer_advance state) new_acc
        | _ ->
            lex_none (add_token state (Number acc))

let lexer_init code = 
    lex_none { code = code; position = 0; tokens = []; line_number = 1} |> List.rev
