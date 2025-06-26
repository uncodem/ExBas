
type token_pos = int

type token =
    | Ident of string * token_pos 
    | Number of int * token_pos
    | Oper of char * token_pos
    | LParen of token_pos
    | RParen of token_pos
    | Comma of token_pos
    | Illegal of string * token_pos

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

let is_special_char = function '(' | ')' | ',' -> true | _ -> false

let token_of_special_char state = function 
    | '(' -> Some (LParen state.line_number)
    | ')' -> Some (RParen state.line_number)
    | ',' -> Some (Comma state.line_number)
    | _ -> None


let print_token = function
    | Ident (s, _) -> print_endline ("Ident(" ^ s ^ ")")
    | Number (x, _) -> print_endline ("Number(" ^ (string_of_int x) ^ ")")
    | Oper (x, _) -> print_endline ("Oper(" ^ (String.make 1 x) ^ ")")
    | Illegal (s, _) -> print_endline ("Illegal(" ^ s ^ ")")
    | LParen _ -> print_endline "LParen"
    | RParen _ -> print_endline "RParen"
    | Comma _ -> print_endline "Comma"

let errorstring_of_token = function
    | Ident (s, line) -> "Identifier \"" ^ s ^ "\" of line " ^ (string_of_int line)
    | Number (x, line) -> "Number " ^ (string_of_int x) ^ " of line " ^ (string_of_int line)
    | Oper (x, line) -> "Operator " ^ (String.make 1 x) ^ " of line " ^ (string_of_int line)
    | Illegal (s, line) -> "Illegal token \"" ^ s ^ "\" of line " ^ (string_of_int line)
    | LParen line -> "LParen ( of line " ^ (string_of_int line)
    | RParen line -> "RParen ) of line " ^ (string_of_int line)
    | Comma line -> "Comma , of line " ^ (string_of_int line)

let add_token_advance state t = 
    add_token state t |> lexer_advance

let rec lex_none ({position; line_number; _} as state) =
    match peek state with
        | Some '\n' ->
            {state with line_number = line_number + 1}
            |> lexer_advance
            |> lex_none
        | Some c when Char.code c <= 32 -> lex_none (lexer_advance state)
        | Some c when is_oper c -> lex_none (add_token_advance state (Oper (c, line_number)))
        | Some c when is_digit c -> lex_number state 0
        | Some c when is_alpha c -> lex_ident state position 0
        | Some c when is_special_char c -> 
            (match token_of_special_char state c with
                | Some t -> lex_none (add_token_advance state t)
                | None -> assert false)
        | Some _ -> lex_illegal state position 0 line_number 
        | None -> state.tokens

and lex_illegal state start len line_num =
    match peek state with
        | Some c when Char.code c <= 32 -> lex_none (add_token state (Illegal ((String.sub state.code start len), line_num)))
        | None -> lex_none (add_token state (Illegal ((String.sub state.code start len), line_num)))
        | _ -> lex_illegal (lexer_advance state) start (len+1) line_num

and lex_ident state start len =
    match peek state with
        | Some c when is_alphanum c -> lex_ident (lexer_advance state) start (len+1)
        | _ -> lex_none (add_token state (Ident ((String.sub state.code start len), state.line_number)))

and lex_number state acc =
    match peek state with
        | Some c when is_digit c -> 
            let digit = Char.code c - Char.code '0' in
            let new_acc = acc * 10 + digit in
            lex_number (lexer_advance state) new_acc
        | _ ->
            lex_none (add_token state (Number (acc, state.line_number)))

let lexer_init code = 
    lex_none { code = code; position = 0; tokens = []; line_number = 1} |> List.rev
