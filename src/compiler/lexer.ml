type token_pos = int

type token =
    | Ident of string * token_pos
    | Number of int * token_pos
    | Float of float * token_pos
    | Oper of string * token_pos
    | LParen of token_pos
    | RParen of token_pos
    | Comma of token_pos
    | Illegal of string * token_pos
    | EndStmt of token_pos
    | BeginBlock of token_pos
    | EndBlock of token_pos
    | LBracket of token_pos
    | RBracket of token_pos
    | If of token_pos
    | Then of token_pos
    | Else of token_pos
    | ElseIf of token_pos
    | Let of token_pos
    | Sub of token_pos
    | Colon of token_pos
    | Return of token_pos
    | While of token_pos
    | For of token_pos
    | Step of token_pos
    | String of string * token_pos
    | True of token_pos
    | False of token_pos
    | Goto of token_pos
    | And of token_pos
    | Or of token_pos
    | Yield of token_pos
    | Dim of token_pos
    | SemiStmt of token_pos

type lexerstate = {
    code : string;
    position : int;
    tokens : token list;
    line_number : int;
  }

let lexer_advance { code; position; tokens; line_number } =
    { code; position = position + 1; tokens; line_number }

let add_token { code; position; tokens; line_number } token =
    { code; position; tokens = token :: tokens; line_number }

let peek { code; position; _ } =
    if position < String.length code then Some code.[position] else None

let is_alpha = function
    | '_' | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false

let is_digit = function
    | '0' .. '9' -> true
    | _ -> false

let is_alphanum = function
    | '0' .. '9' -> true
    | c -> is_alpha c

let is_oper = function
    | '+' | '-' | '*' | '/' | '%' -> true
    | '=' | '>' | '<' | '!' -> true
    | _ -> false

let is_special_char = function
    | '(' | ')' | ',' | ':' | '@' -> true
    | _ -> false

let token_of_special_char { line_number; _ } = function
    | '(' -> Some (LParen line_number)
    | ')' -> Some (RParen line_number)
    | ',' -> Some (Comma line_number)
    | ':' -> Some (Colon line_number)
    | '@' -> Some (SemiStmt line_number)
    | _ -> None

let convert_token t =
    match t with
    | Ident (x, pos) -> (
        match x with
        | "if" -> If pos
        | "then" -> Then pos
        | "begin" -> BeginBlock pos
        | "end" -> EndBlock pos
        | "let" -> Let pos
        | "sub" -> Sub pos
        | "else" -> Else pos
        | "elseif" -> ElseIf pos
        | "return" -> Return pos
        | "while" -> While pos
        | "for" -> For pos
        | "step" -> Step pos
        | "true" -> True pos
        | "false" -> False pos
        | "goto" -> Goto pos
        | "and" -> And pos
        | "or" -> Or pos
        | "yield" -> Yield pos
        | "dim" -> Dim pos
        | _ -> t)
    | _ -> t

let print_token = function
    | Ident (s, _) -> print_endline ("Ident(" ^ s ^ ")")
    | Number (x, _) -> print_endline ("Number(" ^ string_of_int x ^ ")")
    | Float (f, _) -> print_endline ("Float(" ^ string_of_float f ^ ")")
    | Oper (x, _) -> print_endline ("Oper(" ^ x ^ ")")
    | Illegal (s, _) -> print_endline ("Illegal(" ^ s ^ ")")
    | LParen _ -> print_endline "LParen"
    | RParen _ -> print_endline "RParen"
    | LBracket _ -> print_endline "LBracket"
    | RBracket _ -> print_endline "RBracket"
    | Comma _ -> print_endline "Comma"
    | EndStmt _ -> print_endline "EndStmt"
    | BeginBlock _ -> print_endline "BeginBlock"
    | EndBlock _ -> print_endline "EndBlock"
    | If _ -> print_endline "If"
    | Then _ -> print_endline "Then"
    | Else _ -> print_endline "Else"
    | Sub _ -> print_endline "Sub"
    | Let _ -> print_endline "Let"
    | Colon _ -> print_endline ":"
    | Return _ -> print_endline "Return"
    | For _ -> print_endline "For"
    | While _ -> print_endline "While"
    | Step _ -> print_endline "Step"
    | String (s, _) -> print_endline ("String(\"" ^ s ^ "\")")
    | True _ -> print_endline "True"
    | False _ -> print_endline "False"
    | Goto _ -> print_endline "Goto"
    | And _ -> print_endline "And"
    | Or _ -> print_endline "Or"
    | Yield _ -> print_endline "Yield"
    | Dim _ -> print_endline "Dim"
    | ElseIf _ -> print_endline "ElseIf"
    | SemiStmt _ -> print_endline "#"

let errorstring_of_token = function
    | Ident (s, line) ->
        "Identifier \"" ^ s ^ "\" of line " ^ string_of_int line
    | Number (x, line) ->
        "Number " ^ string_of_int x ^ " of line " ^ string_of_int line
    | Float (f, line) ->
        "Float " ^ string_of_float f ^ " of line " ^ string_of_int line
    | Oper (x, line) -> "Operator " ^ x ^ " of line " ^ string_of_int line
    | Illegal (s, line) ->
        "Illegal token \"" ^ s ^ "\" of line " ^ string_of_int line
    | LParen line -> "LParen ( of line " ^ string_of_int line
    | RParen line -> "RParen ) of line " ^ string_of_int line
    | LBracket line -> "LBracket [ of line " ^ string_of_int line
    | RBracket line -> "RBracket ] of line " ^ string_of_int line
    | Comma line -> "Comma , of line " ^ string_of_int line
    | EndStmt line -> "EndStmt in line " ^ string_of_int line
    | BeginBlock line -> "Begin of line " ^ string_of_int line
    | EndBlock line -> "EndBlock of line " ^ string_of_int line
    | If line -> "If of line " ^ string_of_int line
    | Then line -> "Then of line " ^ string_of_int line
    | Else line -> "Else of line " ^ string_of_int line
    | Let line -> "Let of line " ^ string_of_int line
    | Sub line -> "Sub of line " ^ string_of_int line
    | Colon line -> "Colon : of line " ^ string_of_int line
    | Return line -> "Return of line " ^ string_of_int line
    | For line -> "For of line " ^ string_of_int line
    | While line -> "While of line " ^ string_of_int line
    | Step line -> "Step of line " ^ string_of_int line
    | String (s, line) -> "String \"" ^ s ^ "\" of line " ^ string_of_int line
    | True line -> "True of line " ^ string_of_int line
    | False line -> "False of line " ^ string_of_int line
    | Goto line -> "Goto of line " ^ string_of_int line
    | And line -> "And of line " ^ string_of_int line
    | Or line -> "Or of line " ^ string_of_int line
    | Yield line -> "Yield of line " ^ string_of_int line
    | Dim line -> "Dim of line " ^ string_of_int line
    | ElseIf line -> "ElseIf of line " ^ string_of_int line
    | SemiStmt line -> "SemiStmt # of line " ^ string_of_int line

let add_token_advance state t = add_token state t |> lexer_advance

let rec lex_none ({ position; line_number; _ } as state) =
    match peek state with
    | Some '\n' ->
        {
          (add_token state (EndStmt line_number)) with
          line_number = line_number + 1;
        }
        |> lexer_advance |> lex_none
    | Some ';' -> add_token_advance state (EndStmt line_number) |> lex_none
    | Some '\'' -> lex_comment state
    | Some '"' -> lex_string (lexer_advance state) (position + 1) 0
    | Some c when c = '[' || c = ']' ->
        add_token_advance state
          (if c = '[' then LBracket line_number else RBracket line_number)
        |> lex_none
    | Some c when Char.code c <= 32 -> lex_none (lexer_advance state)
    | Some c when is_oper c -> lex_oper (lexer_advance state) (String.make 1 c)
    | Some c when is_digit c -> lex_number state 0
    | Some c when is_alpha c -> lex_ident state position 0
    | Some c when is_special_char c -> (
        match token_of_special_char state c with
        | Some t -> lex_none (add_token_advance state t)
        | None -> assert false)
    | Some _ -> lex_illegal state position 0 line_number
    | None -> state.tokens

and lex_string state start len =
    match peek state with
    | Some '"' ->
        lex_none
          (add_token_advance state
             (String (String.sub state.code start len, state.line_number)))
    | None | Some '\n' ->
        lex_none
          (add_token_advance state
             (Illegal ("Unterminated string", state.line_number)))
    | _ -> lex_string (lexer_advance state) start (len + 1)

and lex_comment state =
    match peek state with
    | Some '\n' -> lex_none state
    | _ -> lex_comment (lexer_advance state)

and lex_illegal state start len line_num =
    match peek state with
    | Some c when Char.code c <= 32 ->
        lex_none
          (add_token state
             (Illegal (String.sub state.code start len, line_num)))
    | None ->
        lex_none
          (add_token state
             (Illegal (String.sub state.code start len, line_num)))
    | _ -> lex_illegal (lexer_advance state) start (len + 1) line_num

and lex_ident state start len =
    match peek state with
    | Some c when is_alphanum c ->
        lex_ident (lexer_advance state) start (len + 1)
    | _ ->
        let tok = Ident (String.sub state.code start len, state.line_number) in
        lex_none (add_token state (convert_token tok))

and lex_number state acc =
    match peek state with
    | Some c when is_digit c ->
        let digit = Char.code c - Char.code '0' in
        let new_acc = (acc * 10) + digit in
        lex_number (lexer_advance state) new_acc
    | Some '.' -> (
        let check = lexer_advance state in
        match peek check with
        | Some c when is_digit c ->
            lex_float (lexer_advance state) (string_of_int acc ^ ".")
        | _ -> lex_illegal check check.position 1 check.line_number)
    | _ -> lex_none (add_token state (Number (acc, state.line_number)))

and lex_float state acc =
    match peek state with
    | Some c when is_digit c ->
        let new_acc = acc ^ String.make 1 c in
        lex_float (lexer_advance state) new_acc
    | _ -> (
        match float_of_string_opt acc with
        | Some f -> lex_none (add_token state (Float (f, state.line_number)))
        | None ->
            lex_none
              (add_token state
                 (Illegal ("Invalid float: " ^ acc, state.line_number))))

and lex_oper state acc =
    let next = peek state in
    match acc.[0] with
    | '=' -> (
        match next with
        | Some '=' ->
            lex_none
              (add_token_advance state (Oper (acc ^ "=", state.line_number)))
        | _ -> lex_none (add_token state (Oper (acc, state.line_number))))
    | '!' | '>' | '<' -> (
        match next with
        | Some '=' ->
            lex_none
              (add_token_advance state (Oper (acc ^ "=", state.line_number)))
        | _ -> lex_none (add_token state (Oper (acc, state.line_number))))
    | _ -> lex_none (add_token state (Oper (acc, state.line_number)))

let lexer_init code =
    lex_none { code; position = 0; tokens = []; line_number = 1 } |> List.rev
