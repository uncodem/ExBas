let () =
  let res =
    Lexer.lexer_init "random_functoin (12, 12, 32, 41)"
    |> Parser.parser_init |> Parser.parse_all
  in
  match res with
  | Ok a -> print_endline (Parser.string_of_ast a)
  | Error e -> Parser.parser_report e
