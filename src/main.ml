let () = 
    let res = Lexer.lexer_init "(7 + 8) / (2 - 3)" |> Parser.parser_init |> Parser.parse_all in
    (match res with
        | Ok a -> print_endline (Parser.string_of_ast (Parser.Statement ("print", [a])))
        | Error _ -> print_endline "Errored out");
