let () =
    let res =
        Lexer.lexer_init "random_functoin (12, 12, 32, 41);;;;\n\n\n\n\n\n print (12)\n"
        |> Parser.parser_init |> Parser.parse_all
    in
    match res with
    | Ok a -> List.iter (fun node -> print_endline (Parser.string_of_ast node)) a
    | Error e -> Parser.parser_report e
