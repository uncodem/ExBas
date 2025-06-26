let () =
    let res =
        Lexer.lexer_init "random_functoin (); input (12, 23, [pint (12, 23); output (); ]);"
        |> Parser.parser_init |> Parser.parse_all
    in
    match res with
    | Ok a -> List.iter (fun node -> print_endline (Parser.string_of_ast node)) a
    | Error e -> Parser.parser_report e


