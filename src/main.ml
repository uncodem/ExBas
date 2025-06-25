let () = 
    let res = Lexer.lexer_init "12*4-2/2" |> Parser.parser_init |> Parser.parse_all in
    match res with
        | Ok a -> print_endline (Parser.string_of_ast a)
        | Error _ -> print_endline "Errored out"
