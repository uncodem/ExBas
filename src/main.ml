let () = 
    Lexer.lexer_init "print 12+4 !asdasd@@@!@SD sad2"
    |> List.iter Lexer.print_token
