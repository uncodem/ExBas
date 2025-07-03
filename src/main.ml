let read_lines fname =
    let ic = open_in fname in
    let rec aux acc =
        match input_line ic with
        | line -> aux (line :: acc)
        | exception End_of_file ->
            close_in ic;
            String.concat "\n" (List.rev acc)
    in
    aux [] ^ "\n"

let () =
    let tokens =
        Lexer.lexer_init (read_lines "test.txt")
    in
    List.iter Lexer.print_token tokens;
    print_endline "---";
    let res = tokens |> Parser.parser_init |> Parser.parse_all in
    match res with
    | Ok a -> (
        match Checker.annotate_node a with
        | Ok tnode -> print_endline (Parser.string_of_ast tnode.node)
        | Error _ -> print_endline "Errored out")
    | Error e -> Parser.parser_report e

