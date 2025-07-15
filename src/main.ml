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

let print_labels tbl =
    let print_entry name position = print_endline (name ^ " @ " ^ string_of_int position) in
    Hashtbl.iter print_entry tbl

let () =
    let tokens = Lexer.lexer_init (read_lines "test.txt") in
    List.iter Lexer.print_token tokens;
    print_endline "---";
    let res = tokens |> Parser.parser_init |> Parser.parse_all in
    match res with
    | Ok a -> (
        print_endline (Parser.string_of_ast a);
        match Checker.checker_init a with
        | Ok _ ->
            let emitted = Emitter.emitter_emit a in
            let gen = Bingen.bingen_init emitted in
            List.iter
              (fun x -> print_endline (Emitter.show_emit_me x))
              emitted;
            Bingen.resolve_labels gen;
            print_labels gen.labels
            
        | Error e -> Checker.checker_report e)
    | Error e -> Parser.parser_report e
