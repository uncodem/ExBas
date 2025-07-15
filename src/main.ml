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
    let tokens = Lexer.lexer_init (read_lines "test.txt") in
    let res = tokens |> Parser.parser_init |> Parser.parse_all in
    match res with
    | Ok a -> (
        match Checker.checker_init a with
        | Ok _ ->
            let (emitted, const_pool) = Emitter.emitter_emit a in
            let raw = Bingen.emit_final emitted const_pool in
            let oc = open_out_bin "out.bin" in
            List.iter (fun x -> Emitter.show_emit_me x |> print_endline) emitted;
            List.iter (output_byte oc) raw;
            close_out oc
            (* List.iter (fun x -> string_of_int x |> print_endline) raw *)
        | Error e -> Checker.checker_report e)
    | Error e -> Parser.parser_report e
