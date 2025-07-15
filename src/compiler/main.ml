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
    if (Array.length Sys.argv - 1) <> 2 then 
        print_endline ("Usage : " ^ Sys.argv.(0) ^ " <source.exbs> <output.exb>")
    else
        let source = Sys.argv.(1) in
        let outpt = Sys.argv.(2) in
        let contents = read_lines source in
        let res = contents
            |> Lexer.lexer_init |> Parser.parser_init |> Parser.parse_all in
        match res with
        | Ok a -> (
            match Checker.checker_init a with
            | Ok _ -> 
                let (emitted, const_pool) = Emitter.emitter_emit a in
                let raw = Bingen.emit_final emitted const_pool in
                let oc = open_out_bin outpt in
                List.iter (output_byte oc) raw;
                close_out oc
            | Error e -> Checker.checker_report e)
        | Error e -> Parser.parser_report e

