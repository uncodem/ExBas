type parserstate = {
    stream: Lexer.token array;
    indx: int;
}

let peek {indx; stream} = 
    if indx >= Array.length stream then None
    else Some stream.(indx)

let next st =
    match peek st with
        | Some t -> Some t, {st with indx = st.indx + 1}
        | None -> None, st

type parser_error =
    | UnexpectedToken
    | UnexpectedEOF

let expect st pred = 
    let (tok, st') = next st in
        match tok with 
            | Some tok when pred tok -> Ok (tok, st')
            | Some _ -> Error UnexpectedToken
            | None -> Error UnexpectedEOF

