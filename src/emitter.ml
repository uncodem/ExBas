
type emit_me =
    | Raw of int
    | LabelDef of string
    | LabelRef of string
    | NoEmit

type const_value =
    | StrConst of string
    | IntConst of int
    | BoolConst of bool
    | FloatConst of float

type emitter_state = {
    buffer: emit_me list;
    const_counter: int;
    const_pool: (const_value, int) Hashtbl.t;
}

let emitter_init () = {
    buffer = [];
    const_counter = 0;
    const_pool = Hashtbl.create 32
}

let rec emit_node state node =
    match node with
    | Parser.Number x -> 

