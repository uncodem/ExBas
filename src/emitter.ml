type emit_me =
    | RawOp of Opcodes.exbvm_opcode
    | RawVal of int
    | LabelDef of string
    | LabelRef of string
    | NoEmit

type const_value =
    | StrConst of string
    | IntConst of int
    | BoolConst of bool
    | FloatConst of float

type emitter_state = {
    mutable buffer : emit_me list;
    mutable const_counter : int;
    const_pool : (const_value, int) Hashtbl.t;
  }

let emitter_init () =
    { buffer = []; const_counter = 0; const_pool = Hashtbl.create 32 }

let constant_of_node = function
    | Parser.Number x -> IntConst x
    | Parser.String s -> StrConst s
    | Parser.Bool b -> BoolConst b
    | Parser.Float f -> FloatConst f
    | _ -> assert false

let opcode_of_binop = function
    | Parser.Add -> Opcodes.OP_add 
    | Parser.Sub -> Opcodes.OP_sub
    | Parser.Mul -> Opcodes.OP_mul
    | Parser.Div -> Opcodes.OP_div
    | Parser.Mod -> Opcodes.OP_mod
    | Parser.And -> Opcodes.OP_and
    | Parser.Or -> Opcodes.OP_or
    | Parser.Eql -> Opcodes.OP_eql
    | Parser.Neql -> Opcodes.OP_neql
    | Parser.More -> Opcodes.OP_more
    | Parser.Less -> Opcodes.OP_less
    | Parser.EqMore -> Opcodes.OP_eqmore
    | Parser.EqLess -> Opcodes.OP_eqless
    | Parser.Not -> Opcodes.OP_not
    | _ -> assert false

let emit_val state v = state.buffer <- v :: state.buffer

let rec emit_node state node =
    match node with
    | Parser.Number _ | Parser.String _ | Parser.Bool _ | Parser.Float _ ->
        let indx = get_const state node in
        emit_val state (RawOp Opcodes.OP_const);
        emit_val state (RawVal indx);
    | Parser.Binary (op, left, right) -> 
        emit_val state (RawOp (opcode_of_binop op));
        emit_node state left;
        emit_node state right
    | Parser.Unary (Parser.Not, right) ->
        emit_val state (RawOp (opcode_of_binop Parser.Not));
        emit_node state right
    | Parser.Unary (Parser.Sub, right) -> 
        emit_val state (RawOp Opcodes.OP_neg);
        emit_node state right
    | _ -> ()

and get_const state v =
    let k = constant_of_node v in
    match Hashtbl.find_opt state.const_pool k with
    | Some indx -> indx
    | None ->
        let indx = state.const_counter in
        Hashtbl.add state.const_pool k indx;
        state.const_counter <- indx + 1;
        indx
