type emit_me =
    | RawOp of Opcodes.exbvm_opcode
    | RawVal of int
    | LabelDef of string
    | LabelRef of string
    | NoEmit
[@@deriving show]

type const_value =
    | StrConst of string
    | IntConst of int
    | BoolConst of bool
    | FloatConst of float
[@@deriving show]

type func_def = Subroutine of string list * Parser.ast_node list

type emitter_state = {
    mutable buffer : emit_me list;
    mutable const_counter : int;
    const_pool : (const_value, int) Hashtbl.t;

    mutable current_scope : int;
    mutable var_counter : int list;
    mutable vars : (string, int) Hashtbl.t list;

    mutable stack_effects : int list;

    mutable block_counter : int;
    mutable block_stack : int list;
}

let add_effect x state =
    match state.stack_effects with
    | [] -> failwith "Attempted to add effect to empty effect stack"
    | hd :: tl -> state.stack_effects <- (hd+x) :: tl

let pop_effect state = 
    match state.stack_effects with
    | [] -> failwith "Attempted to pop empty effect stack"
    | _ :: tl -> state.stack_effects <- tl

let push_effect state = 
    state.stack_effects <- 0 :: state.stack_effects

let zero_effect state =
    match state.stack_effects with
    | [] -> failwith "Attempted to zero empty effect stack"
    | _ :: tl -> state.stack_effects <- 0 :: tl

let rec follow_index_chain acc = function
    | Parser.Index (left, index) -> follow_index_chain (index :: acc) left
    | Parser.Var name -> (name, List.rev acc)
    | _ -> failwith "Not an index chain"

let rec find_var scopes vname count = 
    match scopes with
    | [] -> None
    | hd :: tl -> 
        let res = Hashtbl.find_opt hd vname in
        if Option.is_none res then find_var tl vname (count+1) 
        else Some (Option.get res, count)

let def_var state vname = 
    let scope = List.hd state.vars in
    let count = List.hd state.var_counter in
    Hashtbl.add scope vname count;
    state.var_counter <- (count + 1) :: List.tl state.var_counter

let new_scope state = 
    state.var_counter <- 0 :: state.var_counter;
    state.vars <- Hashtbl.create 32 :: state.vars

let del_scope state = 
    match state.vars, state.var_counter with
    | [], [] | _, [] | [], _ -> failwith "Attempted to delete empty scope stack/vcount stack"
    | _ :: vtl, _ :: vctl ->
        state.var_counter <- vctl;
        state.vars <- vtl

let repeat n x =
    let rec aux acc n = if n = 0 then List.rev acc else aux (x::acc) (n-1) in
    aux [] n

let emitter_init () = {
    buffer = [];
    const_counter = 0; 
    const_pool = Hashtbl.create 32; 
    current_scope = 0; 
    vars = [Hashtbl.create 32]; 
    var_counter = [0];
    stack_effects = [0];
    block_counter = 0;
    block_stack = [];
}

let gen_label prefix id = "@" ^ prefix ^ string_of_int id

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

let drop_effect state =
    let count = List.hd state.stack_effects in
    for _ = 1 to count do
        emit_val state (RawOp Opcodes.OP_drop)
    done

let rec emit_node state node =
    match node with
    | Parser.Number _ | Parser.String _ | Parser.Bool _ | Parser.Float _ ->
        let indx = get_const state node in
        emit_val state (RawOp Opcodes.OP_const);
        emit_val state (RawVal indx);
        add_effect 1 state
    | Parser.Binary (op, left, right) -> 
        emit_node state left;
        emit_node state right;
        emit_val state (RawOp (opcode_of_binop op));
        add_effect (-1) state
    | Parser.Unary (Parser.Not, right) ->
        emit_node state right;
        emit_val state (RawOp (opcode_of_binop Parser.Not))
    | Parser.Unary (Parser.Sub, right) -> 
        emit_node state right;
        emit_val state (RawOp Opcodes.OP_neg)
    | Parser.Let (vname, right, _) ->
        def_var state vname;
        emit_node state right;
        emit_val state (RawOp Opcodes.OP_defvar);
        add_effect (-1) state
    | Parser.Var vname ->
        let (vindex, scope_idx) = Option.get (find_var state.vars vname 0) in
        emit_val state (RawOp Opcodes.OP_pushvar);
        emit_val state (RawVal scope_idx);
        emit_val state (RawVal vindex);
        add_effect 1 state
    | Parser.Index _ ->
        let (vname, indices) = follow_index_chain [] node in
        let (vindex, scope_idx) = Option.get (find_var state.vars vname 0) in
        let depth = List.length indices in
        List.iter (emit_node state) indices;
        emit_val state (RawOp Opcodes.OP_pushvar);
        emit_val state (RawVal scope_idx);
        emit_val state (RawVal vindex);
        add_effect 1 state;
        if depth = 1 then emit_val state (RawOp Opcodes.OP_rget)
        else begin
            emit_val state (RawOp Opcodes.OP_rget_nd);
            emit_val state (RawVal depth)
        end;

    | Parser.Assign _ -> emit_assign state node 
    | Parser.Goto (lbl, _) ->
        emit_val state (RawOp Opcodes.OP_jmp);
        emit_val state (LabelRef lbl);
        emit_val state NoEmit
    | Parser.Label (name, _) ->
        emit_val state (LabelDef name)
    | Parser.Return (expr_opt, _) ->
        if Option.is_some expr_opt then begin
            emit_node state (Option.get expr_opt);
            zero_effect state;
        end else ();
        emit_val state (RawOp Opcodes.OP_endscope);
        emit_val state (RawOp Opcodes.OP_ret)
    | Parser.Yield (expr_opt, _) ->
        if Option.is_some expr_opt then begin
            emit_node state (Option.get expr_opt);
            zero_effect state
        end else ();
        emit_val state (RawOp Opcodes.OP_jmp);
        emit_val state (LabelRef (gen_label "endblock" (List.hd state.block_stack)));
        emit_val state NoEmit
    | Parser.Block _ -> emit_block state node
    | Parser.Program _ -> emit_program state node
    | _ -> failwith "Unhandled node!"

and emit_block state = function
    | Parser.Block stmts ->
        push_effect state;
        state.block_stack <- state.block_counter :: state.block_stack;
        let current = state.block_counter in
        state.block_counter <- state.block_counter + 1;
        emit_val state (RawOp Opcodes.OP_startscope);
        new_scope state;
        let aux x = 
                emit_node state x;
                drop_effect state;
                zero_effect state in
        List.iter aux stmts;
        del_scope state;
        emit_val state (LabelDef (gen_label "endblock" current));
        emit_val state (RawOp Opcodes.OP_endscope);
        state.block_stack <- List.tl state.block_stack;
        pop_effect state;
    | _ -> assert false

and emit_program state = function
    | Parser.Program stmts ->
        let aux x = 
            emit_node state x;
            drop_effect state; 
            zero_effect state in
        List.iter aux stmts;
        emit_val state (RawOp Opcodes.OP_ret)
    | _ -> assert false

and emit_assign state = function
    | Parser.Assign (Parser.Var vname, right, _) ->
        let (pos, count) = Option.get ( find_var state.vars vname 0 ) in
        emit_node state right;
        emit_val state (RawOp Opcodes.OP_popvar);
        emit_val state (RawVal count);
        emit_val state (RawVal pos);
        add_effect (-1) state
    | Parser.Assign (Parser.Index _ as left, right, _) ->
        let (vname, indices) = follow_index_chain [] left in 
        let (pos, scope_idx) = Option.get (find_var state.vars vname 0) in
        let depth = List.length indices in
        emit_val state (RawOp Opcodes.OP_pushvar);
        emit_val state (RawVal scope_idx);
        emit_val state (RawVal pos);

        List.iter (emit_node state) indices;
        emit_node state right;

        (* TODO: While the VM has compile-time indexing opcodes, we'll just use runtime indexing for now. *)
        if depth = 1 then emit_val state (RawOp Opcodes.OP_rset)
        else begin 
            emit_val state (RawOp Opcodes.OP_rset_nd);
            emit_val state (RawVal depth)
        end
            

    | _ -> assert false


and get_const state v =
    let k = constant_of_node v in
    match Hashtbl.find_opt state.const_pool k with
    | Some indx -> indx
    | None ->
        let indx = state.const_counter in
        Hashtbl.add state.const_pool k indx;
        state.const_counter <- indx + 1;
        indx
