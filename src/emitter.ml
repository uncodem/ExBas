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
}

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

let emitter_init () = {
    buffer = [];
    const_counter = 0; 
    const_pool = Hashtbl.create 32; 
    current_scope = 0; 
    vars = [Hashtbl.create 32]; 
    var_counter = [0];
}

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
        emit_node state left;
        emit_node state right;
        emit_val state (RawOp (opcode_of_binop op))
    | Parser.Unary (Parser.Not, right) ->
        emit_val state (RawOp (opcode_of_binop Parser.Not));
        emit_node state right
    | Parser.Unary (Parser.Sub, right) -> 
        emit_node state right;
        emit_val state (RawOp Opcodes.OP_neg)
    | Parser.Let (vname, right, _) ->
        def_var state vname;
        emit_node state right;
        emit_val state (RawOp Opcodes.OP_defvar)
    | Parser.Var vname ->
        let (vindex, scope_idx) = Option.get (find_var state.vars vname 0) in
        emit_val state (RawOp Opcodes.OP_pushvar);
        emit_val state (RawVal scope_idx);
        emit_val state (RawVal vindex)
    | Parser.Index _ ->
        let (vname, indices) = follow_index_chain [] node in
        let (vindex, scope_idx) = Option.get (find_var state.vars vname 0) in
        List.iter (emit_node state) indices;
        (* TODO: VM currently has 6 indexing opcodes, 2 of which handle multidimensional arrays, they haven't been added to the Opcodes module yet. *)
        (* Would need to restructure emitter since 2 of them use compile-time indexing while 4 use runtime. *)
        emit_val state (RawOp Opcodes.OP_pushvar);
        emit_val state (RawVal scope_idx);
        emit_val state (RawVal vindex)

    | Parser.Assign _ -> emit_assign state node 
    | Parser.Goto (lbl, _) ->
        emit_val state (RawOp Opcodes.OP_jmp);
        emit_val state (LabelRef lbl);
        emit_val state NoEmit
    | Parser.Label (name, _) ->
        emit_val state (LabelDef name)

    | _ -> failwith "Unhandled node!"

and emit_assign state = function
    | Parser.Assign (Parser.Var vname, right, _) ->
        let (pos, count) = Option.get ( find_var state.vars vname 0 ) in
        emit_node state right;
        emit_val state (RawOp Opcodes.OP_popvar);
        emit_val state (RawVal count);
        emit_val state (RawVal pos)
    | Parser.Assign (Parser.Index _ as left, right, _) ->
        let (vname, indices) = follow_index_chain [] left in 
        let (pos, scope_idx) = Option.get (find_var state.vars vname 0) in

        List.iter (emit_node state) indices;
        emit_val state (RawOp Opcodes.OP_pushvar);
        emit_val state (RawVal scope_idx);
        emit_val state (RawVal pos);
        emit_node state right;

        (* TODO: While the VM already has the proper opcodes for this, Opcodes module doesn't include it yet. *)

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
