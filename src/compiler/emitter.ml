type emit_me =
    | RawOp of Opcodes.exbvm_opcode
    | RawValB of int
    | RawValS of int
    | LabelDef of string
    | LabelRef of string
    | NoEmit

type const_value =
    | StrConst of string
    | IntConst of int
    | BoolConst of bool
    | FloatConst of float

type func_def =
    | Subroutine of string list * emit_me list
    | Intrinsic of emit_me list * int
    | Empty

let intrinsics =
    [
      ("print", [ RawOp Opcodes.OP_native; RawValB 0 ], -1);
      ("input", [ RawOp Opcodes.OP_native; RawValB 1 ], 1);
      ("size", [ RawOp Opcodes.OP_size ], 1);
      ("int2str", [ RawOp Opcodes.OP_cast; RawValB 1 ], 0);
      ("float2str", [ RawOp Opcodes.OP_cast; RawValB 1 ], 0);
      ("int2float", [ RawOp Opcodes.OP_cast; RawValB 3 ], 0);
      ("str2float", [ RawOp Opcodes.OP_cast; RawValB 3 ], 0);
      ("str2int", [ RawOp Opcodes.OP_cast; RawValB 0 ], 0);
      ("float2int", [ RawOp Opcodes.OP_cast; RawValB 0 ], 0); ("any", [], 0);
      ("str2any", [], 0); ("int2any", [], 0); ("float2any", [], 0);
      ("any2str", [RawOp Opcodes.OP_cast; RawValB 1], 0);
      ("any2int", [RawOp Opcodes.OP_cast; RawValB 0], 0);
      ("any2float", [RawOp Opcodes.OP_cast; RawValB 3], 0);
      ("any2bool", [RawOp Opcodes.OP_cast; RawValB 2], 0);
      ("copy", [RawOp Opcodes.OP_copy], 1);
    ]

type emitter_state = {
    mutable buffer : emit_me list;
    mutable const_counter : int;
    const_pool : (const_value, int) Hashtbl.t;
    mutable const_history : const_value list;
    mutable current_scope : int;
    mutable var_counter : int list;
    mutable vars : (string, int) Hashtbl.t list;
    mutable stack_effects : int list;
    mutable block_counter : int;
    mutable block_stack : int list;
    mutable if_counter : int;
    mutable while_counter : int;
    mutable for_counter : int;
    func_table : (string, func_def) Hashtbl.t;
  }

let push x stack = x :: stack
let top = List.hd

let pop = function
    | _ :: tl -> tl
    | [] -> failwith "Tried to pop empty stack"

let peek = function
    | hd :: _ -> hd
    | [] -> failwith "Tried to peek empty stack"

let apply_top f = function
    | hd :: tl -> f hd :: tl
    | [] -> failwith "Tried to apply empty stack"

let add_effect x state =
    match state.stack_effects with
    | [] -> failwith "Attempted to add effect to empty effect stack"
    | hd :: tl -> state.stack_effects <- (hd + x) :: tl

let pop_effect state =
    match state.stack_effects with
    | [] -> failwith "Attempted to pop empty effect stack"
    | _ :: tl -> state.stack_effects <- tl

let push_effect state = state.stack_effects <- 0 :: state.stack_effects

let zero_effect state =
    match state.stack_effects with
    | [] -> failwith "Attempted to zero empty effect stack"
    | _ :: tl -> state.stack_effects <- 0 :: tl

let rec follow_index_chain acc = function
    | Parser.Index (left, index) -> follow_index_chain (index :: acc) left
    | Parser.Var _ as v -> (v, List.rev acc)
    | _ -> failwith "Not an index chain"

let rec find_var scopes vname count =
    match scopes with
    | [] -> None
    | hd :: tl ->
        let res = Hashtbl.find_opt hd vname in
        if Option.is_none res then find_var tl vname (count + 1)
        else Some (Option.get res, count)

let def_var state vname =
    let scope = peek state.vars in
    let count = peek state.var_counter in
    Hashtbl.add scope vname count;
    state.var_counter <- apply_top (( + ) 1) state.var_counter

let new_scope state =
    state.var_counter <- push 0 state.var_counter;
    state.vars <- push (Hashtbl.create 32) state.vars

let del_scope state =
    match (state.vars, state.var_counter) with
    | [], [] | _, [] | [], _ ->
        failwith "Attempted to delete empty scope stack/vcount stack"
    | _ :: vtl, _ :: vctl ->
        state.var_counter <- vctl;
        state.vars <- vtl

let rec get_vname = function
    | Parser.Var vname -> vname
    | Parser.Assign (left, _, _) -> get_vname left
    | _ -> assert false

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

let emit_stash_value state =
    let stash_idx, global_idx = Option.get (find_var state.vars "@stash" 0) in
    emit_val state (RawOp Opcodes.OP_dup);
    emit_val state (RawOp Opcodes.OP_popvar);
    emit_val state (RawValB global_idx);
    emit_val state (RawValB stash_idx)

let emit_load_stash state =
    let stash_idx, global_idx = Option.get (find_var state.vars "@stash" 0) in
    emit_val state (RawOp Opcodes.OP_pushvar);
    emit_val state (RawValB global_idx);
    emit_val state (RawValB stash_idx);
    add_effect 1 state

let drop_effect state =
    let count = peek state.stack_effects in
    for _ = 1 to count do
      emit_val state (RawOp Opcodes.OP_drop)
    done

let clear_effect state =
    drop_effect state;
    zero_effect state

let get_const state v =
    let k = constant_of_node v in
    match Hashtbl.find_opt state.const_pool k with
    | Some indx -> indx
    | None ->
        let indx = state.const_counter in
        Hashtbl.add state.const_pool k indx;
        state.const_history <- k :: state.const_history;
        state.const_counter <- indx + 1;
        indx

let rec emit_node state node =
    match node with
    | Parser.Number _ | Parser.String _ | Parser.Bool _ | Parser.Float _ ->
        let indx = get_const state node in
        emit_val state (RawOp Opcodes.OP_const);
        emit_val state (RawValB indx);
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
        let vindex, scope_idx = Option.get (find_var state.vars vname 0) in
        emit_val state (RawOp Opcodes.OP_pushvar);
        emit_val state (RawValB scope_idx);
        emit_val state (RawValB vindex);
        add_effect 1 state
    | Parser.Index _ ->
        let vnode, indices = follow_index_chain [] node in
        let depth = List.length indices in
        emit_node state vnode;
        List.iter (emit_node state) indices;
        add_effect (-1) state;
        if depth = 1 then emit_val state (RawOp Opcodes.OP_rget)
        else (
          emit_val state (RawOp Opcodes.OP_rget_nd);
          emit_val state (RawValB depth))
    | Parser.Assign _ -> emit_assign state node
    | Parser.Goto (lbl, _) ->
        emit_val state (RawOp Opcodes.OP_jmp);
        emit_val state (LabelRef lbl);
        emit_val state NoEmit
    | Parser.Label (name, _) -> emit_val state (LabelDef name)
    | Parser.Return (expr_opt, _) ->
        if Option.is_some expr_opt then (
          emit_node state (Option.get expr_opt);
          zero_effect state)
        else ();
        emit_val state (RawOp Opcodes.OP_endscope);
        emit_val state (RawOp Opcodes.OP_ret)
    | Parser.Yield (expr_opt, _) ->
        if Option.is_some expr_opt then (
          emit_node state (Option.get expr_opt);
          zero_effect state)
        else ();
        emit_val state (RawOp Opcodes.OP_jmp);
        emit_val state
          (LabelRef (gen_label "endblock" (peek state.block_stack)));
        emit_val state NoEmit
    | Parser.Block _ -> emit_block state node
    | Parser.Program _ -> emit_program state node
    | Parser.If _ -> emit_if state node
    | Parser.While _ -> emit_while state node
    | Parser.Dim _ -> emit_dim state node
    | Parser.For _ -> emit_for state node
    | Parser.Call _ | Parser.Statement _ -> emit_call state node
    | Parser.FuncDef _ -> emit_funcdef state node
    | _ -> failwith "Unhandled node!"

and emit_block state = function
    | Parser.Block stmts ->
        push_effect state;
        let current = state.block_counter in
        state.block_stack <- push current state.block_stack;
        state.block_counter <- state.block_counter + 1;
        emit_val state (RawOp Opcodes.OP_startscope);
        new_scope state;
        let aux x =
            emit_node state x;
            clear_effect state
        in
        List.iter aux stmts;
        del_scope state;
        emit_val state (LabelDef (gen_label "endblock" current));
        emit_val state (RawOp Opcodes.OP_endscope);
        state.block_stack <- pop state.block_stack;
        pop_effect state
    | _ -> assert false

and emit_program state = function
    | Parser.Program stmts ->
        let aux x =
            emit_node state x;
            clear_effect state
        in
        List.iter aux stmts;
        emit_val state (RawOp Opcodes.OP_ret)
    | _ -> assert false

and emit_assign state = function
    | Parser.Assign (Parser.Var vname, right, isstmt_opt) ->
        let pos, count = Option.get (find_var state.vars vname 0) in 
        emit_node state right; (* +1 *)
        if Option.is_none isstmt_opt then (
            emit_val state (RawOp Opcodes.OP_dup); (* +1 *)
            add_effect (+1) state;
        )
        else ();

        add_effect (-1) state;
        emit_val state (RawOp Opcodes.OP_popvar); (* -1 *)
        emit_val state (RawValB count);
        emit_val state (RawValB pos)
    | Parser.Assign ((Parser.Index _ as left), right, isstmt_opt) ->
        let vnode, indices = follow_index_chain [] left in
        let depth = List.length indices in
        let is_expr = Option.is_none isstmt_opt in

        emit_node state vnode;
        List.iter (emit_node state) indices;
        emit_node state right;

        if is_expr then emit_stash_value state else ();

        (* TODO: While the VM has compile-time indexing opcodes, we'll just use runtime indexing for now. *)
        if depth = 1 then emit_val state (RawOp Opcodes.OP_rset)
        else (
          emit_val state (RawOp Opcodes.OP_rset_nd);
          emit_val state (RawValB depth));
        add_effect (-depth - 2) state;
        if is_expr then emit_load_stash state else ()
    | _ -> assert false

and emit_if state = function
    | Parser.If (cond, tarm, Some farm, pos) ->
        let counter = state.if_counter in
        state.if_counter <- state.if_counter + 1;
        emit_node state cond;
        emit_val state (RawOp Opcodes.OP_tjmp);
        add_effect (-1) state;
        emit_val state (LabelRef (gen_label "iftrue" counter));
        emit_val state NoEmit;
        emit_node state farm;
        if Option.is_some pos then clear_effect state 
        else ();
        emit_val state (RawOp Opcodes.OP_jmp);
        emit_val state (LabelRef (gen_label "ifend" counter));
        emit_val state NoEmit;
        emit_val state (LabelDef (gen_label "iftrue" counter));
        emit_node state tarm;
        if Option.is_some pos then clear_effect state 
        else ();
        emit_val state (LabelDef (gen_label "ifend" counter));
        if Option.is_none pos then zero_effect state else ()
    | Parser.If (cond, tarm, None, _) ->
        let counter = state.if_counter in
        state.if_counter <- state.if_counter + 1;
        emit_node state cond;
        emit_val state (RawOp Opcodes.OP_not);
        emit_val state (RawOp Opcodes.OP_tjmp);
        add_effect (-1) state;
        emit_val state (LabelRef (gen_label "ifend" counter));
        emit_val state NoEmit;
        emit_node state tarm;
        clear_effect state;
        emit_val state (LabelDef (gen_label "ifend" counter))
        (* We don't check if we zero_effect here since if-exprs always have else arms. *)
    | _ -> assert false

and emit_while state = function
    | Parser.While (cond, body, _) ->
        let counter = state.while_counter in
        state.while_counter <- state.while_counter + 1;
        emit_val state (LabelDef (gen_label "whilecond" counter));
        emit_node state cond;
        emit_val state (RawOp Opcodes.OP_not);
        emit_val state (RawOp Opcodes.OP_tjmp);
        add_effect (-1) state;
        emit_val state (LabelRef (gen_label "whileend" counter));
        emit_val state NoEmit;
        emit_node state body;
        emit_val state (RawOp Opcodes.OP_jmp);
        emit_val state (LabelRef (gen_label "whilecond" counter));
        emit_val state NoEmit;
        emit_val state (LabelDef (gen_label "whileend" counter))
    | _ -> assert false

and emit_dim state = function
    | Parser.Dim (vname, sizes, _, _) ->
        (* Typing is ignored because the VM allows arrays to contain anything, it's more for the typechecker *)
        let sizes =
            List.map
              (function
                | Parser.Number x -> x
                | _ -> assert false)
              sizes
        in
        (* Weirdly enough, the parser restricts sizes to Number nodes only *)
        let depth = List.length sizes in
        def_var state vname;
        if depth = 1 then (
          emit_val state (RawOp Opcodes.OP_createarray);
          emit_val state (RawValS (top sizes)))
        else (
          emit_val state (RawOp Opcodes.OP_createarray_nd);
          emit_val state (RawValB depth);
          List.iter (fun x -> emit_val state (RawValS x)) sizes);
        emit_val state (RawOp Opcodes.OP_defvar)
    | _ -> assert false

and emit_for state = function
    | Parser.For (base, dest, step, body, _) ->
        let counter = state.for_counter in
        state.for_counter <- state.for_counter + 1;
        let vname = get_vname base in
        let dest_name = gen_label "fordest" counter in
        let step_name = gen_label "forstep" counter in
        def_var state dest_name;
        def_var state step_name;
        let (destpos, destscope) = Option.get (find_var state.vars dest_name 0) in

        let step_node =
            Parser.Assign
              ( Parser.Var vname,
                Parser.Binary (Parser.Add, Parser.Var vname, Parser.Var step_name),
                None )
        in
        emit_node state base;

        emit_node state dest;
        emit_val state (RawOp Opcodes.OP_defvar);

        emit_node state step;
        emit_val state (RawOp Opcodes.OP_defvar);

        emit_val state (LabelDef (gen_label "forcond" counter));

        emit_val state (RawOp Opcodes.OP_pushvar);
        emit_val state (RawValB destscope);
        emit_val state (RawValB destpos);

        emit_val state (RawOp Opcodes.OP_eql);
        emit_val state (RawOp Opcodes.OP_tjmp);
        add_effect (-2) state;
        emit_val state (LabelRef (gen_label "forend" counter));
        emit_val state NoEmit;
        emit_node state body;
        emit_node state step_node;

        emit_val state (RawOp Opcodes.OP_jmp);
        emit_val state (LabelRef (gen_label "forcond" counter));
        emit_val state NoEmit;
        emit_val state (LabelDef (gen_label "forend" counter));
        add_effect (-2) state;
    | _ -> assert false

and emit_call state = function
    | Parser.Call (fname, params) | Parser.Statement (fname, params, _) -> (
        let param_count = List.length params in
        List.iter (emit_node state) params;
        add_effect (-param_count) state;
        let fentry = Hashtbl.find state.func_table fname in
        match fentry with
        | Intrinsic (ops, x) ->
            add_effect x state;
            state.buffer <- List.rev ops @ state.buffer
        | Empty | Subroutine _ ->
            emit_val state (RawOp Opcodes.OP_call);
            emit_val state (LabelRef ("@" ^ fname));
            emit_val state NoEmit)
    | _ -> assert false

and emit_funcdef_body state params body =
    push_effect state;

    emit_val state (RawOp Opcodes.OP_startscope);
    new_scope state;

    let aux x =
        emit_node state x;
        clear_effect state
    in

    let def_params name =
        def_var state name;
        emit_val state (RawOp Opcodes.OP_defvar)
    in

    List.iter def_params (List.rev params);
    List.iter aux body;

    del_scope state;
    emit_val state (RawOp Opcodes.OP_endscope);
    emit_val state (RawOp Opcodes.OP_ret);
    pop_effect state

and emit_funcdef state = function
    | Parser.FuncDef (name, params, _, Parser.Block body, _) ->
        let global_code = state.buffer in
        let param_names = List.map fst params in
        Hashtbl.add state.func_table name Empty;
        state.buffer <- [];
        emit_val state (LabelDef ("@" ^ name));
        emit_funcdef_body state param_names body;
        Hashtbl.add state.func_table name
          (Subroutine (param_names, List.rev state.buffer));
        state.buffer <- global_code
    | _ -> assert false

let collect_bodies state =
    let aux _ entry acc =
        match entry with
        | Subroutine (_, body) -> acc @ body
        | Intrinsic _ -> acc
        | Empty -> acc
    in
    Hashtbl.fold aux state.func_table []

let emitter_init () =
    let ret =
        {
          buffer = [];
          const_counter = 0;
          const_pool = Hashtbl.create 32;
          current_scope = 0;
          vars = [ Hashtbl.create 32 ];
          var_counter = [ 0 ];
          stack_effects = [ 0 ];
          block_counter = 0;
          block_stack = [];
          if_counter = 0;
          while_counter = 0;
          for_counter = 0;
          func_table = Hashtbl.create 32;
          const_history = [];
        }
    in
    let register_intrinsic =
       fun (name, data, x) ->
        Hashtbl.add ret.func_table name (Intrinsic (data, x))
    in
    let zero_const_idx = get_const ret (Parser.Number 0) in
    def_var ret "@stash";
    emit_val ret (RawOp Opcodes.OP_const);
    emit_val ret (RawValB zero_const_idx);
    emit_val ret (RawOp Opcodes.OP_defvar);
    List.iter register_intrinsic intrinsics;
    ret

let emitter_emit root =
    let state = emitter_init () in
    emit_node state root;
    let pool = List.rev state.const_history in
    (List.rev state.buffer @ collect_bodies state, pool)
