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

val emitter_init : unit -> emitter_state
(** Create baseline state for emitter *)

val emit_node : emitter_state -> Parser.ast_node -> unit
(** Mutates emitter_state with program emissions *)

val emitter_emit : Parser.ast_node -> emit_me list * const_value list
(** Automatically initialize emitter and execute pass 1 and 2. *)
