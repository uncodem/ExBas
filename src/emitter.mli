type emitter_state

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

val emitter_int : unit -> emitter_state
(** Create baseline state for emitter *)

val emit_node : emitter_state -> Parser.ast_node -> unit
(** Mutates emitter_state with program emissions *)

