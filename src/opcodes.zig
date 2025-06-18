const std = @import("std");

pub const VmOpcode = enum(u8) {
    OP_CONST = 0,
    OP_COPY,

    OP_STARTSCOPE,
    OP_ENDSCOPE,

    OP_DEFVAR,
    OP_PUSHVAR,
    OP_POPVAR,

    OP_ADD,
    OP_SUB,
    OP_DIV,
    OP_MUL,
    OP_EQL,
    OP_NEQL,
    OP_MORE,
    OP_LESS,

    OP_JMP,
    OP_CALL,
    OP_TJMP,
    OP_TCALL,

    OP_DUMP,
    OP_INPUT,
    OP_RET,
    _
};

