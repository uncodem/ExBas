const std = @import("std");

pub const VmOpcode = enum(u8) {
    OP_CONST = 0,

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

    OP_DUMP,
    OP_RET,
    _
};

