const std = @import("std");

pub const VmOpcode = enum(u8) {
    OP_CONST = 0,
    OP_COPY,
    OP_CAST,
    OP_SIZE,

    OP_STARTSCOPE,
    OP_ENDSCOPE,

    OP_DEFVAR,
    OP_PUSHVAR,
    OP_POPVAR,
    OP_SWAP,
    OP_DUP,
    OP_DROP,

    OP_CREATEARRAY,
    OP_CREATEARRAY_ND,
    OP_INITARRAY,
    OP_CGET,
    OP_RGET,
    OP_CSET,
    OP_RSET,

    OP_ADD,
    OP_SUB,
    OP_DIV,
    OP_MUL,
    OP_MOD,
    OP_NEG,

    OP_AND,
    OP_OR,
    OP_NOT,

    OP_EQL,
    OP_NEQL,

    OP_MORE,
    OP_LESS,
    OP_EQMORE,
    OP_EQLESS,

    OP_JMP,
    OP_CALL,
    OP_TJMP,
    OP_TCALL,

    OP_DUMP,
    OP_NATIVE,
    OP_RET,
    _,
};
