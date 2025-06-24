const std = @import("std");
const loader = @import("loader.zig");
const opcodes = @import("opcodes.zig");

const Opcode = opcodes.VmOpcode;
const Program = loader.Program;

pub const DebugError = error {
    InvalidOpcode,
    MalformedProgram
};

const BytecodeIter = struct {
    code: []const u8 = undefined,
    index: usize = 0,

    pub fn init(code: []const u8) BytecodeIter {
        return .{.code = code};
    }

    pub fn next(self: *BytecodeIter) ?u8 {
        if (self.index >= self.code.len) return null;
        const ret = self.code[self.index];
        self.index += 1;
        return ret;
    }

    pub fn offset(self: *BytecodeIter) usize {
        return self.index - 1;
    }
};

pub fn countOperands(opc: Opcode) !u8 {
    return switch(opc) {
        .OP_CAST, .OP_CONST => 1,

        .OP_PUSHVAR, .OP_POPVAR, .OP_CREATEARRAY,
        .OP_INITARRAY, .OP_CGET, .OP_CSET, .OP_JMP, .OP_TJMP, .OP_CALL, .OP_TCALL, => 2,

        .OP_ADD, .OP_SUB, .OP_MUL, .OP_DIV, .OP_AND, 
        .OP_OR, .OP_NOT, .OP_LESS, .OP_EQL, 
        .OP_EQLESS, .OP_EQMORE, .OP_NEQL, 
        .OP_SIZE, .OP_COPY, .OP_STARTSCOPE, .OP_ENDSCOPE, .OP_SWAP, 
        .OP_DUMP, .OP_RET, .OP_DROP, .OP_RSET, .OP_RGET, .OP_DUP, .OP_INPUT, .OP_DEFVAR => 0,

        else => error.InvalidOpcode,
    };
}

pub fn dumpOpcode(opc: Opcode) []const u8 {
    return @tagName(opc);
}

pub fn dumpConstants(program: Program) void {
    // TODO: rework Value to have a dump function that works for any writer
    // Currently, Value.dump() simply writes to stderr
    if (program.constants.items.len == 0) { std.debug.print("No constants.\n", .{}); return; }
    for (0.., program.constants.items) |i, val| {
        std.debug.print("{d}. ", .{i});
        val.dump();
    }
}

pub fn dumpCode(program: Program) !void {
    var iter = BytecodeIter.init(program.code);
    var operand_count: u8 = 0;

    std.debug.print("{s:<4}  {s:<16}  {s}\n", .{"ADDR", "OPCODE", "OPERANDS"});

    while (iter.next()) |x| {
        if (operand_count == 0) {
            const addr = iter.offset();
            const opc = std.meta.intToEnum(Opcode, x) catch {
                std.debug.print("{X:04}  {s:<16}({X:02})\n", .{addr, "INVALID OPCODE", x});
                continue;
            };
            std.debug.print("{X:04}  {s:<16}  ", .{addr, dumpOpcode(opc)});
            operand_count = try countOperands(opc);
        } else {
            std.debug.print("{X:02} ", .{x});
            operand_count -= 1;
        }

        if (operand_count == 0) {
            std.debug.print("\n", .{});
        }
    }
    if (operand_count != 0) return error.MalformedProgram;
}


