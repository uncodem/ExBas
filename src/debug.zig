const std = @import("std");
const loader = @import("loader.zig");
const opcodes = @import("opcodes.zig");

const Opcode = opcodes.VmOpcode;
const Program = loader.Program;

pub const DebugError = error {
    InvalidOpcode
};

pub fn dumpOpcode(raw_opc: u8) ![]u8 {
    const opc: Opcode = std.meta.intToEnum(Opcode, raw_opc) catch return error.InvalidOpcode;
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

