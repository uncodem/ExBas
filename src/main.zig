const std = @import("std");
const core = @import("core.zig");
const loader = @import("loader.zig");
const opcodes = @import("opcodes.zig");

const opc = opcodes.VmOpcode;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const code = [_]u8{ @intFromEnum(opc.OP_CONST), 0, @intFromEnum(opc.OP_DUMP), @intFromEnum(opc.OP_RET) };

    const const_data = [_]u8{ 1, 0x41, 0x42, 0x43, 0x00 };

    const prog = try loader.Program.init(allocator, &const_data, &code);
    defer prog.deinit();

    std.debug.print("{any}", .{gpa.detectLeaks()});
}
