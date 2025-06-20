const std = @import("std");
const core = @import("core.zig");
const loader = @import("loader.zig");
const opcodes = @import("opcodes.zig");

const opc = opcodes.VmOpcode;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    defer _ = gpa.detectLeaks();

    const allocator = gpa.allocator();
    // const allocator = std.heap.c_allocator;

    //    const byte_data = [_]u8{0, 36, 0x00, 0x00, 0x00, 0, 33, 0x00, 0x00, 0x00, 1, 0x41, 0x42, 0x43, 0x44, 0x45, 0x00};
    const byte_data = [_]u8{};

    const test_prog = [_]u8{ @intFromEnum(opc.OP_INPUT), @intFromEnum(opc.OP_CAST), 3, @intFromEnum(opc.OP_DUMP), @intFromEnum(opc.OP_RET) };

    const prog = try loader.Program.init(allocator, &byte_data, &test_prog);
    defer prog.deinit();

    var vm = try core.Vm.init(allocator, &prog);
    defer vm.deinit();

    try vm.run(std.io.getStdIn().reader());
}
