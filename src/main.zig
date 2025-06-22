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
    const test_prog = [_]u8{ @intFromEnum(opc.OP_INPUT), @intFromEnum(opc.OP_CAST), 3, @intFromEnum(opc.OP_DUMP), @intFromEnum(opc.OP_RET) };

    const test_rig = try core.makeTestVm(allocator, &test_prog);
    var vm = test_rig.vm;
    defer vm.deinit();
    const prog = test_rig.program;
    defer prog.deinit();
    defer test_rig.allocator.destroy(prog);

    try vm.run(std.io.getStdIn().reader());
}
