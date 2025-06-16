const std = @import("std");
const vals = @import("vals.zig");
const core = @import("core.zig");
const opcodes = @import("opcodes.zig");

const opc = opcodes.VmOpcode;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    // const allocator = std.heap.c_allocator;

    const byte_data = [_]u8{0, 36, 0x00, 0x00, 0x00, 0, 33, 0x00, 0x00, 0x00, 1, 0x41, 0x42, 0x43, 0x44, 0x45, 0x00};

    var values = try vals.readValues(allocator, &byte_data);
    defer {
        for (values.items) |val| {
            val.deinit();
        }
        values.deinit();
    }

    const test_prog = [_]u8{
        @intFromEnum(opc.OP_CONST), 0,
        @intFromEnum(opc.OP_CONST), 1,
        @intFromEnum(opc.OP_ADD),
        @intFromEnum(opc.OP_DUMP),
        @intFromEnum(opc.OP_RET)
    };

    var vm = try core.Vm.init(allocator, &test_prog, values.items);
    defer vm.deinit();

    try vm.run();
}

