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

    const file_data = [_]u8{
        0xef, 0xbe, 0xad, 0xde,
        0x03, 0x00, 0x00, 0x00,
        @intFromEnum(opc.OP_INPUT),
        @intFromEnum(opc.OP_DUMP),
        @intFromEnum(opc.OP_RET),
        1, 0x41, 0x42, 0x43, 0x00,
    };

    var file = try std.fs.cwd().createFile("test.bin", .{});
    const writer = file.writer();
    try writer.writeAll(&file_data);
    file.close();

    var program = try loader.readFile(allocator, "test.bin");
    defer program.deinit();

    try std.fs.cwd().deleteFile("test.bin");

    var vm = try core.Vm.init(allocator, &program);
    defer vm.deinit();

    try vm.run(std.io.getStdIn().reader());
}
