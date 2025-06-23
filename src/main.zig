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

    var program = try loader.readFile(allocator, "test.bin");
    defer program.deinit();

    var vm = try core.Vm.init(allocator, &program);
    defer vm.deinit();

    try vm.run(std.io.getStdIn().reader());
}
