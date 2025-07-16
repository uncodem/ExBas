const std = @import("std");
const core = @import("core.zig");
const loader = @import("loader.zig");
const opcodes = @import("opcodes.zig");
const debug = @import("debug.zig");
const interface = @import("interface.zig");

const opc = opcodes.VmOpcode;

fn print_usage(path: []const u8, command: ?[]const u8) void {
    if (command) |x| std.debug.print("Unknown command: {s}.\n", .{x});
    std.debug.print("Usage : {s} [run|dump] <filename>\n", .{path});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
//    defer _ = gpa.detectLeaks();

    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    const prog_path = args.next().?;
    const command = args.next() orelse {
        print_usage(prog_path, null);
        return;
    };

    const filename = args.next() orelse {
        print_usage(prog_path, null);
        return;
    };

    if (std.mem.eql(u8, command, "run")) {
        var program = loader.readFile(allocator, filename) catch |err| {
            if (err == error.FileNotFound) {
                std.debug.print("Unable to open file : {s}\n", .{filename});
                return;
            } else {
                return err;
            }
        };
        defer program.deinit();

        var vm = try core.Vm.init(allocator, &program);
        defer vm.deinit();

        const stdErr = std.io.getStdErr().writer();
        try vm.registerNative(interface.print);
        try vm.registerNative(interface.input);

        try vm.run(stdErr);
    } else if (std.mem.eql(u8, command, "dump")) {
        var program = loader.readFile(allocator, filename) catch |err| {
            if (err == error.FileNotFound) {
                std.debug.print("Unable to open file : {s}\n", .{filename});
                return;
            } else {
                return err;
            }
        };
        defer program.deinit();

        const writer = std.io.getStdOut().writer();
        try writer.print("Constants:\n", .{});
        try debug.dumpConstants(writer, program);
        try writer.print("\nCode:\n", .{});
        try debug.dumpCode(writer, program);
    } else {
        print_usage(prog_path, command);
    }
}
