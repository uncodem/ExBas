const std = @import("std");
const vals = @import("vals.zig");

const expect = std.testing.expect;

const Value = vals.Value;

pub const LoaderError = error{
    InvalidProgram,
};

pub const Program = struct {
    allocator: std.mem.Allocator = undefined,
    code: []u8 = undefined,
    constants: std.ArrayList(Value) = undefined,

    pub fn init(allocator: std.mem.Allocator, const_data: []const u8, code_data: []const u8) !Program {
        return Program{
            .allocator = allocator,
            .constants = try vals.readValues(allocator, const_data),
            .code = codeblk: {
                const progcode = try allocator.alloc(u8, code_data.len);
                std.mem.copyForwards(u8, progcode, code_data);
                break :codeblk progcode;
            },
        };
    }

    pub fn deinit(self: Program) void {
        self.allocator.free(self.code);
        for (self.constants.items) |constant| {
            constant.deinit();
        }
        self.constants.deinit();
    }
};

pub const MAGIC_NUMBER = 0xdeadbeef;
pub const MAX_SIZE: comptime_int = 1e7;

pub fn readReader(allocator: std.mem.Allocator, reader: anytype) !Program {
    var ret = Program{ .allocator = allocator };

    const magic_test = try reader.readInt(u32, .little);

    if (magic_test != MAGIC_NUMBER) return error.InvalidProgram;

    const codesize: usize = @as(usize, try reader.readInt(u32, .little));
    if (codesize == 0 or codesize > MAX_SIZE) return error.InvalidProgram;

    ret.code = try allocator.alloc(u8, codesize);
    errdefer allocator.free(ret.code);

    if (codesize != try reader.readAll(ret.code)) return error.InvalidProgram;

    const const_bytes = try reader.readAllAlloc(allocator, MAX_SIZE - codesize);
    defer allocator.free(const_bytes);

    ret.constants = try vals.readValues(allocator, const_bytes);

    return ret;
}

pub fn readFile(allocator: std.mem.Allocator, fname: []const u8) !Program {
    const file = if (std.fs.path.isAbsolute(fname)) try std.fs.openFileAbsolute(fname, .{}) else try std.fs.cwd().openFile(fname, .{});
    defer file.close();

    const reader = file.reader();

    return readReader(allocator, reader);
}

test "src/loader.zig Program.init" {
    // Test code is
    // OP_CONST 0
    // OP_DUMP
    // OP_RET

    const opcodes = @import("opcodes.zig");
    const opc = opcodes.VmOpcode;

    const code = [_]u8{ @intFromEnum(opc.OP_CONST), 0, @intFromEnum(opc.OP_DUMP), @intFromEnum(opc.OP_RET) };

    // String("ABC")
    const const_data = [_]u8{ 1, 0x41, 0x42, 0x43, 0x00 };

    const prog = try Program.init(std.testing.allocator, &const_data, &code);
    defer prog.deinit();

    try expect(prog.code.len == code.len);
    try expect(prog.constants.items.len == 1);
}

test "src/loader.zig readReader" {
    const opcodes = @import("opcodes.zig");
    const opc = opcodes.VmOpcode;

    const progdata = [_]u8{
        0xef, 0xbe, 0xad, 0xde, // Magic number
        0x04,                       0x00, 0x00,                      0x00, // Codesize
        @intFromEnum(opc.OP_CONST), 0,    @intFromEnum(opc.OP_DUMP),
        @intFromEnum(opc.OP_RET), // End code chunk
        1, 0x41, 0x42, 0x43, 0x00, // Start const chunk; String("ABC")
    };

    var bufstream = std.io.fixedBufferStream(&progdata);
    const prog = try readReader(std.testing.allocator, bufstream.reader());
    defer prog.deinit();
    try expect(prog.code.len == 4);
    try expect(prog.constants.items.len == 1);
}

test "src/loader.zig malformed magic number readReader" {
    const badmagic = [_]u8{
        0xef, 0xed, 0xd0, 0xde, // Bad Magic number
    };

    var bufstream = std.io.fixedBufferStream(&badmagic);
    const result = readReader(std.testing.allocator, bufstream.reader());

    try std.testing.expectError(error.InvalidProgram, result);
}

test "src/loader.zig malformed program readReader" {
    const badcodesize = [_]u8{
        0xef, 0xbe, 0xad, 0xde, // Magic number
        0x04, 0x00, 0x00, 0x00,
        0x00, 0x00, // invalid program
    };

    var bufstream = std.io.fixedBufferStream(&badcodesize);
    const result = readReader(std.testing.allocator, bufstream.reader());

    try std.testing.expectError(error.InvalidProgram, result);
}

test "src/loader.zig empty program readReader" {
    const badprogram = [_]u8{
        0xef, 0xbe, 0xad, 0xde, // Magic number
        0x00, 0x00, 0x00, 0x00, // Empty Program
    };

    var bufstream = std.io.fixedBufferStream(&badprogram);
    const result = readReader(std.testing.allocator, bufstream.reader());
    try std.testing.expectError(error.InvalidProgram, result);
}

test "src/loader.zig oversized program readReader" {
    const badprogram = [_]u8{
        0xef, 0xbe, 0xad, 0xde, // Magic number
        0xff, 0xff, 0xff, 0xff, // 4GB
        0x00, 0x00, 0x00, 0x00,
    };

    var bufstream = std.io.fixedBufferStream(&badprogram);
    const result = readReader(std.testing.allocator, bufstream.reader());
    try std.testing.expectError(error.InvalidProgram, result);
}

test "src/loader.zig readFile" {
    const opcodes = @import("opcodes.zig");
    const opc = opcodes.VmOpcode;

    const file_data = [_]u8{
        0xef, 0xbe, 0xad, 0xde,
        0x04, 0x00, 0x00, 0x00,
        @intFromEnum(opc.OP_NATIVE), 1,
        @intFromEnum(opc.OP_DUMP),
        @intFromEnum(opc.OP_RET),
        1, 0x41, 0x42, 0x43, 0x00,
    };

    var file = try std.fs.cwd().createFile("_readfile_test.bin", .{});
    const writer = file.writer();
    try writer.writeAll(&file_data);
    file.close();

    var program = try readFile(std.testing.allocator, "_readfile_test.bin");
    defer program.deinit();
    try std.fs.cwd().deleteFile("_readfile_test.bin");

    for (file_data[8..12], program.code) |x, y| {
        try expect(x == y);
    }

    try expect(program.constants.items.len == 1);
    try expect(program.constants.items[0].kind() == .String);
}

