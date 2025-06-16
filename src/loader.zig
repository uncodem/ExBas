const std = @import("std");
const vals = @import("vals.zig");

const Value = vals.Value;

pub const LoaderError = error {
    InvalidProgram,
    MalformedProgramFile
};

pub const Program = struct {
    allocator: std.mem.Allocator = undefined,
    code: []u8 = undefined,
    constants: std.ArrayList(Value) = undefined,

    pub fn deinit(self: Program) void {
        self.code.deinit();
        self.constants.deinit();
    }
};

pub const MAGIC_NUMBER = 0xdeadbeef;
pub const MAX_SIZE = 1e7;

pub fn readFile(allocator: std.mem.Allocator, fname: []const u8) !Program {
    var ret = Program{
        .allocator = allocator
    };

    const file = if (std.fs.path.isAbsolute(fname)) try std.fs.openFileAbsolute(fname, .{}) else try std.fs.cwd().openFile(fname, .{});
    defer file.close();

    const reader = file.reader();
    const magic_test = try reader.readInt(u32, .little) catch return error.InvalidProgram;

    if (magic_test != MAGIC_NUMBER) return error.InvalidProgram;
    
    const codesize = try reader.readInt(usize, .little) catch return error.MalformedProgramFile;
    ret.code = try allocator.alloc(u8, codesize);
    errdefer allocator.free(ret.code);

    if (codesize != try reader.readAll(ret.code)) return error.InvalidProgram;

    const values = try reader.readAllAlloc(allocator, MAX_SIZE);
    defer allocator.free(values);
    
    ret.constants = try vals.readValues(allocator, values);

    return ret;
}

