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
pub const MAX_SIZE = 1e7;

pub fn readReader(allocator: std.mem.Allocator, reader: std.io.Reader) !Program {
    var ret = Program{
        .allocator = allocator
    };

    const magic_test = try reader.readInt(u32, .little) catch return error.InvalidProgram;

    if (magic_test != MAGIC_NUMBER) return error.InvalidProgram;
    
    const codesize: usize = try reader.readInt(u32, .little) catch return error.MalformedProgramFile;
    ret.code = try allocator.alloc(u8, codesize);
    errdefer allocator.free(ret.code);

    if (codesize != try reader.readAll(ret.code)) return error.InvalidProgram;

    const values = try reader.readAllAlloc(allocator, MAX_SIZE);
    defer allocator.free(values);
    
    ret.constants = try vals.readValues(allocator, values);

    return ret;
}

pub fn readFile(allocator: std.mem.Allocator, fname: []const u8) !Program {
    const file = if (std.fs.path.isAbsolute(fname)) try std.fs.openFileAbsolute(fname, .{}) else try std.fs.cwd().openFile(fname, .{});
    defer file.close();

    const reader = file.reader();

    return readReader(allocator, reader);
}

