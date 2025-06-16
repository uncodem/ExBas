const std = @import("std");
const vals = @import("vals.zig");

const Value = vals.Value;

pub const Program = struct {
    allocator: std.mem.Allocator = undefined,
    code: std.ArrayList(u8) = undefined,
    constants: std.ArrayList(Value) = undefined,

    pub fn deinit(self: Program) void {
        self.code.deinit();
        self.constants.deinit();
    }
};



