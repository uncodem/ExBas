const std = @import("std");

const stack = @import("stack.zig");
const vals = @import("vals.zig");

const Stack = stack.Stack;
const Value = vals.Value;

pub const VmCtx = struct {
    allocator: std.mem.Allocator = undefined,
    vm_stack: *Stack(*Value) = undefined,

    pub fn push(self: VmCtx, val: *Value) !void {
        try self.vm_stack.push(val);
    }

    pub fn pop(self: VmCtx) !*Value {
        return try self.vm_stack.pop();
    } 

    pub fn release(self: VmCtx, val: *Value) void {
        if (val.release()) self.allocator.destroy(val);
    }

    pub fn copy(self: VmCtx, val: *Value) !*Value {
        return try val.alloc_copy(self.allocator);
    }

    // Direct copy of the VM's makeValue function. Still assumes that every value it gets
    // is heap allocated and uses the VM's allocator.
    pub fn makeValue(self: VmCtx, data: vals.ValueData) !*Value {
        const ret = try self.allocator.create(Value);
        ret.* = Value{
            .data = data,
            .allocator = self.allocator,
            .refcount = 1,
            .size = switch (data) {
                .Int, .Float => 4,
                .Bool => 1,
                .String => data.String.len,
                .Array => data.Array.len,
            },
        };
        return ret;
    }
};

// -- Built-in functions, default implementations of the guaranteed functions.
// TODO: add more built-ins

pub fn print(ctx: VmCtx) !void {
    const stdout = std.io.getStdOut().writer();
    const v = try ctx.pop();
    defer ctx.release(v);

    switch (v.data) {
        .Int => |x| try stdout.print("{d}\n", .{x}),
        .Float => |x| try stdout.print("{d}\n", .{x}),
        .String => |x| try stdout.print("{s}\n", .{x}),
        .Bool => try stdout.print("{s}\n", .{ if (v.data.Bool) "T" else "F" }),
        .Array => return error.InvalidDataType
    }
}

pub fn input(ctx: VmCtx) !void {
    const stdin = std.io.getStdIn().reader();
    var buffer = std.ArrayList(u8).init(ctx.allocator);
    try stdin.streamUntilDelimiter(buffer.writer(), '\n', null);
    const strslice = try buffer.toOwnedSlice();
    try ctx.push(try ctx.makeValue(.{.String = strslice}));
}

