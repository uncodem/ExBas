const std = @import("std");
const builtin = @import("builtin");
const expect = std.testing.expect;

pub const StackError = error{StackUnderflow};

pub fn Stack(comptime T: type) type {
    return struct {
        const Self = @This();

        data: []T = undefined,
        sp: usize = 0,
        allocator: std.mem.Allocator = undefined,

        // Should use only for debugging
        push_count: u32 = 0,
        pop_count: u32 = 0,

        pub fn init(allocator: std.mem.Allocator) !Self {
            return Self{ .data = try allocator.alloc(T, 8), .allocator = allocator };
        }

        pub fn deinit(this: Self) void {
            this.allocator.free(this.data);
        }

        pub fn push(this: *Self, x: T) !void {
            if (this.data.len == this.sp) {
                this.data = try this.allocator.realloc(this.data, this.sp * 2);
            }
            this.data[this.sp] = x;
            this.sp += 1;
            this.push_count += 1;
        }

        pub fn top(this: *Self) ?T {
            if (this.sp == 0) return null;
            return this.data[this.sp - 1];
        }

        pub fn pop(this: *Self) !T {
            const ret = this.top();
            if (ret == null) return error.StackUnderflow;
            this.sp -= 1;
            this.push_count += 1;
            return ret.?;
        }
    };
}

test "src/stack.zig" {
    var stack = try Stack(u32).init(std.testing.allocator);
    defer stack.deinit();
    try expect(stack.data.len == 8);

    for (0..9) |i| {
        try stack.push(@intCast(i));
    }

    try expect(stack.top().? == 8);
    try expect(stack.data.len == 16);
}
