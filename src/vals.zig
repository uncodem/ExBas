const std = @import("std");
const expect = std.testing.expect;


pub const ValueError = error {
    InvalidData,
    InvalidDataType
};

pub const ValueType = enum(u8) {
    Int = 0,
    String,
    Bool,
    _
};

pub const Value = struct {
    size: usize = 0,
    refcount: u32 = 0,
    data: union(ValueType) {
        Int: i32,
        String: []u8,
        Bool: bool
    },
    allocator: std.mem.Allocator = undefined,

    pub fn copy(self: Value) !Value {
        return switch(self.data) {
            .Int => Value{
                .refcount = 1,
                .allocator = self.allocator,
                .data = .{ .Int = self.data.Int },
                .size = self.size,
            },
            .Bool => Value{
                .refcount = 1,
                .allocator = self.allocator,
                .data = .{ .Bool = self.data.Bool },
                .size = self.size
            },
            .String => Value{
                .refcount = 1,
                .size = self.size,
                .allocator = self.allocator,
                .data = .{ .String = blk: {
                    const buffer = try self.allocator.alloc(u8, self.size);
                    std.mem.copyForwards(u8, buffer, self.data.String);
                    break :blk buffer;
                }}
            }
        };
    }

    pub fn alloc_copy(self: Value, allocator: std.mem.Allocator) !*Value {
        const ret: *Value = try allocator.create(Value);
        ret.* = try self.copy();
        ret.allocator = allocator;
        return ret;
    }

    pub fn dump(self: Value) void {
        switch (self.data) {
            .String => |x| std.debug.print("StrValue({s})\n", .{x}),
            .Int => |x| std.debug.print("IntValue({d})\n", .{x}),
            .Bool => |x| { 
                const ch: u8 = if (x) 'T' else 'F';
                std.debug.print("BoolValue({c})\n", .{ ch });
            }
        }
    }

    pub fn deinit(self: Value) void {
        return switch(self.data) {
            .Int, .Bool => {},
            .String => self.allocator.free(self.data.String)
        };
    }

    pub fn release(self: *Value) bool {
        if (self.refcount == 0) unreachable;

        self.refcount -= 1;

        if (self.refcount == 0) {
            self.deinit();
            return true;
        }

        return false;
    }

    pub fn kind(self: Value) ValueType {
        return @as(ValueType, self.data);
    }

};

pub fn readValue(allocator: std.mem.Allocator, byte_data: []const u8) !Value {
    if (byte_data.len < 1) return error.InvalidData;

    const ret_type: ValueType = @enumFromInt(byte_data[0]);
    const val_data = byte_data[1..];

    return switch(ret_type) {
        .Int => intblk: {
            if (byte_data.len < 5) return error.InvalidData;
            const val = std.mem.readInt(i32, val_data[0..4], .little);
            break :intblk Value{
                .allocator = allocator,
                .size = 4,
                .refcount = 0,
                .data = .{ .Int = val },
            };         
        },

        .Bool => Value{
            .allocator = allocator,
            .size = 1,
            .refcount = 0,
            .data = .{ .Bool = (byte_data[1] == 1) }
        },

        .String => strblk: {
            const endIndx = std.mem.indexOfScalar(u8, val_data, 0) orelse return error.InvalidData;
            const strslice = val_data[0..endIndx];
            const buffer = try allocator.alloc(u8, strslice.len);
            std.mem.copyForwards(u8, buffer, strslice);
            break :strblk Value{
                .allocator = allocator,
                .size = buffer.len,
                .data = .{ .String = buffer },
                .refcount = 0
            };
        },

        _ => return error.InvalidDataType
    };
}

pub fn readValues(allocator: std.mem.Allocator, byte_data: []const u8) !std.ArrayList(Value) {
    var ret = std.ArrayList(Value).init(allocator);
    var pos: usize = 0;

    while (pos < byte_data.len) {
        const value = try readValue(allocator, byte_data[pos..]);
        try ret.append(value);
        pos += value.size+1; 

        // Skip null-terminator for string
        if (value.kind() == .String) pos += 1;
    }

    return ret;
}

test "src/vals.zig readValue" {
    // StrValue(ABC)
    const strbyte_data = [_]u8{1, 65, 66, 67, 0};
    const strvalue = try readValue(std.testing.allocator, &strbyte_data);
    defer strvalue.deinit();

    try expect(strvalue.kind() == .String);
    try expect(strvalue.data.String.len == 3 and strvalue.size == 3);

    // IntValue(65)
    const intbyte_data = [_]u8{0, 0x41, 0x00, 0x00, 0x00};
    const intvalue = try readValue(std.testing.allocator, &intbyte_data);
    defer intvalue.deinit();

    try expect(intvalue.kind() == .Int);
    try expect(intvalue.size == 4);
    try expect(intvalue.data.Int == 65);

    // BoolValue(F)
    const boolbyte_data = [_]u8{2, 0};
    const boolvalue = try readValue(std.testing.allocator, &boolbyte_data);
    defer boolvalue.deinit();

    try expect(boolvalue.kind() == .Bool);
    try expect(boolvalue.size == 1);
    try expect(!boolvalue.data.Bool);

}

test "src/vals.zig readValues" {
    const byte_data = [_]u8{0, 0x42, 0x00, 0x00, 0x00, 2, 0x01, 1, 0x41, 0x42, 0x43, 0x44, 0x45, 0x00};
    const values = try readValues(std.testing.allocator, &byte_data);
    defer {
        for (values.items) |value| {
            value.deinit();
        }
        values.deinit();
    }
    try expect(values.items.len == 3);
}

