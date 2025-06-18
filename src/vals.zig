const std = @import("std");
const expect = std.testing.expect;

pub const ValueError = error {
    InvalidData,
    InvalidDataType,
    InvalidCast
};

pub const ValueType = enum(u8) {
    Int = 0,
    String,
    Bool,
    Float,
    _
};

pub const Value = struct {
    size: usize = 0,
    refcount: u32 = 0,
    data: union(ValueType) {
        Int: i32,
        String: []u8,
        Bool: bool,
        Float: f32
    } = undefined,
    allocator: std.mem.Allocator = undefined,

    pub fn copy(self: Value) !Value {
        return Value{
            .allocator = self.allocator,
            .size = self.size,
            .data = switch(self.data) {
                .Int => .{ .Int = self.data.Int },
                .Bool => .{ .Bool = self.data.Bool },
                .Float => .{ .Float = self.data.Float },
                .String => .{ .String = blk: {
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
        ret.refcount = 1;
        ret.allocator = allocator;
        return ret;
    }

    pub fn dump(self: Value) void {
        switch (self.data) {
            .String => |x| std.debug.print("StrValue({s})\n", .{x}),
            .Int => |x| std.debug.print("IntValue({d})\n", .{x}),
            .Float => |x| std.debug.print("FloatValue({d})\n", .{x}),
            .Bool => |x| { 
                const ch: u8 = if (x) 'T' else 'F';
                std.debug.print("BoolValue({c})\n", .{ ch });
            }
        }
    }

    pub fn deinit(self: Value) void {
        return switch(self.data) {
            .Int, .Bool, .Float => {},
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

    pub fn strcast(self: *Value, allocator: std.mem.Allocator) ![]u8 {
        var buffer = std.ArrayList(u8).init(allocator);
        const writer = buffer.writer();

        switch (self.data) {
            .Int => |x| { try writer.print("{d}", .{x}); },
            .Float => |x| { try writer.print("{d}", .{x}); },
            else => return error.InvalidCast
        }

        return try buffer.toOwnedSlice();
    }

    pub fn cast(self: *Value, allocator: std.mem.Allocator, res_type: ValueType) !Value {
        var ret = Value{
            .allocator = self.allocator,
            .size = switch(res_type) {
                .Int, .Float => 4,
                .Bool => 1,
                .String => 0, // Zero due to it being dependent on data
                _ => return error.InvalidDataType
            }
        };

        switch(self.data) {
            .String => |x| {
                ret.data = switch(res_type) {
                    .Int => .{.Int = try std.fmt.parseInt(i32, x, 10)},
                    .Float => .{.Float = try std.fmt.parseFloat(f32, x)},
                    else => return error.InvalidCast
                };
            },
            .Bool => |x| {
                ret.data = switch(res_type) {
                    .Int => .{.Int = if (x) 1 else 0},
                    .Float => .{.Float = if (x) 1.0 else 0.0},
                    else => return error.InvalidCast
                };
            },
            .Int => |x| {
                ret.data = switch(res_type) {
                    .String => .{.String = try self.strcast(allocator)},
                    .Float => .{.Float = @floatFromInt(x)},
                    else => return error.InvalidCast
                };
                if (res_type == .String) ret.size = ret.data.String.len;
            },
            .Float => |x| {
                ret.data = switch(res_type) {
                    .String => .{.String = try self.strcast(allocator)},
                    .Int => .{.Int = @intFromFloat(x)},
                    else => return error.InvalidCast
                };
                if (res_type == .String) ret.size = ret.data.String.len;
            },
            else => unreachable
        }

        return ret;
    }

};

pub fn readValue(allocator: std.mem.Allocator, byte_data: []const u8) !Value {
    if (byte_data.len < 1) return error.InvalidData;

    const ret_type: ValueType = std.meta.intToEnum(ValueType, byte_data[0]) catch return error.InvalidDataType;
    const val_data = byte_data[1..];

    const valsize: usize = switch(ret_type) {
        .Int, .Float => 4,
        .Bool => 1,
        .String => 0, // Zero due to it being dependent on data
        _ => return error.InvalidDataType
    };

    var ret = Value{
        .allocator = allocator,
        .size = valsize,
        .refcount = 0
    };

    if (ret_type != .String and val_data.len < valsize) return error.InvalidData;

    ret.data = switch (ret_type) {
        .Int => .{ .Int = std.mem.readInt(i32, val_data[0..4], .little) },
        .Float => .{ .Float = @bitCast(std.mem.readInt(i32, val_data[0..4], .little)) },
        .Bool => .{ .Bool = (val_data[0] == 1) },
        .String => .{ .String = strblk: {
            const endIndx = std.mem.indexOfScalar(u8, val_data, 0) orelse return error.InvalidData;
            const strslice = val_data[0..endIndx];
            const buffer = try allocator.alloc(u8, strslice.len);
            std.mem.copyForwards(u8, buffer, strslice);
            ret.size = strslice.len;
            break :strblk buffer;
        }},
        _ => unreachable
    };

    return ret;
}

pub fn readValues(allocator: std.mem.Allocator, byte_data: []const u8) !std.ArrayList(Value) {
    var ret = std.ArrayList(Value).init(allocator);
    var pos: usize = 0;

    if (byte_data.len == 0) { return ret; } // Allow empty constants

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
    try expect(std.mem.eql(u8, strvalue.data.String, "ABC"));

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

    const floatbyte_data = [_]u8{3,86,14,73,64};
    const floatvalue = try readValue(std.testing.allocator, &floatbyte_data);
    defer floatvalue.deinit();

    try expect(floatvalue.kind() == .Float);
    try expect(floatvalue.size == 4);
    try expect(floatvalue.data.Float == 3.1415);
}

test "src/vals.zig readValues" {
    const byte_data = [_]u8{0, 0x42, 0x00, 0x00, 0x00, 2, 0x01, 1, 0x41, 0x42, 0x43, 0x44, 0x45, 0x00, 3,86,14,73,64};
    const values = try readValues(std.testing.allocator, &byte_data);
    defer {
        for (values.items) |value| {
            value.deinit();
        }
        values.deinit();
    }
    try expect(values.items.len == 4);
}

test "src/vals.zig casting" {
    const allocator = std.testing.allocator;
    const expectError = std.testing.expectError;

    const strbyte_data = [_]u8{1, 65, 66, 67, 0};
    var strvalue = try readValue(std.testing.allocator, &strbyte_data);
    defer strvalue.deinit();

    try expectError(error.InvalidCast, strvalue.cast(allocator, ValueType.Bool)); // Str -> Bool

    const boolbyte_data = [_]u8{2, 0};
    var boolvalue = try readValue(std.testing.allocator, &boolbyte_data);
    defer boolvalue.deinit();

    try expectError(error.InvalidCast, boolvalue.cast(allocator, ValueType.String)); // Bool -> Str
    const intbool = try boolvalue.cast(allocator, ValueType.Int); // Bool -> Int
    try expect(intbool.data.Int == 0);

    const floatbyte_data = [_]u8{3,86,14,73,64};
    var floatvalue = try readValue(std.testing.allocator, &floatbyte_data);
    defer floatvalue.deinit();
    
    var floatstring = try floatvalue.cast(allocator, ValueType.String); // Float -> Str
    defer floatstring.deinit();

    try expect(std.mem.eql(u8, floatstring.data.String, "3.1415"));
}

