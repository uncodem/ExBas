const std = @import("std");
const opcodes = @import("opcodes.zig");

const VmOp = opcodes.VmOpcode;

const expect = std.testing.expect;
const expectError = std.testing.expectError;

pub const ValueError = error{ InvalidData, InvalidDataType, InvalidCast, InvalidIndex, InvalidOperation };

pub const ValueType = enum(u8) {
    Int = 0,
    String,
    Bool,
    Float,
    Array,
    _,
};

pub const ValueData = union(ValueType) {
    Int: i32,
    String: []u8,
    Bool: bool,
    Float: f32,
    Array: []Value,
};

pub const Value = struct {
    size: usize = 0,
    refcount: u32 = 0,
    data: ValueData = undefined,

    allocator: std.mem.Allocator = undefined,

    pub fn copy(self: Value) !Value {
        return Value{
            .allocator = self.allocator,
            .size = self.size,
            .data = switch (self.data) {
                .Int => .{ .Int = self.data.Int },
                .Bool => .{ .Bool = self.data.Bool },
                .Float => .{ .Float = self.data.Float },
                .String => .{ .String = strblk: {
                    const buffer = try self.allocator.alloc(u8, self.size);
                    std.mem.copyForwards(u8, buffer, self.data.String);
                    break :strblk buffer;
                } },
                .Array => .{ .Array = arrblk: {
                    const buffer = try self.allocator.alloc(Value, self.size);
                    for (buffer, self.data.Array) |*x, y| {
                        x.* = try y.copy();
                    }
                    break :arrblk buffer;
                } },
            },
        };
    }

    pub fn alloc_copy(self: Value, allocator: std.mem.Allocator) !*Value {
        const ret: *Value = try allocator.create(Value);
        ret.* = try self.copy();
        ret.refcount = 1;
        return ret;
    }

    pub fn dump(self: Value, writer: anytype) !void {
        switch (self.data) {
            .String => |x| try writer.print("StrValue({s})\n", .{x}),
            .Int => |x| try writer.print("IntValue({d})\n", .{x}),
            .Float => |x| try writer.print("FloatValue({d})\n", .{x}),
            .Bool => |x| {
                const ch: u8 = if (x) 'T' else 'F';
                try writer.print("BoolValue({c})\n", .{ch});
            },
            .Array => |x| for (x) |item| try item.dump(writer),
        }
    }

    pub fn deinit(self: Value) void {
        return switch (self.data) {
            .Int, .Bool, .Float => {},
            .String => |x| self.allocator.free(x),
            .Array => |x| {
                for (x) |item| {
                    item.deinit();
                }
                self.allocator.free(x);
            },
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
            .Int => |x| {
                try writer.print("{d}", .{x});
            },
            .Float => |x| {
                try writer.print("{d}", .{x});
            },
            else => return error.InvalidCast,
        }

        return try buffer.toOwnedSlice();
    }

    // indx is i32 to allow runtime indexing.
    pub fn at(self: *Value, indx: i32) !Value {
        if (indx < 0) return error.InvalidIndex;
        return switch (self.data) {
            .Int, .Float, .Bool => return error.InvalidDataType,
            .Array => |x| arrblk: {
                if (indx >= x.len) return error.InvalidIndex;
                break :arrblk x[@intCast(indx)];
            },
            .String => |x| strblk: {
                // We do not have a char type in the VM, this would just return an Int. Since I want to avoid an unnecessary allocation.
                if (indx >= x.len) return error.InvalidIndex;
                break :strblk Value{
                    .allocator = self.allocator, // Should not matter anyways
                    .data = .{ .Int = @intCast(x[@intCast(indx)]) },
                    .size = 4,
                };
            },
        };
    }

    pub fn concat(self: *Value, y: Value, allocator: std.mem.Allocator) !Value {
        if (self.kind() != .String or y.kind() != .String) return error.InvalidDataType;

        var ret = Value{
            .allocator = allocator,
            .size = switch (self.data) {
                .String => 0, // Zero due to it being dependent on data
                else => return error.InvalidDataType,
            },
        };

        ret.data = switch (self.data) {
            .String => |x| strblk: {
                var buffer = std.ArrayList(u8).init(allocator);
                const writer = buffer.writer();
                try writer.print("{s}{s}", .{ x, y.data.String });
                break :strblk .{ .String = try buffer.toOwnedSlice() };
            },
            else => unreachable,
        };

        ret.size = ret.data.String.len;
        return ret;
    }

    pub fn arithmeticOp(self: *Value, y: Value, op: VmOp, comptime T: type) !Value {
        if (T != f32 and T != i32) @compileError("value.arithmeticOp only supports f32 and i32");
        if (self.kind() != y.kind()) return error.InvalidDataType;

        const a = if (T == i32) self.data.Int else self.data.Float;
        const b = if (T == i32) y.data.Int else y.data.Float;

        const c: T = switch (op) {
            .OP_ADD => a + b,
            .OP_SUB => a - b,
            .OP_MUL => a * b,
            .OP_DIV => if (T == i32) @divFloor(a, b) else a / b,
            .OP_MOD => @rem(a, b),
            else => return error.InvalidOperation,
        };

        return Value{ .size = 4, .data = switch (T) {
            i32 => .{ .Int = c },
            f32 => .{ .Float = c },
            else => unreachable,
        } };
    }

    pub fn comparisonOp(self: *Value, y: Value, op: VmOp, comptime T: type) !Value {
        if (T != f32 and T != i32) @compileError("value.comparisonOp only supports f32 and i32");
        if (self.kind() != y.kind()) return error.InvalidDataType;

        const a = if (T == i32) self.data.Int else self.data.Float;
        const b = if (T == i32) y.data.Int else y.data.Float;

        const c: bool = switch (op) {
            .OP_MORE => a > b,
            .OP_LESS => a < b,
            .OP_EQMORE => a >= b,
            .OP_EQLESS => a <= b,
            else => return error.InvalidOperation,
        };

        return Value{ .size = 1, .data = .{ .Bool = c } };
    }

    pub fn logicOp(self: *Value, y: Value, op: VmOp) !Value {
        if (self.kind() != .Bool or y.kind() != .Bool) return error.InvalidDataType;

        const a = self.data.Bool;
        const b = y.data.Bool;
        return Value{
            .size = 1,
            .data = .{ .Bool = switch (op) {
                .OP_AND => a and b,
                .OP_OR => a or b,
                else => return error.InvalidOperation,
            } },
        };
    }

    pub fn eql(self: *Value, y: Value) !bool {
        if (self.kind() == .Array or y.kind() == .Array) return error.InvalidDataType;
        if (self.kind() != y.kind()) return false;
        return switch (self.data) {
            .Int => |x| x == y.data.Int,
            .Float => |x| x == y.data.Float,
            .Bool => |x| x == y.data.Bool,
            .String => |x| std.mem.eql(u8, x, y.data.String),
            else => return error.InvalidDataType,
        };
    }

    pub fn setAt(self: *Value, value: Value, indx: i32) !void {
        if (self.kind() != .Array) return error.InvalidDataType;
        if (indx < 0 or indx >= self.size) return error.InvalidIndex;
        self.data.Array[@intCast(indx)].deinit();
        self.data.Array[@intCast(indx)] = value;
    }

    pub fn cast(self: *Value, allocator: std.mem.Allocator, res_type: ValueType) !Value {
        if (self.kind() == .Array or res_type == .Array) return error.InvalidCast;

        var ret = Value{
            .allocator = self.allocator,
            .size = switch (res_type) {
                .Int, .Float => 4,
                .Bool => 1,
                .String => 0, // Zero due to it being dependent on data
                .Array => return error.InvalidCast,
                _ => return error.InvalidDataType,
            },
        };

        ret.data = switch (self.data) {
            .String => |x| switch (res_type) {
                .Int => .{ .Int = try std.fmt.parseInt(i32, x, 10) },
                .Float => .{ .Float = try std.fmt.parseFloat(f32, x) },
                else => return error.InvalidCast,
            },
            .Bool => |x| switch (res_type) {
                .Int => .{ .Int = if (x) 1 else 0 },
                .Float => .{ .Float = if (x) 1.0 else 0.0 },
                else => return error.InvalidCast,
            },
            .Int => |x| switch (res_type) {
                .String => .{ .String = try self.strcast(allocator) },
                .Float => .{ .Float = @floatFromInt(x) },
                else => return error.InvalidCast,
            },
            .Float => |x| switch (res_type) {
                .String => .{ .String = try self.strcast(allocator) },
                .Int => .{ .Int = @intFromFloat(x) },
                else => return error.InvalidCast,
            },
            else => return error.InvalidDataType,
        };

        if (res_type == .String) ret.size = ret.data.String.len;

        return ret;
    }
};

pub const ReadValueResult = struct { value: Value, bytes_read: usize };

pub fn readValue(allocator: std.mem.Allocator, byte_data: []const u8) !ReadValueResult {
    if (byte_data.len < 1) return error.InvalidData;

    const ret_type: ValueType = std.meta.intToEnum(ValueType, byte_data[0]) catch return error.InvalidDataType;
    const val_data = byte_data[1..];

    const valsize: usize = switch (ret_type) {
        .Int, .Float => 4,
        .Bool => 1,
        .Array, .String => 0, // Zero due to it being dependent on data
        _ => return error.InvalidDataType,
    };

    var valret = Value{ .allocator = allocator, .size = valsize, .refcount = 0 };
    var consumed = valsize + 1;

    if (valsize != 0 and val_data.len < valsize) return error.InvalidData;

    valret.data = switch (ret_type) {
        .Int => .{ .Int = std.mem.readInt(i32, val_data[0..4], .little) },
        .Float => .{ .Float = @bitCast(std.mem.readInt(i32, val_data[0..4], .little)) },
        .Bool => .{ .Bool = (val_data[0] == 1) },
        .String => .{ .String = strblk: {
            const endIndx = std.mem.indexOfScalar(u8, val_data, 0) orelse return error.InvalidData;
            const strslice = val_data[0..endIndx];
            const buffer = try allocator.alloc(u8, strslice.len);
            std.mem.copyForwards(u8, buffer, strslice);
            valret.size = strslice.len;
            consumed += 1 + strslice.len;
            break :strblk buffer;
        } },
        .Array => .{ .Array = arrblk: {
            const arrsize = val_data[0];
            valret.size = arrsize;
            consumed += 1;
            var pos: usize = 1;
            var buffer = std.ArrayList(Value).init(allocator);
            errdefer buffer.deinit();
            for (0..arrsize) |_| {
                const v = try readValue(allocator, val_data[pos..]);
                pos += v.bytes_read;
                if (pos > val_data.len) return error.InvalidData;
                consumed += v.bytes_read;
                try buffer.append(v.value);
            }
            break :arrblk try buffer.toOwnedSlice();
        } },
        _ => unreachable,
    };

    return ReadValueResult{ .value = valret, .bytes_read = consumed };
}

pub fn readValues(allocator: std.mem.Allocator, byte_data: []const u8) !std.ArrayList(Value) {
    var ret = std.ArrayList(Value).init(allocator);
    var pos: usize = 0;

    if (byte_data.len == 0) {
        return ret;
    } // Allow empty constants

    while (pos < byte_data.len) {
        const result = try readValue(allocator, byte_data[pos..]);
        try ret.append(result.value);
        pos += result.bytes_read;
    }

    return ret;
}

test "src/vals.zig readValue String" {
    // StrValue(ABC)
    const strbyte_data = [_]u8{ 1, 65, 66, 67, 0 };
    const strresult = try readValue(std.testing.allocator, &strbyte_data);
    const strvalue = strresult.value;
    defer strvalue.deinit();

    try expect(strresult.bytes_read == strbyte_data.len);
    try expect(strvalue.kind() == .String);
    try expect(strvalue.data.String.len == 3 and strvalue.size == 3);
    try expect(std.mem.eql(u8, strvalue.data.String, "ABC"));
}

test "src/vals.zig readValue Int" {
    // IntValue(65)
    const intbyte_data = [_]u8{ 0, 0x41, 0x00, 0x00, 0x00 };
    const intresult = try readValue(std.testing.allocator, &intbyte_data);
    const intvalue = intresult.value;
    defer intvalue.deinit();

    try expect(intresult.bytes_read == intbyte_data.len);
    try expect(intvalue.kind() == .Int);
    try expect(intvalue.size == 4);
    try expect(intvalue.data.Int == 65);
}

test "src/vals.zig readValue Bool" {
    // BoolValue(F)
    const boolbyte_data = [_]u8{ 2, 0 };
    const boolresult = try readValue(std.testing.allocator, &boolbyte_data);
    const boolvalue = boolresult.value;
    defer boolvalue.deinit();

    try expect(boolresult.bytes_read == boolbyte_data.len);
    try expect(boolvalue.kind() == .Bool);
    try expect(boolvalue.size == 1);
    try expect(!boolvalue.data.Bool);
}

test "src/vals.zig readValue Float" {
    const floatbyte_data = [_]u8{ 3, 86, 14, 73, 64 };
    const floatresult = try readValue(std.testing.allocator, &floatbyte_data);
    const floatvalue = floatresult.value;
    defer floatvalue.deinit();

    try expect(floatresult.bytes_read == floatbyte_data.len);
    try expect(floatvalue.kind() == .Float);
    try expect(floatvalue.size == 4);
    try expect(floatvalue.data.Float == 3.1415);
}

test "src/vals.zig readValue Array" {
    const arrbyte_data = [_]u8{ 4, 4, 2, 0, 2, 1, 2, 0, 2, 1 };
    const arrresult = try readValue(std.testing.allocator, &arrbyte_data);
    const arrvalue = arrresult.value;
    defer arrvalue.deinit();

    try expect(arrbyte_data.len == arrresult.bytes_read);
    try expect(arrvalue.data.Array.len == 4);
    try expect(arrvalue.kind() == .Array);
    for (0..4, arrvalue.data.Array) |x, y| {
        try expect((x % 2 == 1) == y.data.Bool);
    }
}

test "src/vals.zig readValues" {
    const byte_data = [_]u8{ 0, 0x42, 0x00, 0x00, 0x00, 2, 0x01, 1, 0x41, 0x42, 0x43, 0x44, 0x45, 0x00, 3, 86, 14, 73, 64 };
    const values = try readValues(std.testing.allocator, &byte_data);
    defer {
        for (values.items) |value| {
            value.deinit();
        }
        values.deinit();
    }

    try expect(values.items[0].data.Int == 0x42);
    try expect(values.items[1].data.Bool == true);
    try expect(values.items[2].data.String.len == 5);
    try expect(values.items[3].data.Float == 3.1415);

    try expect(values.items.len == 4);
}

test "src/vals.zig casting" {
    const allocator = std.testing.allocator;

    const strbyte_data = [_]u8{ 1, 65, 66, 67, 0 };
    var strvalue = (try readValue(std.testing.allocator, &strbyte_data)).value;
    defer strvalue.deinit();

    try expectError(error.InvalidCast, strvalue.cast(allocator, ValueType.Bool)); // Str -> Bool

    const boolbyte_data = [_]u8{ 2, 0 };
    var boolvalue = (try readValue(std.testing.allocator, &boolbyte_data)).value;
    defer boolvalue.deinit();

    try expectError(error.InvalidCast, boolvalue.cast(allocator, ValueType.String)); // Bool -> Str
    const intbool = try boolvalue.cast(allocator, ValueType.Int); // Bool -> Int
    try expect(intbool.data.Int == 0);

    const floatbyte_data = [_]u8{ 3, 86, 14, 73, 64 };
    var floatvalue = (try readValue(std.testing.allocator, &floatbyte_data)).value;
    defer floatvalue.deinit();

    var floatstring = try floatvalue.cast(allocator, ValueType.String); // Float -> Str
    defer floatstring.deinit();

    try expect(std.mem.eql(u8, floatstring.data.String, "3.1415"));
}

test "src/vals.zig eql" {
    const byte_data = [_]u8{ 1, 65, 66, 67, 0, 1, 68, 69, 70, 0, 0, 0x42, 0x00, 0x00, 0x00 };
    const values = try readValues(std.testing.allocator, &byte_data);
    defer {
        for (values.items) |v| {
            v.deinit();
        }
        values.deinit();
    }
    try expect(!(try values.items[0].eql(values.items[1])));
    try expect(!(try values.items[0].eql(values.items[2])));
}

test "src/vals.zig arithmetic" {
    const byte_data = [_]u8{ 0, 0x42, 0x00, 0x00, 0x00, 0, 0x03, 0x00, 0x00, 0x00, 3, 86, 14, 73, 64 };
    const values = try readValues(std.testing.allocator, &byte_data);
    defer {
        for (values.items) |v| {
            v.deinit();
        }
        values.deinit();
    }

    const success = try values.items[0].arithmeticOp(values.items[1], .OP_ADD, i32);
    const failure = values.items[1].arithmeticOp(values.items[2], .OP_ADD, i32);

    try expect(success.data.Int == 69);
    try expectError(error.InvalidDataType, failure);
}

test "src/vals.zig comparison" {
    const byte_data = [_]u8{ 0, 0x42, 0x00, 0x00, 0x00, 0, 0x03, 0x00, 0x00, 0x00, 3, 86, 14, 73, 64 };
    const values = try readValues(std.testing.allocator, &byte_data);
    defer {
        for (values.items) |v| {
            v.deinit();
        }
        values.deinit();
    }

    const success = try values.items[0].comparisonOp(values.items[1], .OP_MORE, i32);
    const failure = values.items[1].comparisonOp(values.items[2], .OP_MORE, i32);

    try expect(success.data.Bool);
    try expectError(error.InvalidDataType, failure);
}

test "src/vals.zig logic" {
    const byte_data = [_]u8{ 2, 1, 2, 0, 2, 1 };
    const values = try readValues(std.testing.allocator, &byte_data);
    defer {
        for (values.items) |v| {
            v.deinit();
        }
        values.deinit();
    }

    const op_or = try values.items[0].logicOp(values.items[1], .OP_OR);
    const op_and = try values.items[1].logicOp(values.items[2], .OP_AND);

    try expect(op_or.data.Bool);
    try expect(!op_and.data.Bool);
}
