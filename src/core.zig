const std = @import("std");
const vals = @import("vals.zig");
const stack = @import("stack.zig");
const opcodes = @import("opcodes.zig");
const loader = @import("loader.zig");
const interface = @import("interface.zig");

const Program = loader.Program;

const Value = vals.Value;
const Scope = std.ArrayList(*Value);

const Stack = stack.Stack;
const VmOpcode = opcodes.VmOpcode;

const ValueStack = Stack(*Value);

const VmCtx = interface.VmCtx;
const NativeFunc = *const fn(VmCtx) anyerror!void;

const expectError = std.testing.expectError;
const expect = std.testing.expect;

pub const VmError = error{ InvalidOpcode, MismatchedTypes, UndefinedVariable, MalformedCode };

pub const Vm = struct {
    stack: ValueStack = undefined,
    callstack: stack.Stack(usize),
    pc: usize = 0,

    program: *const Program = undefined,

    current_scope: *Scope = undefined,
    scopes: std.ArrayList(Scope) = undefined,

    constants: []const Value = undefined,

    allocator: std.mem.Allocator = undefined,
    natives: std.ArrayList(NativeFunc) = undefined,
    ctx: VmCtx = undefined,

    pub fn init(allocator: std.mem.Allocator, program: *const Program) !Vm {
        var scopes = std.ArrayList(Scope).init(allocator);

        // Initialize global scope
        try scopes.append(Scope.init(allocator));

        return Vm{
            .program = program,
            .allocator = allocator,
            .pc = 0,
            .stack = try ValueStack.init(allocator),
            .natives = std.ArrayList(NativeFunc).init(allocator),
            .callstack = try stack.Stack(usize).init(allocator),
            .scopes = scopes,
            .current_scope = &scopes.items[0],
            .constants = program.constants.items,
        };
    }

    pub fn deinit(self: *Vm) void {
        for (self.scopes.items) |scope| {
            for (scope.items) |variable| {
                variable.deinit();
                self.allocator.destroy(variable);
            }
            scope.deinit();
        }
        self.scopes.deinit();

        for (self.stack.data[0..self.stack.sp]) |item| {
            item.deinit();
            self.allocator.destroy(item);
        }

        self.natives.deinit();
        self.stack.deinit();
        self.callstack.deinit();
    }

    pub fn registerNative(self: *Vm, func: NativeFunc) !void {
        try self.natives.append(func);
    }

    fn fetch(self: *Vm) !u8 {
        if (self.pc >= self.program.code.len) return error.MalformedCode;
        const x = self.program.code[self.pc];
        self.pc += 1;
        return x;
    }

    fn fetch16(self: *Vm, comptime T: type) !T {
        if (!(T == u16 or T == i16)) @compileError("Vm.fetch16 only supports u16 or i16");
        var ret: u16 = try self.fetch();
        ret |= @as(u16, try self.fetch()) << 8;
        return @bitCast(ret);
    }

    // scope_indx is relative to local scope where the innermost scope is 0
    // and scope_indx = n is global scope

    pub fn getvar(self: *Vm, scope_indx: u8, indx: u8) !*Value {
        if (scope_indx >= self.scopes.items.len) return error.MalformedCode;

        var scope = self.current_scope;

        if (scope_indx != 0) scope = &self.scopes.items[self.scopes.items.len - scope_indx - 1];
        if (indx >= scope.items.len) return error.UndefinedVariable;

        return scope.items[indx];
    }

    pub fn setvar(self: *Vm, v: *Value, scope_indx: u8, indx: u8) !void {
        if (scope_indx >= self.scopes.items.len) return error.MalformedCode;

        var scope = self.current_scope;

        if (scope_indx != 0) scope = &self.scopes.items[self.scopes.items.len - scope_indx - 1];
        if (indx >= scope.items.len) return error.UndefinedVariable;

        self.release(scope.items[indx]);
        scope.items[indx] = v;
    }

    fn pop(self: *Vm) !*Value {
        return try self.stack.pop();
    }

    fn popExpect(self: *Vm, expected: vals.ValueType) !*Value {
        const v = try self.pop();
        if (v.kind() != expected) return error.MismatchedTypes;
        return v;
    }

    fn new_scope(self: *Vm) !void {
        try self.scopes.append(Scope.init(self.allocator));
        self.current_scope = &self.scopes.items[self.scopes.items.len - 1];
    }

    fn deinit_scope(self: *Vm) !void {
        // Prevent global scope from getting prematurely ended
        if (self.scopes.items.len == 1) return error.MalformedCode;

        var scope = self.scopes.pop() orelse return error.MalformedCode;
        for (scope.items) |item| {
            self.release(item);
        }
        scope.deinit();
        self.current_scope = &self.scopes.items[self.scopes.items.len - 1];
    }

    fn binOp(self: *Vm, op: VmOpcode) !*Value {
        const b = try self.pop();
        const a = try self.pop();
        defer { self.release(b); self.release(a); }

        if (a.kind() != b.kind()) return error.MismatchedTypes;
        if (a.kind() == .Array) return error.InvalidDataType;

        const ret = try self.allocator.create(Value);
        ret.* = switch (op) {
            .OP_EQL => Value{ .size = 1, .data = .{ .Bool = try a.eql(b.*) } },
            .OP_NEQL => Value{ .size = 1, .data = .{ .Bool = !(try a.eql(b.*)) } },
            .OP_ADD => switch (a.kind()) {
                .String => try a.concat(b.*, self.allocator),
                .Int => try a.arithmeticOp(b.*, op, i32),
                .Float => try a.arithmeticOp(b.*, op, f32),
                else => return error.MismatchedTypes
            },
            .OP_SUB, .OP_MUL, .OP_DIV, .OP_MOD => switch (a.kind()) {
                .Int => try a.arithmeticOp(b.*, op, i32),
                .Float => try a.arithmeticOp(b.*, op, f32),
                else => return error.MismatchedTypes
            },
            .OP_MORE, .OP_LESS, .OP_EQMORE, .OP_EQLESS => switch (a.kind()) {
                .Int => try a.comparisonOp(b.*, op, i32),
                .Float => try a.arithmeticOp(b.*, op, f32),
                else => return error.MismatchedTypes
            },
            .OP_AND, .OP_OR => try a.logicOp(b.*, op),
            else => unreachable
        };
        ret.refcount = 1;

        return ret;
    }

    fn jump(self: *Vm, offs: i16) !void {
        var newaddr: isize = @intCast(self.pc);
        newaddr += @intCast(offs);
        if (newaddr < 0 or newaddr >= self.program.code.len) return error.MalformedCode;
        self.pc = @intCast(newaddr);
    }

    fn release(self: *Vm, x: *Value) void {
        if (x.release()) self.allocator.destroy(x);
    }

    fn buildArrays(self: *Vm, sizes: []const u16) !*Value {
        if (sizes.len == 0) return error.MalformedCode;

        const arrsize: u16 = sizes[0];
        const ret = try self.reserveArray(arrsize);
        if (sizes.len == 1) return ret;

        for (ret.data.Array) |*x| {
            const elem = try self.buildArrays(sizes[1..]);
            x.* = elem.*;
            self.allocator.destroy(elem);
        }
        return ret;
    }

    // This function would not make copies of the data it gets.
    // It would assume that any []Value or []u8 are already heap allocated and use the VM's allocator.
    fn makeValue(self: *Vm, data: vals.ValueData) !*Value {
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

    fn reserveArray(self: *Vm, size: u16) !*Value {
        const data = vals.ValueData{ .Array = try self.allocator.alloc(Value, @intCast(size)) };
        for (data.Array) |*x| {
            x.* = Value{
                .allocator = self.allocator,
                .data = .{.Int = 0},
                .size = 4
            };
        }
        return self.makeValue(data);
    }

    fn getND(_: *Vm, root: *Value, indices: []const u16) !*Value {
        if (indices.len == 0) return error.MalformedCode;
        var current = root;
        for (indices) |i| {
            if (current.kind() != .Array) return error.MismatchedTypes;
            current = &current.data.Array[i];
        }
        return current;
    }

    fn setND(_: *Vm, root: *Value, indices: []const u16, v: *Value) !void {
        if (indices.len == 0) return error.MalformedCode;
        var current = root;

        for (indices[0..indices.len-1]) |i| {
            if (current.kind() != .Array) return error.MismatchedTypes;
            current = &current.data.Array[i];
        }

        if (current.kind() != .Array) return error.MismatchedTypes;
        try current.setAt(try v.copy(), indices[indices.len-1]);
    }

    pub fn step(self: *Vm, writer: anytype) !bool {
        const opc = std.meta.intToEnum(VmOpcode, try self.fetch()) catch return error.InvalidOpcode;
        switch (opc) {
            .OP_DUMP => {
                var x = try self.pop();
                defer self.release(x);
                try x.dump(writer);
            },

            .OP_NATIVE => {
                const ctx = VmCtx{
                    .allocator = self.allocator,
                    .vm_stack = &self.stack
                };
                const func_idx = try self.fetch();
                if (func_idx >= self.natives.items.len) return error.MalformedCode;
                try self.natives.items[func_idx](ctx);
            },

            .OP_CAST => {
                const targetType = std.meta.intToEnum(vals.ValueType, try self.fetch()) catch return error.InvalidCast;
                const old_v = try self.pop();
                defer self.release(old_v);
                const new_v = try old_v.cast(self.allocator, targetType);

                try self.stack.push(try new_v.alloc_copy(self.allocator));
            },

            .OP_CONST => try self.stack.push(try self.constants[self.fetch() catch return error.MalformedCode].alloc_copy(self.allocator)),
            .OP_COPY => try self.stack.push(try self.stack.top().?.alloc_copy(self.allocator)),

            .OP_STARTSCOPE => try self.new_scope(),
            .OP_ENDSCOPE => try self.deinit_scope(),

            .OP_DEFVAR => {
                // We do not touch refcount here as popping and adding it to scope would cancel out
                try self.current_scope.append(try self.pop());
            },

            .OP_PUSHVAR => {
                const v = try self.getvar(try self.fetch(), try self.fetch());
                v.refcount += 1;
                try self.stack.push(v);
            },

            .OP_DROP => {
                const v = try self.pop();
                self.release(v);
            },

            .OP_DUP => {
                const v = self.stack.top() orelse return error.MalformedCode;
                v.refcount += 1;
                try self.stack.push(v);
            },

            .OP_SWAP => {
                // This would not touch the refcounts since we do not consume the values themselves.
                const a = try self.pop();
                const b = try self.pop();
                try self.stack.push(a);
                try self.stack.push(b);
            },

            .OP_CREATEARRAY => try self.stack.push(try self.reserveArray(try self.fetch16(u16))),
            .OP_CREATEARRAY_ND => {
                const depth: u16 = try self.fetch16(u16);
                const sizes = try self.allocator.alloc(u16, depth);
                for (sizes) |*size| {
                    size.* = try self.fetch16(u16);
                }
                try self.stack.push(try self.buildArrays(sizes));
            },
            .OP_INITARRAY => {
                const arrsize: u16 = try self.fetch16(u16);
                const arrvalue = try self.reserveArray(arrsize);

                for (0..arrsize) |i| {
                    const v = try self.pop();
                    defer self.release(v);
                    arrvalue.data.Array[arrsize - i - 1] = try v.copy();
                }

                try self.stack.push(arrvalue);
            },

            .OP_RGET => {
                const indx = try self.popExpect(.Int);
                const arr = try self.popExpect(.Array);
                defer {
                    self.release(indx);
                    self.release(arr);
                }
                try self.stack.push(try (try arr.at(indx.data.Int)).alloc_copy(self.allocator));
            },

            .OP_RSET => {
                const v = try self.pop();
                const indx = try self.popExpect(.Int);
                const arr = try self.popExpect(.Array);
                defer {
                    self.release(v);
                    self.release(indx);
                    self.release(arr);
                }

                try arr.setAt(try v.copy(), indx.data.Int);
            },

            .OP_CGET => {
                const arr = try self.popExpect(.Array);
                defer self.release(arr);
                try self.stack.push(try (try arr.at(try self.fetch16(u16))).alloc_copy(self.allocator));
            },

            .OP_CSET => {
                const v = try self.pop();
                const arr = try self.popExpect(.Array);
                defer {
                    self.release(v);
                    self.release(arr);
                }
                try arr.setAt(try v.copy(), try self.fetch16(u16));
            },

            .OP_RGET_ND => {
                const arr = try self.popExpect(.Array);
                defer self.release(arr);
                const depth = try self.fetch16(u16);
                const indices = try self.allocator.alloc(u16, depth);
                defer self.allocator.free(indices);

                for (indices, 0..) |_, i| {
                    const indx_val = try self.popExpect(.Int);
                    const indx_int = indx_val.data.Int;
                    if (indx_int < 0) return error.MalformedCode;
                    indices[depth - 1 - i] = @intCast(indx_int);
                }

                const v = try self.getND(arr, indices);
                try self.stack.push(try v.alloc_copy(self.allocator));
            },

            .OP_RSET_ND => {
                const v = try self.pop();
                defer self.release(v);
                const arr = try self.popExpect(.Array);
                defer self.release(arr);
                const depth = try self.fetch16(u16);
                const indices = try self.allocator.alloc(u16, depth);
                defer self.allocator.free(indices);
                for (indices, 0..) |_, i| {
                    const indx_val = try self.popExpect(.Int);
                    const indx_int = indx_val.data.Int;
                    if (indx_int < 0) return error.MalformedCode;
                    indices[depth - 1 - i] = @intCast(indx_int);
                }
                try self.setND(arr, indices, v);
            },

            .OP_SIZE => {
                const top = try self.pop();
                defer self.release(top);

                try self.stack.push(try self.makeValue(.{ .Int = @intCast(top.size) }));
            },

            .OP_POPVAR => try self.setvar(try self.pop(), try self.fetch(), try self.fetch()),

            .OP_ADD, .OP_SUB, .OP_MUL, .OP_DIV, .OP_MOD, 
            .OP_LESS, .OP_MORE, .OP_EQMORE, .OP_EQLESS, 
            .OP_EQL, .OP_NEQL, .OP_AND, .OP_OR => try self.stack.push( try self.binOp(opc) ),

            .OP_NOT => {
                const old_v = try self.popExpect(.Bool);
                defer self.release(old_v);
                const new_v = try self.makeValue(.{.Bool = !old_v.data.Bool});
                try self.stack.push(new_v);
            },

            .OP_NEG => {
                const old_v = try self.pop();
                defer self.release(old_v);
                const payload: vals.ValueData = switch (old_v.data) {
                    .Int => |x| .{ .Int = x * -1 } ,
                    .Float => |x| .{ .Float = x * -1.0},
                    else => return error.InvalidDataType
                };
                const new_v = try self.makeValue(payload);
                try self.stack.push(new_v);
            },

            .OP_JMP => try self.jump(try self.fetch16(i16)),
            .OP_CALL => {
                const offs: i16 = try self.fetch16(i16);
                try self.callstack.push(self.pc);
                try self.jump(offs);
            },
            .OP_TJMP => {
                const offs: i16 = try self.fetch16(i16);
                const x = try self.stack.pop();
                defer self.release(x);
                if (x.kind() != .Bool) return error.MismatchedTypes;
                if (x.data.Bool) try self.jump(offs);
            },
            .OP_TCALL => {
                const offs: i16 = try self.fetch16(i16);
                const x = try self.stack.pop();
                defer self.release(x);
                if (x.kind() != .Bool) return error.MismatchedTypes;
                if (x.data.Bool) {
                    try self.callstack.push(self.pc);
                    try self.jump(offs);
                }
            },

            .OP_RET => {
                // Return if there is an address on the callstack, but quit if there is none
                self.pc = self.callstack.pop() catch return false;
            },
            else => return error.MalformedCode,
        }
        return true;
    }

    pub fn run(self: *Vm, writer: anytype) !void {
        while (try self.step(writer)) {}
    }
};

// Creates Vm for testing. Provides constants but not code.

pub const TestVM = struct {
    program: *const Program = undefined,
    vm: Vm = undefined,
    allocator: std.mem.Allocator = undefined,
};

pub fn makeTestVm(allocator: std.mem.Allocator, code: []const u8) !TestVM {
    const const_data = [_]u8{0, 36, 0x00, 0x00, 0x00, 0, 33, 0x00, 0x00, 0x00, 1, 0x41, 0x42, 0x43, 0x44, 0x45, 0x00};
    const program = try allocator.create(Program);
    program.* = try Program.init(allocator, &const_data, code);

    return .{
        .vm = try Vm.init(allocator, program),
        .program = program,
        .allocator = allocator,
    };
}

test "src/core.zig jump" {
    const dummy_prog = [_]u8{@intFromEnum(VmOpcode.OP_RET)} ** 10;
    const res = try makeTestVm(std.testing.allocator, &dummy_prog);
    var vm = res.vm;
    defer vm.deinit();
    defer { res.program.deinit(); res.allocator.destroy(res.program); }
    
    const err = vm.jump(-1); // pc = 0; pc -> -1 MalformedCode
    try expectError(error.MalformedCode, err);
    try vm.jump(1); // pc = 0; pc -> 1 Noerr
    try expect(vm.pc == 1);
}

test "src/core.zig fetches" {
    const dummy_prog = [_]u8{
        0xbb, 0xaa, // 0xaabb - unsigned fetch
        0xff, 0xff, // -1 - signed fetch16
        0x00, // fetch16 should fail
    };
    const res = try makeTestVm(std.testing.allocator, &dummy_prog);
    var vm = res.vm;
    defer vm.deinit();
    defer { res.program.deinit(); res.allocator.destroy(res.program); }

    try expect(try vm.fetch16(u16) == 0xaabb);
    try expect(try vm.fetch16(i16) == -1);

    _ = try vm.fetch();
    try expectError(error.MalformedCode, vm.fetch());
}

test "src/core.zig deinit_scope" {
    const dummy_prog = [_]u8{@intFromEnum(VmOpcode.OP_RET)} ** 10;
    const res = try makeTestVm(std.testing.allocator, &dummy_prog);
    var vm = res.vm;
    defer vm.deinit();
    defer { res.program.deinit(); res.allocator.destroy(res.program); }

    try vm.new_scope();
    try vm.deinit_scope();
    const err = vm.deinit_scope(); // Try to deinit global scope
    try expectError(error.MalformedCode, err);

}

test "src/core.zig vars" {
    const dummy_prog = [_]u8{@intFromEnum(VmOpcode.OP_RET)} ** 10;
    const res = try makeTestVm(std.testing.allocator, &dummy_prog);
    var vm = res.vm;
    defer vm.deinit();
    defer { res.program.deinit(); res.allocator.destroy(res.program); }

    const val = vm.constants[0];

    try vm.current_scope.append(try val.alloc_copy(std.testing.allocator)); // Equivalent to OP_CONST 0; OP_DEFVAR

    const vcopy = try val.alloc_copy(std.testing.allocator);
    try vm.setvar(vcopy, 0, 0);
    try expect( try vm.getvar(0, 0) == vcopy );
    try vm.new_scope();
    try expectError(error.MalformedCode, vm.getvar(3, 0));
    try expectError(error.UndefinedVariable, vm.getvar(1, 1));
    // These failing getvar calls test setvar calls because they use identical logic for resolving variables.
    try vm.deinit_scope();
}


test "src/core.zig getND, setND usage" {
    const dummy_prog = [_]u8{@intFromEnum(VmOpcode.OP_RET)} ** 10;
    const res = try makeTestVm(std.testing.allocator, &dummy_prog);
    var vm = res.vm;
    defer vm.deinit();
    defer {
        res.program.deinit();
        res.allocator.destroy(res.program);
    }

    const arr = try vm.buildArrays(&[_]u16{3,2});
    defer vm.release(arr);

    const val = try vm.makeValue(.{.Int = 123});
    defer vm.release(val);

    try vm.setND(arr, &[_]u16{1,0}, val);
    const ptr = try vm.getND(arr, &[_]u16{1,0});
    try expect(ptr.data.Int == 123);
}

