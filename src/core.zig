const std = @import("std");
const vals = @import("vals.zig");
const stack = @import("stack.zig");
const opcodes = @import("opcodes.zig");
const loader = @import("loader.zig");

const Program = loader.Program;

const Value = vals.Value;
const Scope = std.ArrayList(*Value);

const Stack = stack.Stack;
const VmOpcode = opcodes.VmOpcode;

const ValueStack = Stack(*Value);

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

    pub fn init(allocator: std.mem.Allocator, program: *const Program) !Vm {
        var scopes = std.ArrayList(Scope).init(allocator);

        // Initialize global scope
        try scopes.append(Scope.init(allocator));

        return Vm{
            .program = program,
            .allocator = allocator,
            .pc = 0,
            .stack = try ValueStack.init(allocator),
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

        self.stack.deinit();
        self.callstack.deinit();
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
        defer {
            self.release(b);
            self.release(a);
        }

        if (a.kind() != .Int or b.kind() != .Int) return error.MismatchedTypes;

        // TODO: Operations regarding strings

        const ret = Value{ .allocator = self.allocator, .data = switch (op) {
            .OP_ADD => .{ .Int = (a.data.Int + b.data.Int) },
            .OP_SUB => .{ .Int = (a.data.Int - b.data.Int) },
            .OP_MUL => .{ .Int = (a.data.Int * b.data.Int) },
            .OP_DIV => .{ .Int = @divFloor(a.data.Int, b.data.Int) },
            .OP_EQL => .{ .Bool = (a.data.Int == b.data.Int) },
            .OP_NEQL => .{ .Bool = (a.data.Int != b.data.Int) },
            .OP_MORE => .{ .Bool = (a.data.Int > b.data.Int) },
            .OP_LESS => .{ .Bool = (a.data.Int < b.data.Int) },
            else => unreachable,
        }, .refcount = 0, .size = switch (op) {
            .OP_ADD, .OP_SUB, .OP_MUL, .OP_DIV => 4,
            .OP_EQL, .OP_NEQL, .OP_MORE, .OP_LESS => 1,
            else => unreachable,
        } };

        return ret.alloc_copy(self.allocator);
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

    // This function would not make copies of the data it gets.
    // It would assume that any []Value or []u8 are already heap allocated and use the VM's allocator.
    fn makeValue(self: *Vm, data: vals.ValueData) !*Value {
        const ret = try self.allocator.create(Value);
        ret.* = Value{
            .data = data,
            .allocator = self.allocator,
            .refcount = 1,
            .size = switch(data) {
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
        return self.makeValue(data);
    }
    
    pub fn run(self: *Vm, reader: anytype) !void {
        while (true) {
            const opc: VmOpcode = @enumFromInt(try self.fetch());
            switch (opc) {
                .OP_DUMP => {
                    var x = try self.pop();
                    defer self.release(x);
                    x.dump();
                },

                .OP_INPUT => {
                    var line = std.ArrayList(u8).init(self.allocator);
                    try reader.streamUntilDelimiter(line.writer(), '\n', null);
                    const strslice = try line.toOwnedSlice();
                    try self.stack.push(try self.makeValue(.{.String = strslice}));
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
                .OP_INITARRAY => {
                    const arrsize: u16 = try self.fetch16(u16);
                    const arrvalue = try self.reserveArray(arrsize);

                    for (0..arrsize) |i| {
                        const v = try self.pop();
                        defer self.release(v);
                        arrvalue.data.Array[arrsize-i-1] = try v.copy();
                    }

                    try self.stack.push(arrvalue);
                },

                .OP_RGET => {
                    const indx = try self.popExpect(.Int);
                    const arr = try self.popExpect(.Array);
                    defer { self.release(indx); self.release(arr); }
                    try self.stack.push(try (try arr.at(indx.data.Int)).alloc_copy(self.allocator));
                },

                .OP_RSET => {
                    const v = try self.pop();
                    const indx = try self.popExpect(.Int);
                    const arr = try self.popExpect(.Array);
                    defer { self.release(v); self.release(indx); self.release(arr); }

                    try arr.setAt(try v.copy(), indx.data.Int);
                },

                .OP_CGET => {
                    const arr = try self.popExpect(.Array);
                    defer self.release(arr);
                    try self.stack.push(try (try arr.at(try self.fetch16(u16))).alloc_copy(self.allocator) );
                },

                .OP_CSET => {
                    const v = try self.pop();
                    const arr = try self.popExpect(.Array);
                    defer { self.release(v); self.release(arr); }
                    try arr.setAt(try v.copy(), try self.fetch16(u16));
                },

                .OP_SIZE => {
                    const top = try self.pop();
                    defer self.release(top);

                    try self.stack.push(try self.makeValue(.{.Int = @intCast(top.size)}));
                },
                
                .OP_POPVAR => try self.setvar(try self.pop(), try self.fetch(), try self.fetch()),

                .OP_ADD, .OP_SUB, .OP_MUL, .OP_DIV, .OP_LESS, .OP_MORE, .OP_EQL, .OP_NEQL => try self.stack.push(try self.binOp(opc)),

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
                    self.pc = self.callstack.pop() catch return;
                },
                else => return error.MalformedCode,
            }
        }
    }
};
