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

pub const VmError = error {
    InvalidOpcode,
    MismatchedTypes,
    UndefinedVariable,
    MalformedCode
};

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
            .constants = program.constants.items
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

    fn fetchi16(self: *Vm) !i16 {
        var ret: i16 = try self.fetch();
        ret |= @as(i16, try self.fetch()) << 8;
        return ret;
    }

    // scope_indx is relative to local scope where the innermost scope is 0 
    // and scope_indx = n is global scope

    pub fn getvar(self: *Vm, scope_indx: u8, indx: u8) !*Value {
        if (scope_indx >= self.scopes.items.len) return error.MalformedCode;

        var scope = self.current_scope;

        if (scope_indx != 0) scope = &self.scopes.items[self.scopes.items.len-scope_indx-1]; 
        if (indx >= scope.items.len) return error.UndefinedVariable;

        return scope.items[indx];
    }

    pub fn setvar(self: *Vm, v: *Value, scope_indx: u8, indx: u8) !void {
        if (scope_indx >= self.scopes.items.len) return error.MalformedCode;

        var scope = self.current_scope;

        if (scope_indx != 0) scope = &self.scopes.items[self.scopes.items.len-scope_indx-1]; 
        if (indx >= scope.items.len) return error.UndefinedVariable;

        self.release(scope.items[indx]);
        scope.items[indx] = v;
    }

    fn pop(self: *Vm) !*Value {
        return try self.stack.pop();
    }

    fn new_scope(self: *Vm) !void {
        try self.scopes.append(Scope.init(self.allocator));
        self.current_scope = &self.scopes.items[self.scopes.items.len-1];
    }

    fn deinit_scope(self: *Vm) !void {
        // Prevent global scope from getting prematurely ended
        if (self.scopes.items.len == 1) return error.MalformedCode;

        var scope = self.scopes.pop() orelse return error.MalformedCode;
        for (scope.items) |item| {
            self.release(item);
        }
        scope.deinit();
        self.current_scope = &self.scopes.items[self.scopes.items.len-1];
    }

    fn binOp(self: *Vm, op: VmOpcode) !*Value {
        const b = try self.pop();
        const a = try self.pop();
        defer { self.release(b); self.release(a); }

        if (a.kind() != .Int or b.kind() != .Int) return error.MismatchedTypes;

        // TODO: Operations regarding strings

        const ret = Value{
            .allocator = self.allocator,
            .data = switch (op) {
                .OP_ADD => .{ .Int = (a.data.Int + b.data.Int) },
                .OP_SUB => .{ .Int = (a.data.Int - b.data.Int) },
                .OP_MUL => .{ .Int = (a.data.Int * b.data.Int) },
                .OP_DIV => .{ .Int = @divFloor(a.data.Int, b.data.Int) },
                .OP_EQL => .{ .Bool = (a.data.Int == b.data.Int) },
                .OP_NEQL => .{ .Bool = (a.data.Int != b.data.Int) },
                .OP_MORE => .{ .Bool = (a.data.Int > b.data.Int) },
                .OP_LESS => .{ .Bool = (a.data.Int < b.data.Int) },
                else => unreachable
            },
            .refcount = 0,
            .size = switch (op) {
                .OP_ADD, .OP_SUB, .OP_MUL, .OP_DIV => 4,
                .OP_EQL, .OP_NEQL, .OP_MORE, .OP_LESS => 1,
                else => unreachable
            }
        };

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
                    const linevalue = try self.allocator.create(Value);
                    linevalue.* = Value{
                        .allocator = self.allocator,
                        .data = .{ .String = strslice },
                        .size = strslice.len,
                        .refcount = 1,
                    };
                    try self.stack.push(linevalue);
                },

                .OP_CONST => try self.stack.push(try self.constants[try self.fetch()].alloc_copy(self.allocator)),
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

                .OP_POPVAR => try self.setvar(try self.pop(), try self.fetch(), try self.fetch()),

                .OP_ADD, .OP_SUB, .OP_MUL, .OP_DIV, 
                .OP_LESS, .OP_MORE, .OP_EQL, .OP_NEQL => try self.stack.push(try self.binOp(opc)),

                .OP_JMP => try self.jump(try self.fetchi16()),
                .OP_CALL => {
                    const offs: i16 = try self.fetchi16();
                    try self.callstack.push(self.pc);
                    try self.jump(offs);
                },
                .OP_TJMP => {
                    const offs: i16 = try self.fetchi16();
                    const x = try self.stack.pop();
                    defer self.release(x);
                    if (x.kind() != .Bool) return error.MismatchedTypes;
                    if (x.data.Bool) try self.jump(offs);
                },
                .OP_TCALL => {
                    const offs: i16 = try self.fetchi16();
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
                _ => return error.MalformedCode
            }
        }
    }
};

