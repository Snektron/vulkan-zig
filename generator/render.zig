const std = @import("std");
const reg = @import("registry.zig");
const util = @import("render/util.zig");
const cparse = @import("registry/c-parse.zig");
const mem = std.mem;
const Allocator = mem.Allocator;

const preamble =
    \\const std = @import("std");
    \\const builtin = @import("builtin");
    \\const root = @import("root");
    \\
    \\pub const vulkan_call_conv: builtin.CallingConvention = if (builtin.os.tag == .windows)
    \\        .Stdcall
    \\    else if (builtin.abi == .android and (builtin.cpu.arch.isARM() or builtin.cpu.arch.isThumb()) and builtin.Target.arm.featureSetHas(builtin.cpu.features, .has_v7) and builtin.cpu.arch.ptrBitWidth() == 32)
    \\        // On Android 32-bit ARM targets, Vulkan functions use the "hardfloat"
    \\        // calling convention, i.e. float parameters are passed in registers. This
    \\        // is true even if the rest of the application passes floats on the stack,
    \\        // as it does by default when compiling for the armeabi-v7a NDK ABI.
    \\        .AAPCSVFP
    \\    else
    \\        .C;
    \\
    ;

const builtin_types = std.ComptimeStringMap([]const u8, .{
    .{"void", @typeName(void)},
    .{"char", @typeName(u8)},
    .{"float", @typeName(f32)},
    .{"double", @typeName(f64)},
    .{"uint8_t", @typeName(u8)},
    .{"uint16_t", @typeName(u16)},
    .{"uint32_t", @typeName(u32)},
    .{"uint64_t", @typeName(u64)},
    .{"int32_t", @typeName(i32)},
    .{"int64_t", @typeName(i64)},
    .{"size_t", @typeName(usize)},
    .{"int", @typeName(c_int)},
});

const foreign_types = std.ComptimeStringMap([]const u8, .{
    .{"Display", "@Type(.Opaque)"},
    .{"VisualID", @typeName(c_uint)},
    .{"Window", @typeName(c_ulong)},
    .{"RROutput", @typeName(c_ulong)},
    .{"wl_display", "@Type(.Opaque)"},
    .{"wl_surface", "@Type(.Opaque)"},
    .{"HINSTANCE", "std.os.HINSTANCE"},
    .{"HWND", "*@Type(.Opaque)"},
    .{"HMONITOR", "*@Type(.Opaque)"},
    .{"HANDLE", "std.os.HANDLE"},
    .{"SECURITY_ATTRIBUTES", "std.os.SECURITY_ATTRIBUTES"},
    .{"DWORD", "std.os.DWORD"},
    .{"LPCWSTR", "std.os.LPCWSTR"},
    .{"xcb_connection_t", "@Type(.Opaque)"},
    .{"xcb_visualid_t", @typeName(u32)},
    .{"xcb_window_t", @typeName(u32)},
    .{"zx_handle_t", @typeName(u32)},
});

fn eqlIgnoreCase(lhs: []const u8, rhs: []const u8) bool {
    if (lhs.len != rhs.len) {
        return false;
    }

    for (lhs) |c, i| {
        if (std.ascii.toLower(c) != std.ascii.toLower(rhs[i])) {
            return false;
        }
    }

    return true;
}

fn Renderer(comptime WriterType: type) type {
    return struct {
        const Self = @This();
        const WriteError = WriterType.Error;
        const RenderTypeInfoError = WriteError || error {
            OutOfMemory,
        };

        writer: WriterType,
        allocator: *Allocator,
        registry: *const reg.Registry,
        id_renderer: util.IdRenderer,

        fn init(writer: WriterType, allocator: *Allocator, registry: *const reg.Registry) Self {
            return .{
                .writer = writer,
                .allocator = allocator,
                .registry = registry,
                .id_renderer = util.IdRenderer.init(allocator, registry.tags),
            };
        }

        fn deinit(self: Self) void {
            self.id_renderer.deinit();
        }

        fn writeIdentifier(self: Self, id: []const u8) !void {
            try util.writeIdentifier(self.writer, id);
        }

        fn writeIdentifierWithCase(self: *Self, case: util.CaseStyle, id: []const u8) !void {
            try self.id_renderer.render(self.writer, case, id);
        }

        fn extractEnumFieldName(self: Self, enum_name: []const u8, field_name: []const u8) ![]const u8 {
            const tag = util.getAuthorTag(enum_name, self.registry.tags);
            const adjusted_enum_name = if (tag) |name|
                    enum_name[0 .. enum_name.len - name.len]
                else
                    enum_name;

            var enum_it = util.SegmentIterator.init(adjusted_enum_name);
            var field_it = util.SegmentIterator.init(field_name);

            while (true) {
                const rest = field_it.rest();
                const enum_segment = enum_it.next() orelse return rest;
                const field_segment = field_it.next() orelse return error.FieldNameEqualsEnumName;

                if (!eqlIgnoreCase(enum_segment, field_segment)) {
                    return rest;
                }
            }
        }

        fn render(self: *Self) !void {
            try self.writer.writeAll(preamble);

            for (self.registry.api_constants) |api_constant| {
                try self.renderApiConstant(api_constant);
            }

            for (self.registry.decls) |decl| {
               try self.renderDecl(decl);
            }
        }

        fn renderApiConstant(self: *Self, api_constant: reg.ApiConstant) !void {
            try self.writer.writeAll("const ");
            try self.writeIdentifier(util.trimVkNamespace(api_constant.name));
            try self.writer.writeAll(" = ");

            if (api_constant.value == .alias) {
                try self.writeIdentifier(util.trimVkNamespace(api_constant.value.alias));
                try self.writer.writeAll(";\n");
                return;
            }

            const expr = api_constant.value.expr;
            const adjusted_expr = if (expr.len > 2 and expr[0] == '(' and expr[expr.len - 1] == ')')
                    expr[1 .. expr.len - 1]
                else
                    expr;

            var tokenizer = cparse.CTokenizer{.source = adjusted_expr};
            var peeked: ?cparse.Token = null;
            while (true) {
                const tok = peeked orelse (try tokenizer.next()) orelse break;
                peeked = null;

                switch (tok.id) {
                    .lparen, .rparen, .tilde, .minus, .id => {
                        try self.writer.writeAll(tok.text);
                        continue;
                    },
                    .int => {},
                    else => return error.InvalidApiConstant,
                }

                const suffix = (try tokenizer.next()) orelse {
                    try self.writer.writeAll(tok.text);
                    break;
                };

                switch (suffix.id) {
                    .id => {
                        if (mem.eql(u8, suffix.text, "ULL")) {
                            try self.writer.print("@as(u64, {})", .{tok.text});
                        } else if (mem.eql(u8, suffix.text, "U")) {
                            try self.writer.print("@as(u32, {})", .{tok.text});
                        } else {
                            return error.InvalidApiConstant;
                        }
                    },
                    .dot => {
                        const decimal = (try tokenizer.next()) orelse return error.InvalidConstantExpr;
                        try self.writer.print("@as(f32, {}.{})", .{tok.text, decimal.text});

                        const f = (try tokenizer.next()) orelse return error.InvalidConstantExpr;
                        if (f.id != .id or !mem.eql(u8, f.text, "f")) {
                            return error.InvalidApiConstant;
                        }
                    },
                    else => {
                        try self.writer.writeAll(tok.text);
                        peeked = suffix;
                    },
                }
            }

            try self.writer.writeAll(";\n");
        }

        fn renderTypeInfo(self: *Self, type_info: reg.TypeInfo) RenderTypeInfoError!void {
            switch (type_info) {
                .name => |name| try self.renderTypeName(name),
                .command_ptr => |command_ptr| try self.renderCommandPtr(command_ptr),
                .pointer => |pointer| try self.renderPointer(pointer),
                .array => |array| try self.renderArray(array),
            }
        }

        fn renderTypeName(self: *Self, name: []const u8) !void {
            if (builtin_types.get(name)) |zig_name| {
                try self.writer.writeAll(zig_name);
                return;
            }

            if (mem.startsWith(u8, name, "vk")) {
                // Function type, always render with the exact same text for linking purposes.
                try self.writeIdentifier(name);
                return;
            } else if (mem.startsWith(u8, name, "Vk")) {
                // Type, strip namespace and write, as they are alreay in title case.
                try self.writeIdentifier(name[2..]);
                return;
            } else if (mem.startsWith(u8, name, "PFN_vk")) {
                // Function pointer type, render using same name for now
                try self.writeIdentifier(name);
                return;
            }

            try self.writeIdentifier(name);
        }

        fn renderCommandPtr(self: *Self, command_ptr: reg.Command) !void {
            try self.writer.writeAll("?fn(");
            for (command_ptr.params) |param| {
                try self.writeIdentifierWithCase(.snake, param.name);
                try self.writer.writeAll(": ");
                try self.renderTypeInfo(param.param_type);
                try self.writer.writeAll(", ");
            }
            try self.writer.writeAll(") callconv(vulkan_call_conv)");
            try self.renderTypeInfo(command_ptr.return_type.*);
        }

        fn renderPointer(self: *Self, pointer: reg.Pointer) !void {
            const child_is_void = pointer.child.* == .name and mem.eql(u8, pointer.child.name, "void");

            if (pointer.is_optional) {
                try self.writer.writeByte('?');
            }

            const size = if (child_is_void) .one else pointer.size;
            switch (size) {
                .one => try self.writer.writeByte('*'),
                .many => try self.writer.writeAll("[*]"),
                .zero_terminated => try self.writer.writeAll("[*:0]"),
            }

            if (pointer.is_const) {
                try self.writer.writeAll("const ");
            }

            if (child_is_void) {
                try self.writer.writeAll("c_void");
            } else {
                try self.renderTypeInfo(pointer.child.*);
            }
        }

        fn renderArray(self: *Self, array: reg.Array) !void {
            try self.writer.writeByte('[');
            switch (array.size) {
                .int => |size| try self.writer.print("{}", .{size}),
                .alias => |alias| try self.writeIdentifier(util.trimVkNamespace(alias)),
            }
            try self.writer.writeByte(']');
            try self.renderTypeInfo(array.child.*);
        }

        fn renderDecl(self: *Self, decl: reg.Declaration) !void {
            switch (decl.decl_type) {
                .container => |container| try self.renderContainer(decl.name, container),
                .enumeration => |enumeration| try self.renderEnumeration(decl.name, enumeration),
                .bitmask => {},
                .handle => |handle| try self.renderHandle(decl.name, handle),
                .command => {},
                .alias => |alias| try self.renderAlias(decl.name, alias),
                .foreign => |foreign| try self.renderForeign(decl.name, foreign),
                .typedef => |type_info| try self.renderTypedef(decl.name, type_info),
                .opaque => try self.renderOpaque(decl.name),
            }
        }

        fn renderContainer(self: *Self, name: []const u8, container: reg.Container) !void {
            try self.writer.writeAll("const ");
            try self.renderTypeName(name);
            try self.writer.writeAll(" = ");

            for (container.fields) |field| {
                if (field.bits != null) {
                    try self.writer.writeAll("packed ");
                    break;
                }
            } else {
                try self.writer.writeAll("extern ");
            }

            if (container.is_union) {
                try self.writer.writeAll("union {");
            } else {
                try self.writer.writeAll("struct {");
            }

            for (container.fields) |field| {
                try self.writeIdentifierWithCase(.snake, field.name);
                try self.writer.writeAll(": ");
                if (field.bits) |bits| {
                    try self.writer.print(" u{},", .{bits});
                    if (field.field_type != .name or builtin_types.get(field.field_type.name) == null) {
                        try self.writer.writeAll("// ");
                        try self.renderTypeInfo(field.field_type);
                        try self.writer.writeByte('\n');
                    }
                } else {
                    try self.renderTypeInfo(field.field_type);
                    try self.renderContainerDefaultField(name, field);
                    try self.writer.writeAll(", ");
                }
            }

            try self.writer.writeAll("};\n");
        }

        fn renderContainerDefaultField(self: *Self, name: []const u8, field: reg.Container.Field) !void {
            if (mem.eql(u8, field.name, "pNext")) {
                try self.writer.writeAll(" = null");
            } else if (mem.eql(u8, field.name, "sType")) {
                try self.writer.writeAll(" = .");
                try self.writeIdentifierWithCase(.snake, util.trimVkNamespace(name));
            }
        }

        fn renderEnumeration(self: *Self, name: []const u8, enumeration: reg.Enum) !void {
            // TODO: Handle bitmasks
            try self.writer.writeAll("const ");
            try self.renderTypeName(name);
            try self.writer.writeAll(" = extern enum {");

            for (enumeration.fields) |field| {
                if (field.value == .alias) {
                    continue;
                }

                try self.writeIdentifierWithCase(.snake, try self.extractEnumFieldName(name, field.name));

                switch (field.value) {
                    .int => |int| try self.writer.print(" = {}, ", .{int}),
                    .bitpos => |pos| try self.writer.print(" = 1 << {}, ", .{pos}),
                    .bit_vector => |value| try self.writer.print(" = 0x{X}, ", .{value}),
                    .alias => unreachable,
                }
            }

            for (enumeration.fields) |field| {
                if (field.value != .alias or field.value.alias.is_compat_alias) {
                    continue;
                }

                try self.writer.writeAll("pub const ");
                try self.writeIdentifierWithCase(.snake, try self.extractEnumFieldName(name, field.name));
                try self.writer.writeAll(" = ");
                try self.writeIdentifierWithCase(.snake, try self.extractEnumFieldName(name, field.value.alias.name));
                try self.writer.writeAll(";");
            }

            try self.writer.writeAll("};\n");
        }

        fn renderHandle(self: *Self, name: []const u8, handle: reg.Handle) !void {
            const backing_type: []const u8 = if (handle.is_dispatchable) "usize" else "u64";

            try self.writer.writeAll("const ");
            try self.renderTypeName(name);
            try self.writer.print(" = extern enum({}) {{null_handle = 0, _}};\n", .{backing_type});
        }

        fn renderAlias(self: *Self, name: []const u8, alias: reg.Alias) !void {
            if (alias.target == .other_command) {
                return; // TODO: Decide on how to tackle commands
            }

            try self.writer.writeAll("const ");
            try self.renderTypeName(name);
            try self.writer.writeAll(" = ");
            try self.renderTypeName(alias.name);
            try self.writer.writeAll(";\n");
        }

        fn renderOpaque(self: *Self, name: []const u8) !void {
            try self.writer.writeAll("const ");
            try self.renderTypeName(name);
            try self.writer.writeAll(" = @Type(.Opaque);\n");
        }

        fn renderForeign(self: *Self, name: []const u8, foreign: reg.Foreign) !void {
            if (mem.eql(u8, foreign.depends, "vk_platform")) {
                return; // Skip built-in types, they are handled differently
            }

            try self.writer.writeAll("const ");
            try self.writeIdentifier(name);
            try self.writer.print(" = if (@hasDecl(root, \"{}\")) root.", .{name});
            try self.writeIdentifier(name);
            try self.writer.writeAll(" else ");

            if (foreign_types.get(name)) |default| {
                try self.writer.writeAll(default);
                try self.writer.writeAll(";\n");
            } else {
                try self.writer.print("@compileError(\"Missing type definition of '{}'\");\n", .{name});
            }
        }

        fn renderTypedef(self: *Self, name: []const u8, type_info: reg.TypeInfo) !void {
            try self.writer.writeAll("const ");
            try self.renderTypeName(name);
            try self.writer.writeAll(" = ");
            try self.renderTypeInfo(type_info);
            try self.writer.writeAll(";\n");
        }
    };
}

pub fn render(writer: var, allocator: *Allocator, registry: *const reg.Registry) !void {
    var renderer = Renderer(@TypeOf(writer)).init(writer, allocator, registry);
    defer renderer.deinit();

    try renderer.render();
}
