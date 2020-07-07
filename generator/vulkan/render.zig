const std = @import("std");
const reg = @import("registry.zig");
const util = @import("util.zig");
const cparse = @import("c-parse.zig");
const mem = std.mem;
const Allocator = mem.Allocator;

const preamble =
    \\
    \\// This file is generated from the Khronos Vulkan XML API Registry
    \\
    \\const std = @import("std");
    \\const builtin = @import("builtin");
    \\const root = @import("root");
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
    \\pub fn FlagsMixin(comptime FlagsType: type) type {
    \\    return struct {
    \\        pub const IntType = Flags;
    \\        pub fn toInt(self: FlagsType) IntType {
    \\            return @bitCast(IntType, self);
    \\        }
    \\        pub fn fromInt(flags: IntType) FlagsType {
    \\            return @bitCast(FlagsType, flags);
    \\        }
    \\        pub fn merge(lhs: FlagsType, rhs: FlagsType) FlagsType {
    \\            return fromInt(toInt(lhs) | toInt(rhs));
    \\        }
    \\        pub fn intersect(lhs: FlagsType, rhs: FlagsType) FlagsType {
    \\            return fromInt(toInt(lhs) & toInt(rhs));
    \\        }
    \\        pub fn complement(self: FlagsType) FlagsType {
    \\            return fromInt(~toInt(lhs));
    \\        }
    \\        pub fn subtract(lhs: FlagsType, rhs: FlagsType) FlagsType {
    \\            return fromInt(toInt(lhs) & toInt(rhs.complement()));
    \\        }
    \\        pub fn contains(lhs: FlagsType, rhs: FlagsType) bool {
    \\            return toInt(intersect(lhs, rhs)) == toInt(rhs);
    \\        }
    \\    };
    \\}
    \\pub fn makeVersion(major: u10, minor: u10, patch: u12) u32 {
    \\    return (@as(u32, major) << 22) | (@as(u32, minor) << 12) | patch;
    \\}
    \\pub fn versionMajor(version: u32) u10 {
    \\    return @truncate(u10, version >> 22);
    \\}
    \\pub fn versionMinor(version: u32) u10 {
    \\    return @truncate(u10, version >> 12);
    \\}
    \\pub fn versionPatch(version: u32) u12 {
    \\    return @truncate(u12, version);
    \\}
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

        const BitflagName = struct {
            /// Name without FlagBits, so VkSurfaceTransformFlagBitsKHR
            /// becomes VkSurfaceTransform
            base_name: []const u8,

            /// Optional tag of the flag
            tag: ?[]const u8,
        };

        const ParamType = enum {
            in_pointer,
            out_pointer,
            bitflags,
            mut_buffer_len,
            buffer_len,
            other,
        };

        const ReturnValue = struct {
            name: []const u8,
            return_value_type: reg.TypeInfo,
            origin: enum {
                parameter,
                inner_return_value,
            },
        };

        const CommandDispatchType = enum {
            base,
            instance,
            device,
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
            try self.id_renderer.render(self.writer, id);
        }

        fn writeIdentifierWithCase(self: *Self, case: util.CaseStyle, id: []const u8) !void {
            try self.id_renderer.renderWithCase(self.writer, case, id);
        }

        fn writeIdentifierFmt(self: *Self, comptime fmt: []const u8, args: var) !void {
            try self.id_renderer.renderFmt(self.writer, fmt, args);
        }

        fn extractEnumFieldName(self: Self, enum_name: []const u8, field_name: []const u8) ![]const u8 {
            const adjusted_enum_name = util.stripAuthorTag(enum_name, self.registry.tags);

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

        fn extractBitflagName(self: Self, name: []const u8) ?BitflagName {
            const tag = util.getAuthorTag(name, self.registry.tags);
            const base_name = if (tag) |tag_name| name[0 .. name.len - tag_name.len] else name;

            if (!mem.endsWith(u8, base_name, "FlagBits")) {
                return null;
            }

            return BitflagName{
                .base_name = base_name[0 .. base_name.len - "FlagBits".len],
                .tag = tag,
            };
        }

        fn isFlags(self: Self, name: []const u8) bool {
            const tag = util.getAuthorTag(name, self.registry.tags);
            const base_name = if (tag) |tag_name| name[0 .. name.len - tag_name.len] else name;

            return mem.endsWith(u8, base_name, "Flags");
        }

        fn classifyParam(self: Self, param: reg.Command.Param) !ParamType {
            switch (param.param_type) {
                .pointer => |ptr| {
                    if (param.is_buffer_len) {
                        if (ptr.is_const or ptr.is_optional) {
                            return error.InvalidRegistry;
                        }

                        return .mut_buffer_len;
                    }

                    if (ptr.child.* == .name) {
                        const child_name = ptr.child.name;
                        if (mem.eql(u8, child_name, "void")) {
                            return .other;
                        } else if (builtin_types.get(child_name) == null and util.trimVkNamespace(child_name).ptr == child_name.ptr) {
                            return .other; // External type
                        }
                    }

                    if (ptr.size == .one and !ptr.is_optional) {
                        if (ptr.is_const) {
                            return .in_pointer;
                        } else {
                            return .out_pointer;
                        }
                    }
                },
                .name => |name| {
                    if (self.extractBitflagName(param.param_type.name) != null or self.isFlags(param.param_type.name)) {
                        return .bitflags;
                    }
                },
                else => {},
            }

            if (param.is_buffer_len) {
                return .buffer_len;
            }

            return .other;
        }

        fn classifyCommandDispatch(self: Self, name: []const u8, command: reg.Command) CommandDispatchType {
            const device_handles = std.ComptimeStringMap(void, .{
                .{"VkDevice", {}},
                .{"VkCommandBuffer", {}},
                .{"VkQueue", {}},
            });

            const override_functions = std.ComptimeStringMap(CommandDispatchType, .{
                .{"vkGetInstanceProcAddr", .base},
                .{"vkCreateInstance", .base},
                .{"vkEnumerateInstanceLayerProperties", .base},
                .{"vkEnumerateInstanceExtensionProperties", .base},
                .{"vkEnumerateInstanceVersion", .base},
                .{"vkGetDeviceProcAddr", .instance},
            });

            if (override_functions.get(name)) |dispatch_type| {
                return dispatch_type;
            }

            switch (command.params[0].param_type) {
                .name => |first_param_type_name| {
                    if (device_handles.get(first_param_type_name)) |_| {
                        return .device;
                    }
                },
                else => {},
            }

            return .instance;
        }

        fn render(self: *Self) !void {
            try self.renderCopyright();
            try self.writer.writeAll(preamble);

            for (self.registry.api_constants) |api_constant| {
                try self.renderApiConstant(api_constant);
            }

            for (self.registry.decls) |decl| {
               try self.renderDecl(decl);
            }

            try self.renderCommandPtrs();
            try self.renderExtensionInfo();
            try self.renderWrappers();
        }

        fn renderCopyright(self: *Self) !void {
            var it = mem.split(self.registry.copyright, "\n");
            while (it.next()) |line| {
                try self.writer.print("// {}\n", .{line});
            }
        }

        fn renderApiConstant(self: *Self, api_constant: reg.ApiConstant) !void {
            try self.writer.writeAll("pub const ");
            try self.renderName(api_constant.name);
            try self.writer.writeAll(" = ");

            switch (api_constant.value) {
                .expr => |expr| try self.renderApiConstantExpr(expr),
                .version => |version| {
                    try self.writer.writeAll("makeVersion(");
                    for (version) |part, i| {
                        if (i != 0) {
                            try self.writer.writeAll(", ");
                        }
                        try self.renderApiConstantExpr(part);
                    }
                    try self.writer.writeAll(")");
                },
            }

            try self.writer.writeAll(";\n");
        }

        fn renderApiConstantExpr(self: *Self, expr: []const u8) !void {
            const adjusted_expr = if (expr.len > 2 and expr[0] == '(' and expr[expr.len - 1] == ')')
                    expr[1 .. expr.len - 1]
                else
                    expr;

            var tokenizer = cparse.CTokenizer{.source = adjusted_expr};
            var peeked: ?cparse.Token = null;
            while (true) {
                const tok = peeked orelse (try tokenizer.next()) orelse break;
                peeked = null;

                switch (tok.kind) {
                    .lparen, .rparen, .tilde, .minus => {
                        try self.writer.writeAll(tok.text);
                        continue;
                    },
                    .id => {
                        try self.renderName(tok.text);
                        continue;
                    },
                    .int => {},
                    else => return error.InvalidApiConstant,
                }

                const suffix = (try tokenizer.next()) orelse {
                    try self.writer.writeAll(tok.text);
                    break;
                };

                switch (suffix.kind) {
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
                        if (f.kind != .id or !mem.eql(u8, f.text, "f")) {
                            return error.InvalidApiConstant;
                        }
                    },
                    else => {
                        try self.writer.writeAll(tok.text);
                        peeked = suffix;
                    },
                }
            }
        }

        fn renderTypeInfo(self: *Self, type_info: reg.TypeInfo) RenderTypeInfoError!void {
            switch (type_info) {
                .name => |name| try self.renderName(name),
                .command_ptr => |command_ptr| try self.renderCommandPtr(command_ptr, true),
                .pointer => |pointer| try self.renderPointer(pointer),
                .array => |array| try self.renderArray(array),
            }
        }

        fn renderName(self: *Self, name: []const u8) !void {
            if (builtin_types.get(name)) |zig_name| {
                try self.writer.writeAll(zig_name);
                return;
            } else if (self.extractBitflagName(name)) |bitflag_name| {
                try self.writeIdentifierFmt("{}Flags{}", .{
                    util.trimVkNamespace(bitflag_name.base_name),
                    @as([]const u8, if (bitflag_name.tag) |tag| tag else "")
                });
                return;
            } else if (mem.startsWith(u8, name, "vk")) {
                // Function type, always render with the exact same text for linking purposes.
                try self.writeIdentifier(name);
                return;
            } else if (mem.startsWith(u8, name, "Vk")) {
                // Type, strip namespace and write, as they are alreay in title case.
                try self.writeIdentifier(name[2..]);
                return;
            } else if (mem.startsWith(u8, name, "PFN_vk")) {
                // Function pointer type, strip off the PFN_vk part and replace it with Pfn. Note that
                // this function is only called to render the typedeffed function pointers like vkVoidFunction
                try self.writeIdentifierFmt("Pfn{}", .{name[6..]});
                return;
            } else if (mem.startsWith(u8, name, "VK_")) {
                // Constants
                try self.writeIdentifier(name[3..]);
                return;
            }

            try self.writeIdentifier(name);
        }

        fn renderCommandPtr(self: *Self, command_ptr: reg.Command, optional: bool) !void {
            if (optional) {
                try self.writer.writeByte('?');
            }
            try self.writer.writeAll("fn(");
            for (command_ptr.params) |param| {
                try self.writeIdentifierWithCase(.snake, param.name);
                try self.writer.writeAll(": ");

                blk: {
                    if (param.param_type == .name) {
                        if (self.extractBitflagName(param.param_type.name)) |bitflag_name| {
                            try self.writeIdentifierFmt("{}Flags{}", .{
                                util.trimVkNamespace(bitflag_name.base_name),
                                @as([]const u8, if (bitflag_name.tag) |tag| tag else "")
                            });
                            try self.writer.writeAll(".IntType");
                            break :blk;
                        } else if (self.isFlags(param.param_type.name)) {
                            try self.renderTypeInfo(param.param_type);
                            try self.writer.writeAll(".IntType");
                            break :blk;
                        }
                    }

                    try self.renderTypeInfo(param.param_type);
                }

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
                .many, .other_field => try self.writer.writeAll("[*]"),
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
                .alias => |alias| try self.renderName(alias),
            }
            try self.writer.writeByte(']');
            try self.renderTypeInfo(array.child.*);
        }

        fn renderDecl(self: *Self, decl: reg.Declaration) !void {
            switch (decl.decl_type) {
                .container => |container| try self.renderContainer(decl.name, container),
                .enumeration => |enumeration| try self.renderEnumeration(decl.name, enumeration),
                .bitmask => |bitmask| try self.renderBitmask(decl.name, bitmask),
                .handle => |handle| try self.renderHandle(decl.name, handle),
                .command => {},
                .alias => |alias| try self.renderAlias(decl.name, alias),
                .foreign => |foreign| try self.renderForeign(decl.name, foreign),
                .typedef => |type_info| try self.renderTypedef(decl.name, type_info),
                .opaque => try self.renderOpaque(decl.name),
            }
        }

        fn renderContainer(self: *Self, name: []const u8, container: reg.Container) !void {
            try self.writer.writeAll("pub const ");
            try self.renderName(name);
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

        fn renderEnumFieldName(self: *Self, name: []const u8, field_name: []const u8) !void {
            try self.writeIdentifierWithCase(.snake, try self.extractEnumFieldName(name, field_name));
        }

        fn renderEnumeration(self: *Self, name: []const u8, enumeration: reg.Enum) !void {
            if (enumeration.is_bitmask) {
                try self.renderBitmaskBits(name, enumeration);
                return;
            }

            try self.writer.writeAll("pub const ");
            try self.renderName(name);
            try self.writer.writeAll(" = extern enum {");

            for (enumeration.fields) |field| {
                if (field.value == .alias) {
                    continue;
                }

                try self.renderEnumFieldName(name, field.name);

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
                try self.renderEnumFieldName(name, field.name);
                try self.writer.writeAll(" = .");
                try self.renderEnumFieldName(name, field.value.alias.name);
                try self.writer.writeAll(";");
            }

            try self.writer.writeAll("};\n");
        }

        fn renderBitmaskBits(self: *Self, name: []const u8, bits: reg.Enum) !void {
            try self.writer.writeAll("pub const ");
            try self.renderName(name);
            try self.writer.writeAll(" = packed struct {");

            if (bits.fields.len == 0) {
                try self.writer.writeAll("_reserved_bits: Flags = 0,");
            } else {
                var flags_by_bitpos = [_]?[]const u8{null} ** 32;
                for (bits.fields) |field| {
                    if (field.value == .bitpos) {
                        flags_by_bitpos[field.value.bitpos] = field.name;
                    }
                }

                for (flags_by_bitpos) |opt_flag_name, bitpos| {
                    if (opt_flag_name) |flag_name| {
                        try self.renderEnumFieldName(name, flag_name);
                    } else {
                        try self.writer.print("_reserved_bit_{}", .{bitpos});
                    }

                    try self.writer.writeAll(": bool ");
                        if (bitpos == 0) { // Force alignment to integer boundaries
                            try self.writer.writeAll("align(@alignOf(Flags)) ");
                        }
                        try self.writer.writeAll("= false, ");
                }
            }
            try self.writer.writeAll("pub usingnamespace FlagsMixin(");
            try self.renderName(name);
            try self.writer.writeAll(");\n};\n");
        }

        fn renderBitmask(self: *Self, name: []const u8, bitmask: reg.Bitmask) !void {
            if (bitmask.bits_enum == null) {
                // The bits structure is generated by renderBitmaskBits, but that wont
                // output flags with no associated bits type.

                try self.writer.writeAll("pub const ");
                try self.renderName(name);
                try self.writer.writeAll(
                    \\ = packed struct {
                    \\_reserved_bits: Flags = 0,
                    \\pub usingnamespace FlagsMixin(
                );
                try self.renderName(name);
                try self.writer.writeAll(
                    \\);
                    \\};
                    \\
                );
            }
        }

        fn renderHandle(self: *Self, name: []const u8, handle: reg.Handle) !void {
            const backing_type: []const u8 = if (handle.is_dispatchable) "usize" else "u64";

            try self.writer.writeAll("pub const ");
            try self.renderName(name);
            try self.writer.print(" = extern enum({}) {{null_handle = 0, _}};\n", .{backing_type});
        }

        fn renderAlias(self: *Self, name: []const u8, alias: reg.Alias) !void {
            if (alias.target == .other_command) {
                return;
            } else if (self.extractBitflagName(name) != null) {
                // Don't make aliases of the bitflag names, as those are replaced by just the flags type
                return;
            }

            try self.writer.writeAll("pub const ");
            try self.renderName(name);
            try self.writer.writeAll(" = ");
            try self.renderName(alias.name);
            try self.writer.writeAll(";\n");
        }

        fn renderOpaque(self: *Self, name: []const u8) !void {
            try self.writer.writeAll("pub const ");
            try self.renderName(name);
            try self.writer.writeAll(" = @Type(.Opaque);\n");
        }

        fn renderForeign(self: *Self, name: []const u8, foreign: reg.Foreign) !void {
            if (mem.eql(u8, foreign.depends, "vk_platform")) {
                return; // Skip built-in types, they are handled differently
            }

            try self.writer.writeAll("pub const ");
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
            try self.writer.writeAll("pub const ");
            try self.renderName(name);
            try self.writer.writeAll(" = ");
            try self.renderTypeInfo(type_info);
            try self.writer.writeAll(";\n");
        }

        fn renderCommandPtrName(self: *Self, name: []const u8) !void {
            try self.writeIdentifierFmt("Pfn{}", .{util.trimVkNamespace(name)});
        }

        fn renderCommandPtrs(self: *Self) !void {
            for (self.registry.decls) |decl| {
                if (decl.decl_type != .command) {
                    continue;
                }

                try self.writer.writeAll("pub const ");
                try self.renderCommandPtrName(decl.name);
                try self.writer.writeAll(" = ");
                try self.renderCommandPtr(decl.decl_type.command, false);
                try self.writer.writeAll(";\n");
            }
        }

        fn renderExtensionInfo(self: *Self) !void {
            try self.writer.writeAll(
                \\pub const extension_info = struct {
                \\    const Info = struct {
                \\        name: [:0]const u8,
                \\        version: u32,
                \\    };
            );
            for (self.registry.extensions) |ext| {
                try self.writer.writeAll("pub const ");
                try self.writeIdentifierWithCase(.snake, util.trimVkNamespace(ext.name));
                try self.writer.writeAll("= Info {\n");
                try self.writer.print(".name = \"{}\", .version = {},", .{ext.name, ext.version});
                try self.writer.writeAll("};\n");
            }
            try self.writer.writeAll("};\n");
        }

        fn renderWrappers(self: *Self) !void {
            try self.renderWrappersOfDispatchType("BaseWrapper", .base);
            try self.renderWrappersOfDispatchType("InstanceWrapper", .instance);
            try self.renderWrappersOfDispatchType("DeviceWrapper", .device);
        }

        fn renderWrappersOfDispatchType(self: *Self, name: []const u8, dispatch_type: CommandDispatchType) !void {
            try self.writer.print(
                \\pub fn {}(comptime Self: type) type {{
                \\    return struct {{
                \\
                , .{name}
            );

            try self.renderWrapperLoader(dispatch_type);

            for (self.registry.decls) |decl| {
                if (decl.decl_type == .command) {
                    const command = decl.decl_type.command;
                    if (self.classifyCommandDispatch(decl.name, command) == dispatch_type) {
                        try self.renderWrapper(decl.name, decl.decl_type.command);
                    }
                }
            }

            try self.writer.writeAll("};}\n");
        }

        fn renderWrapperLoader(self: *Self, dispatch_type: CommandDispatchType) !void {
            const params = switch (dispatch_type) {
                .base => "loader: PfnGetInstanceProcAddr",
                .instance => "instance: Instance, loader: PfnGetInstanceProcAddr",
                .device => "device: Device, loader: PfnGetDeviceProcAddr",
            };

            const loader_first_param = switch (dispatch_type) {
                .base => ".null_handle, ",
                .instance => "instance, ",
                .device => "device, ",
            };

            @setEvalBranchQuota(2000);

            try self.writer.print(
                \\pub fn load({}) !Self {{
                \\    var self: Self = undefined;
                \\    inline for (std.meta.fields(Self)) |field| {{
                \\        const name = @ptrCast([*:0]const u8, field.name ++ "\x00");
                \\        const cmd_ptr = loader({}name) orelse return error.InvalidCommand;
                \\        @field(self, field.name) = @ptrCast(field.field_type, cmd_ptr);
                \\    }}
                \\    return self;
                \\}}
                \\
                , .{params, loader_first_param}
            );
        }

        fn derefName(name: []const u8) []const u8 {
            var it = util.SegmentIterator.init(name);
            return if (mem.eql(u8, it.next().?, "p"))
                    name[1..]
                else
                    name;
        }

        fn renderWrapperPrototype(self: *Self, name: []const u8, command: reg.Command, returns: []const ReturnValue) !void {
            try self.writer.writeAll("pub fn ");
            try self.writeIdentifierWithCase(.camel, util.trimVkNamespace(name));
            try self.writer.writeAll("(self: Self, ");

            for (command.params) |param| {
                switch (try self.classifyParam(param)) {
                    .in_pointer => {
                        // Remove one pointer level
                        try self.writeIdentifierWithCase(.snake, derefName(param.name));
                        try self.writer.writeAll(": ");
                        try self.renderTypeInfo(param.param_type.pointer.child.*);
                    },
                    .out_pointer => continue, // Return value
                    .bitflags, // Special stuff handled in renderWrapperCall
                    .buffer_len,
                    .mut_buffer_len,
                    .other => {
                        try self.writeIdentifierWithCase(.snake, param.name);
                        try self.writer.writeAll(": ");
                        try self.renderTypeInfo(param.param_type);
                    },
                }

                try self.writer.writeAll(", ");
            }

            try self.writer.writeAll(") ");

            if (command.return_type.* == .name and mem.eql(u8, command.return_type.name, "VkResult")) {
                try self.renderErrorSet(command.error_codes);
                try self.writer.writeByte('!');
            }

            if (returns.len == 1) {
                try self.renderTypeInfo(returns[0].return_value_type);
            } else if (returns.len > 1) {
                try self.renderReturnStructName(name);
            } else {
                try self.writer.writeAll("void");
            }
        }

        fn renderWrapperCall(self: *Self, name: []const u8, command: reg.Command, returns: []const ReturnValue) !void {
            try self.writer.writeAll("self.");
            try self.writeIdentifier(name);
            try self.writer.writeAll("(");

            for (command.params) |param| {
                switch (try self.classifyParam(param)) {
                    .in_pointer => {
                        try self.writer.writeByte('&');
                        try self.writeIdentifierWithCase(.snake, derefName(param.name));
                    },
                    .out_pointer => {
                        try self.writer.writeByte('&');
                        if (returns.len > 1) {
                            try self.writer.writeAll("return_values.");
                        }
                        try self.writeIdentifierWithCase(.snake, derefName(param.name));
                    },
                    .bitflags => {
                        try self.writeIdentifierWithCase(.snake, param.name);
                        try self.writer.writeAll(".toInt()");
                    },
                    .buffer_len, .mut_buffer_len, .other => {
                        try self.writeIdentifierWithCase(.snake, param.name);
                    },
                }

                try self.writer.writeAll(", ");
            }
            try self.writer.writeAll(")");
        }

        fn extractReturns(self: *Self, command: reg.Command) ![]const ReturnValue {
            var returns = std.ArrayList(ReturnValue).init(self.allocator);

            if (command.return_type.* == .name) {
                const return_name = command.return_type.name;
                if (!mem.eql(u8, return_name, "void") and !mem.eql(u8, return_name, "VkResult")) {
                    try returns.append(.{
                        .name = "return_value",
                        .return_value_type = command.return_type.*,
                        .origin = .inner_return_value,
                    });
                }
            }

            if (command.success_codes.len > 1) {
                if (command.return_type.* != .name or !mem.eql(u8, command.return_type.name, "VkResult")) {
                    return error.InvalidRegistry;
                }

                try returns.append(.{
                    .name = "result",
                    .return_value_type = command.return_type.*,
                    .origin = .inner_return_value,
                });
            } else if (command.success_codes.len == 1 and !mem.eql(u8, command.success_codes[0], "VK_SUCCESS")) {
                return error.InvalidRegistry;
            }

            for (command.params) |param| {
                if ((try self.classifyParam(param)) == .out_pointer) {
                    try returns.append(.{
                        .name = derefName(param.name),
                        .return_value_type = param.param_type.pointer.child.*,
                        .origin = .parameter,
                    });
                }
            }

            return returns.toOwnedSlice();
        }

        fn renderReturnStructName(self: *Self, command_name: []const u8) !void {
            try self.writeIdentifierFmt("{}Result", .{util.trimVkNamespace(command_name)});
        }

        fn renderReturnStruct(self: *Self, command_name: []const u8, returns: []const ReturnValue) !void {
            try self.writer.writeAll("pub const ");
            try self.renderReturnStructName(command_name);
            try self.writer.writeAll(" = struct {\n");
            for (returns) |ret| {
                try self.writeIdentifierWithCase(.snake, ret.name);
                try self.writer.writeAll(": ");
                try self.renderTypeInfo(ret.return_value_type);
                try self.writer.writeAll(", ");

            }
            try self.writer.writeAll("};\n");
        }

        fn renderWrapper(self: *Self, name: []const u8, command: reg.Command) !void {
            const returns_vk_result = command.return_type.* == .name
                and mem.eql(u8, command.return_type.name, "VkResult");
            const returns_void = command.return_type.* == .name
                and mem.eql(u8, command.return_type.name, "void");

            const returns = try self.extractReturns(command);

            if (returns.len > 1) {
                try self.renderReturnStruct(name, returns);
            }

            try self.renderWrapperPrototype(name, command, returns);

            if (returns.len == 1 and returns[0].origin == .inner_return_value) {
                try self.writer.writeAll("{\n\n");

                if (returns_vk_result) {
                    try self.writer.writeAll("const result = ");
                    try self.renderWrapperCall(name, command, returns);
                    try self.writer.writeAll(";\n");

                    try self.renderErrorSwitch("result", command);
                    try self.writer.writeAll("return result;\n");
                } else {
                    try self.writer.writeAll("return ");
                    try self.renderWrapperCall(name, command, returns);
                    try self.writer.writeAll(";\n");
                }

                try self.writer.writeAll("\n}\n");
                return;
            }

            try self.writer.writeAll("{\n");
            if (returns.len == 1) {
                try self.writer.writeAll("var ");
                try self.writeIdentifierWithCase(.snake, returns[0].name);
                try self.writer.writeAll(": ");
                try self.renderTypeInfo(returns[0].return_value_type);
                try self.writer.writeAll(" = undefined;\n");
            } else if (returns.len > 1) {
                try self.writer.writeAll("var return_values: ");
                try self.renderReturnStructName(name);
                try self.writer.writeAll(" = undefined;\n");
            }

            if (returns_vk_result) {
                try self.writer.writeAll("const result = ");
                try self.renderWrapperCall(name, command, returns);
                try self.writer.writeAll(";\n");

                try self.renderErrorSwitch("result", command);
                if (command.success_codes.len > 1) {
                    try self.writer.writeAll("return_values.result = result;\n");
                }
            } else {
                if (!returns_void) {
                    try self.writer.writeAll("return_values.return_value = ");
                }
                try self.renderWrapperCall(name, command, returns);
                try self.writer.writeAll(";\n");
            }

            if (returns.len == 1) {
                try self.writer.writeAll("return ");
                try self.writeIdentifierWithCase(.snake, returns[0].name);
                try self.writer.writeAll(";\n");
            } else if (returns.len > 1) {
                try self.writer.writeAll("return return_values;\n");
            }

            try self.writer.writeAll("}\n");
        }

        fn renderErrorSwitch(self: *Self, result_var: []const u8, command: reg.Command) !void {
            try self.writer.writeAll("switch (");
            try self.writeIdentifier(result_var);
            try self.writer.writeAll(") {\n");

            for (command.success_codes) |success| {
                try self.writer.writeByte('.');
                try self.renderEnumFieldName("VkResult", success);
                try self.writer.writeAll(" => {},");
            }

            for (command.error_codes) |err| {
                try self.writer.writeByte('.');
                try self.renderEnumFieldName("VkResult", err);
                try self.writer.writeAll(" => return error.");
                try self.renderResultAsErrorName(err);
                try self.writer.writeAll(", ");
            }

            try self.writer.writeAll("else => return error.Unknown,}\n");
        }

        fn renderErrorSet(self: *Self, errors: []const []const u8) !void {
            try self.writer.writeAll("error{");
            for (errors) |name| {
                try self.renderResultAsErrorName(name);
                try self.writer.writeAll(", ");
            }
            try self.writer.writeAll("Unknown, }");
        }

        fn renderResultAsErrorName(self: *Self, name: []const u8) !void {
            const error_prefix = "VK_ERROR_";
            if (mem.startsWith(u8, name, error_prefix)) {
                try self.writeIdentifierWithCase(.title, name[error_prefix.len ..]);
            } else {
                // Apparently some commands (VkAcquireProfilingLockInfoKHR) return
                // success codes as error...
                try self.writeIdentifierWithCase(.title, util.trimVkNamespace(name));
            }
        }
    };
}

pub fn render(writer: var, allocator: *Allocator, registry: *const reg.Registry) !void {
    var renderer = Renderer(@TypeOf(writer)).init(writer, allocator, registry);
    defer renderer.deinit();
    try renderer.render();
}
