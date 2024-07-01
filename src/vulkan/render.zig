const std = @import("std");
const reg = @import("registry.zig");
const id_render = @import("../id_render.zig");
const cparse = @import("c_parse.zig");
const mem = std.mem;
const Allocator = mem.Allocator;
const CaseStyle = id_render.CaseStyle;
const IdRenderer = id_render.IdRenderer;

const preamble =
    \\// This file is generated from the Khronos Vulkan XML API registry by vulkan-zig.
    \\
    \\const std = @import("std");
    \\const builtin = @import("builtin");
    \\const root = @import("root");
    \\const vk = @This();
    \\const Allocator = std.mem.Allocator;
    \\
    \\pub const vulkan_call_conv: std.builtin.CallingConvention = if (builtin.os.tag == .windows and builtin.cpu.arch == .x86)
    \\        .Stdcall
    \\    else if (builtin.abi == .android and (builtin.cpu.arch.isARM() or builtin.cpu.arch.isThumb()) and std.Target.arm.featureSetHas(builtin.cpu.features, .has_v7) and builtin.cpu.arch.ptrBitWidth() == 32)
    \\        // On Android 32-bit ARM targets, Vulkan functions use the "hardfloat"
    \\        // calling convention, i.e. float parameters are passed in registers. This
    \\        // is true even if the rest of the application passes floats on the stack,
    \\        // as it does by default when compiling for the armeabi-v7a NDK ABI.
    \\        .AAPCSVFP
    \\    else
    \\        .C;
    \\pub fn FlagsMixin(comptime FlagsType: type) type {
    \\    return struct {
    \\        pub const IntType = @typeInfo(FlagsType).Struct.backing_integer.?;
    \\        pub fn toInt(self: FlagsType) IntType {
    \\            return @bitCast(self);
    \\        }
    \\        pub fn fromInt(flags: IntType) FlagsType {
    \\            return @bitCast(flags);
    \\        }
    \\        pub fn merge(lhs: FlagsType, rhs: FlagsType) FlagsType {
    \\            return fromInt(toInt(lhs) | toInt(rhs));
    \\        }
    \\        pub fn intersect(lhs: FlagsType, rhs: FlagsType) FlagsType {
    \\            return fromInt(toInt(lhs) & toInt(rhs));
    \\        }
    \\        pub fn complement(self: FlagsType) FlagsType {
    \\            return fromInt(~toInt(self));
    \\        }
    \\        pub fn subtract(lhs: FlagsType, rhs: FlagsType) FlagsType {
    \\            return fromInt(toInt(lhs) & toInt(rhs.complement()));
    \\        }
    \\        pub fn contains(lhs: FlagsType, rhs: FlagsType) bool {
    \\            return toInt(intersect(lhs, rhs)) == toInt(rhs);
    \\        }
    \\        pub usingnamespace FlagFormatMixin(FlagsType);
    \\    };
    \\}
    \\fn FlagFormatMixin(comptime FlagsType: type) type {
    \\    return struct {
    \\        pub fn format(
    \\            self: FlagsType,
    \\            comptime _: []const u8,
    \\            _: std.fmt.FormatOptions,
    \\            writer: anytype,
    \\        ) !void {
    \\            try writer.writeAll(@typeName(FlagsType) ++ "{");
    \\            var first = true;
    \\            @setEvalBranchQuota(10_000);
    \\            inline for (comptime std.meta.fieldNames(FlagsType)) |name| {
    \\                if (name[0] == '_') continue;
    \\                if (@field(self, name)) {
    \\                    if (first) {
    \\                        try writer.writeAll(" ." ++ name);
    \\                        first = false;
    \\                    } else {
    \\                        try writer.writeAll(", ." ++ name);
    \\                    }
    \\                }
    \\            }
    \\            if (!first) try writer.writeAll(" ");
    \\            try writer.writeAll("}");
    \\        }
    \\    };
    \\}
    \\pub fn makeApiVersion(variant: u3, major: u7, minor: u10, patch: u12) u32 {
    \\    return (@as(u32, variant) << 29) | (@as(u32, major) << 22) | (@as(u32, minor) << 12) | patch;
    \\}
    \\pub fn apiVersionVariant(version: u32) u3 {
    \\    return @truncate(version >> 29);
    \\}
    \\pub fn apiVersionMajor(version: u32) u7 {
    \\    return @truncate(version >> 22);
    \\}
    \\pub fn apiVersionMinor(version: u32) u10 {
    \\    return @truncate(version >> 12);
    \\}
    \\pub fn apiVersionPatch(version: u32) u12 {
    \\    return @truncate(version);
    \\}
    \\pub const ApiInfo = struct {
    \\    name: [:0]const u8 = "custom",
    \\    version: u32 = makeApiVersion(0, 0, 0, 0),
    \\    base_commands: BaseCommandFlags = .{},
    \\    instance_commands: InstanceCommandFlags = .{},
    \\    device_commands: DeviceCommandFlags = .{},
    \\};
;

const builtin_types = std.StaticStringMap([]const u8).initComptime(.{
    .{ "void", @typeName(void) },
    .{ "char", @typeName(u8) },
    .{ "float", @typeName(f32) },
    .{ "double", @typeName(f64) },
    .{ "uint8_t", @typeName(u8) },
    .{ "uint16_t", @typeName(u16) },
    .{ "uint32_t", @typeName(u32) },
    .{ "uint64_t", @typeName(u64) },
    .{ "int8_t", @typeName(i8) },
    .{ "int16_t", @typeName(i16) },
    .{ "int32_t", @typeName(i32) },
    .{ "int64_t", @typeName(i64) },
    .{ "size_t", @typeName(usize) },
    .{ "int", @typeName(c_int) },
});

const foreign_types = std.StaticStringMap([]const u8).initComptime(.{
    .{ "Display", "opaque {}" },
    .{ "VisualID", @typeName(c_uint) },
    .{ "Window", @typeName(c_ulong) },
    .{ "RROutput", @typeName(c_ulong) },
    .{ "wl_display", "opaque {}" },
    .{ "wl_surface", "opaque {}" },
    .{ "HINSTANCE", "std.os.windows.HINSTANCE" },
    .{ "HWND", "std.os.windows.HWND" },
    .{ "HMONITOR", "*opaque {}" },
    .{ "HANDLE", "std.os.windows.HANDLE" },
    .{ "SECURITY_ATTRIBUTES", "std.os.windows.SECURITY_ATTRIBUTES" },
    .{ "DWORD", "std.os.windows.DWORD" },
    .{ "LPCWSTR", "std.os.windows.LPCWSTR" },
    .{ "xcb_connection_t", "opaque {}" },
    .{ "xcb_visualid_t", @typeName(u32) },
    .{ "xcb_window_t", @typeName(u32) },
    .{ "zx_handle_t", @typeName(u32) },
    .{ "_screen_context", "opaque {}" },
    .{ "_screen_window", "opaque {}" },
    .{ "IDirectFB", "opaque {}" },
    .{ "IDirectFBSurface", "opaque {}" },
});

const CommandDispatchType = enum {
    base,
    instance,
    device,

    fn name(self: CommandDispatchType) []const u8 {
        return switch (self) {
            .base => "Base",
            .instance => "Instance",
            .device => "Device",
        };
    }

    fn nameLower(self: CommandDispatchType) []const u8 {
        return switch (self) {
            .base => "base",
            .instance => "instance",
            .device => "device",
        };
    }
};

const dispatchable_handles = std.StaticStringMap(CommandDispatchType).initComptime(.{
    .{ "VkDevice", .device },
    .{ "VkCommandBuffer", .device },
    .{ "VkQueue", .device },
    .{ "VkInstance", .instance },
});

const additional_namespaces = std.StaticStringMap([]const u8).initComptime(.{
    // vkCmdBegin...
    .{ "VkCommandBuffer", "Cmd" },
    // vkQueueSubmit...
    .{ "VkQueue", "Queue" },
});

const dispatch_override_functions = std.StaticStringMap(CommandDispatchType).initComptime(.{
    // See https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#initialization-functionpointers
    .{ "vkGetInstanceProcAddr", .base },
    .{ "vkGetDeviceProcAddr", .instance },

    .{ "vkEnumerateInstanceVersion", .base },
    .{ "vkEnumerateInstanceExtensionProperties", .base },
    .{ "vkEnumerateInstanceLayerProperties", .base },
    .{ "vkCreateInstance", .base },
});

// Functions that return an array of objects via a count and data pointer.
const enumerate_functions = std.StaticStringMap(void).initComptime(.{
    .{"vkEnumeratePhysicalDevices"},
    .{"vkEnumeratePhysicalDeviceGroups"},
    .{"vkGetPhysicalDeviceQueueFamilyProperties"},
    .{"vkGetPhysicalDeviceQueueFamilyProperties2"},
    .{"vkEnumerateInstanceLayerProperties"},
    .{"vkEnumerateInstanceExtensionProperties"},
    .{"vkEnumerateDeviceLayerProperties"},
    .{"vkEnumerateDeviceExtensionProperties"},
    .{"vkGetImageSparseMemoryRequirements"},
    .{"vkGetImageSparseMemoryRequirements2"},
    .{"vkGetDeviceImageSparseMemoryRequirements"},
    .{"vkGetPhysicalDeviceSparseImageFormatProperties"},
    .{"vkGetPhysicalDeviceSparseImageFormatProperties2"},
    .{"vkGetPhysicalDeviceToolProperties"},
    .{"vkGetPipelineCacheData"},

    .{"vkGetPhysicalDeviceSurfaceFormatsKHR"},
    .{"vkGetPhysicalDeviceSurfaceFormats2KHR"},
    .{"vkGetPhysicalDeviceSurfacePresentModesKHR"},

    .{"vkGetSwapchainImagesKHR"},
    .{"vkGetPhysicalDevicePresentRectanglesKHR"},

    .{"vkGetPhysicalDeviceCalibrateableTimeDomainsKHR"},
});

// Given one of the above commands, returns the type of the array elements
// (and performs some basic verification that the command has the expected signature).
fn getEnumerateFunctionDataType(command: reg.Command) !reg.TypeInfo {
    if (command.params.len < 2) {
        return error.InvalidRegistry;
    }
    const count_param = command.params[command.params.len - 2];
    if (!count_param.is_buffer_len) {
        return error.InvalidRegistry;
    }
    const data_param = command.params[command.params.len - 1];
    return switch (data_param.param_type) {
        .pointer => |pointer| pointer.child.*,
        else => error.InvalidRegistry,
    };
}

fn eqlIgnoreCase(lhs: []const u8, rhs: []const u8) bool {
    if (lhs.len != rhs.len) {
        return false;
    }

    for (lhs, rhs) |l, r| {
        if (std.ascii.toLower(l) != std.ascii.toLower(r)) {
            return false;
        }
    }

    return true;
}

pub fn trimVkNamespace(id: []const u8) []const u8 {
    const prefixes = [_][]const u8{ "VK_", "vk", "Vk", "PFN_vk" };
    for (prefixes) |prefix| {
        if (mem.startsWith(u8, id, prefix)) {
            return id[prefix.len..];
        }
    }

    return id;
}

fn Renderer(comptime WriterType: type) type {
    return struct {
        const Self = @This();
        const WriteError = WriterType.Error;
        const RenderTypeInfoError = WriteError || std.fmt.ParseIntError || error{ OutOfMemory, InvalidRegistry };

        const BitflagName = struct {
            /// Name without FlagBits, so VkSurfaceTransformFlagBitsKHR
            /// becomes VkSurfaceTransform
            base_name: []const u8,

            /// Optional flag bits revision, used in places like VkAccessFlagBits2KHR
            revision: ?[]const u8,

            /// Optional tag of the flag
            tag: ?[]const u8,
        };

        const ParamType = enum {
            in_pointer,
            out_pointer,
            in_out_pointer,
            bitflags,
            mut_buffer_len,
            buffer_len,
            dispatch_handle,
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

        writer: WriterType,
        allocator: Allocator,
        registry: *const reg.Registry,
        id_renderer: *IdRenderer,
        declarations_by_name: std.StringHashMap(*const reg.DeclarationType),
        structure_types: std.StringHashMap(void),

        fn init(writer: WriterType, allocator: Allocator, registry: *const reg.Registry, id_renderer: *IdRenderer) !Self {
            var declarations_by_name = std.StringHashMap(*const reg.DeclarationType).init(allocator);
            errdefer declarations_by_name.deinit();

            for (registry.decls) |*decl| {
                const result = try declarations_by_name.getOrPut(decl.name);
                if (result.found_existing) {
                    std.log.err("duplicate registry entry '{s}'", .{decl.name});
                    return error.InvalidRegistry;
                }

                result.value_ptr.* = &decl.decl_type;
            }

            const vk_structure_type_decl = declarations_by_name.get("VkStructureType") orelse return error.InvalidRegistry;
            const vk_structure_type = switch (vk_structure_type_decl.*) {
                .enumeration => |e| e,
                else => return error.InvalidRegistry,
            };
            var structure_types = std.StringHashMap(void).init(allocator);
            errdefer structure_types.deinit();

            for (vk_structure_type.fields) |field| {
                try structure_types.put(field.name, {});
            }

            return Self{
                .writer = writer,
                .allocator = allocator,
                .registry = registry,
                .id_renderer = id_renderer,
                .declarations_by_name = declarations_by_name,
                .structure_types = structure_types,
            };
        }

        fn deinit(self: *Self) void {
            self.declarations_by_name.deinit();
        }

        fn writeIdentifier(self: Self, id: []const u8) !void {
            try id_render.writeIdentifier(self.writer, id);
        }

        fn writeIdentifierWithCase(self: *Self, case: CaseStyle, id: []const u8) !void {
            try self.id_renderer.renderWithCase(self.writer, case, id);
        }

        fn writeIdentifierFmt(self: *Self, comptime fmt: []const u8, args: anytype) !void {
            try self.id_renderer.renderFmt(self.writer, fmt, args);
        }

        fn extractEnumFieldName(self: Self, enum_name: []const u8, field_name: []const u8) ![]const u8 {
            const adjusted_enum_name = self.id_renderer.stripAuthorTag(enum_name);

            var enum_it = id_render.SegmentIterator.init(adjusted_enum_name);
            var field_it = id_render.SegmentIterator.init(field_name);

            while (true) {
                const rest = field_it.rest();
                const field_segment = field_it.next() orelse return error.InvalidRegistry;
                const enum_segment = enum_it.next() orelse return rest;

                if (!eqlIgnoreCase(enum_segment, field_segment)) {
                    return rest;
                }
            }
        }

        fn extractBitflagFieldName(bitflag_name: BitflagName, field_name: []const u8) ![]const u8 {
            var flag_it = id_render.SegmentIterator.init(bitflag_name.base_name);
            var field_it = id_render.SegmentIterator.init(field_name);

            while (true) {
                const rest = field_it.rest();
                const field_segment = field_it.next() orelse return error.InvalidRegistry;
                const flag_segment = flag_it.next() orelse {
                    if (bitflag_name.revision) |revision| {
                        if (mem.eql(u8, revision, field_segment))
                            return field_it.rest();
                    }

                    return rest;
                };

                if (!eqlIgnoreCase(flag_segment, field_segment)) {
                    return rest;
                }
            }
        }

        fn extractBitflagName(self: Self, name: []const u8) !?BitflagName {
            const tag = self.id_renderer.getAuthorTag(name);
            const tagless_name = if (tag) |tag_name| name[0 .. name.len - tag_name.len] else name;

            // Strip out the "version" number of a bitflag, like VkAccessFlagBits2KHR.
            const base_name = std.mem.trimRight(u8, tagless_name, "0123456789");

            const maybe_flag_bits_index = mem.lastIndexOf(u8, base_name, "FlagBits");
            if (maybe_flag_bits_index == null) {
                return null;
            } else if (maybe_flag_bits_index != base_name.len - "FlagBits".len) {
                // It is unlikely that a type that is not a flag bit would contain FlagBits,
                // and more likely that we have missed something if FlagBits isn't the last
                // part of base_name
                return error.InvalidRegistry;
            }

            return BitflagName{
                .base_name = base_name[0 .. base_name.len - "FlagBits".len],
                .revision = if (base_name.len != tagless_name.len) tagless_name[base_name.len..] else null,
                .tag = tag,
            };
        }

        fn isFlags(self: Self, name: []const u8) bool {
            const tag = self.id_renderer.getAuthorTag(name);
            const tagless_name = if (tag) |tag_name| name[0 .. name.len - tag_name.len] else name;
            const base_name = std.mem.trimRight(u8, tagless_name, "0123456789");
            return mem.endsWith(u8, base_name, "Flags");
        }

        fn resolveDeclaration(self: Self, name: []const u8) ?reg.DeclarationType {
            const decl = self.declarations_by_name.get(name) orelse return null;
            return self.resolveAlias(decl.*) catch return null;
        }

        fn resolveAlias(self: Self, start_decl: reg.DeclarationType) !reg.DeclarationType {
            var decl = start_decl;
            while (true) {
                const name = switch (decl) {
                    .alias => |alias| alias.name,
                    else => return decl,
                };

                const decl_ptr = self.declarations_by_name.get(name) orelse return error.InvalidRegistry;
                decl = decl_ptr.*;
            }
        }

        fn isInOutPointer(self: Self, ptr: reg.Pointer) !bool {
            if (ptr.child.* != .name) {
                return false;
            }

            const decl = self.resolveDeclaration(ptr.child.name) orelse return error.InvalidRegistry;
            if (decl != .container) {
                return false;
            }

            const container = decl.container;
            if (container.is_union) {
                return false;
            }

            for (container.fields) |field| {
                if (mem.eql(u8, field.name, "pNext")) {
                    return true;
                }
            }

            return false;
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
                        } else if (builtin_types.get(child_name) == null and trimVkNamespace(child_name).ptr == child_name.ptr) {
                            return .other; // External type
                        }
                    }

                    if (ptr.size == .one and !ptr.is_optional) {
                        // Sometimes, a mutable pointer to a struct is taken, even though
                        // Vulkan expects this struct to be initialized. This is particularly the case
                        // for getting structs which include pNext chains.
                        if (ptr.is_const) {
                            return .in_pointer;
                        } else if (try self.isInOutPointer(ptr)) {
                            return .in_out_pointer;
                        } else {
                            return .out_pointer;
                        }
                    }
                },
                .name => |name| {
                    if (dispatchable_handles.get(name) != null) {
                        return .dispatch_handle;
                    }

                    if ((try self.extractBitflagName(name)) != null or self.isFlags(name)) {
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

        fn classifyCommandDispatch(name: []const u8, command: reg.Command) CommandDispatchType {
            if (dispatch_override_functions.get(name)) |dispatch_type| {
                return dispatch_type;
            }

            switch (command.params[0].param_type) {
                .name => |first_param_type_name| {
                    if (dispatchable_handles.get(first_param_type_name)) |dispatch_type| {
                        return dispatch_type;
                    }
                },
                else => {},
            }

            return .instance;
        }

        fn render(self: *Self) !void {
            try self.writer.writeAll(preamble);

            for (self.registry.api_constants) |api_constant| {
                try self.renderApiConstant(api_constant);
            }

            for (self.registry.decls) |decl| {
                try self.renderDecl(decl);
            }

            try self.renderCommandPtrs();
            try self.renderFeatureInfo();
            try self.renderExtensionInfo();
            try self.renderWrappers();
            try self.renderProxies();
        }

        fn renderApiConstant(self: *Self, api_constant: reg.ApiConstant) !void {
            try self.writer.writeAll("pub const ");
            try self.renderName(api_constant.name);
            try self.writer.writeAll(" = ");

            switch (api_constant.value) {
                .expr => |expr| try self.renderApiConstantExpr(expr),
                .version => |version| {
                    try self.writer.writeAll("makeApiVersion(");
                    for (version, 0..) |part, i| {
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

            var tokenizer = cparse.CTokenizer{ .source = adjusted_expr };
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
                            try self.writer.print("@as(u64, {s})", .{tok.text});
                        } else if (mem.eql(u8, suffix.text, "U")) {
                            try self.writer.print("@as(u32, {s})", .{tok.text});
                        } else {
                            return error.InvalidApiConstant;
                        }
                    },
                    .dot => {
                        const decimal = (try tokenizer.next()) orelse return error.InvalidConstantExpr;
                        try self.writer.print("@as(f32, {s}.{s})", .{ tok.text, decimal.text });

                        const f = (try tokenizer.next()) orelse return error.InvalidConstantExpr;
                        if (f.kind != .id or f.text.len != 1 or (f.text[0] != 'f' and f.text[0] != 'F')) {
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
            } else if (try self.extractBitflagName(name)) |bitflag_name| {
                try self.writeIdentifierFmt("{s}Flags{s}{s}", .{
                    trimVkNamespace(bitflag_name.base_name),
                    @as([]const u8, if (bitflag_name.revision) |revision| revision else ""),
                    @as([]const u8, if (bitflag_name.tag) |tag| tag else ""),
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
                try self.writeIdentifierFmt("Pfn{s}", .{name[6..]});
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
            try self.writer.writeAll("*const fn(");
            for (command_ptr.params) |param| {
                try self.writeIdentifierWithCase(.snake, param.name);
                try self.writer.writeAll(": ");

                blk: {
                    if (param.param_type == .name) {
                        if (try self.extractBitflagName(param.param_type.name)) |bitflag_name| {
                            try self.writeIdentifierFmt("{s}Flags{s}{s}", .{
                                trimVkNamespace(bitflag_name.base_name),
                                @as([]const u8, if (bitflag_name.revision) |revision| revision else ""),
                                @as([]const u8, if (bitflag_name.tag) |tag| tag else ""),
                            });
                            break :blk;
                        } else if (self.isFlags(param.param_type.name)) {
                            try self.renderTypeInfo(param.param_type);
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
                try self.writer.writeAll("anyopaque");
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
                .external => try self.renderExternal(decl.name),
            }
        }

        fn renderSpecialContainer(self: *Self, name: []const u8) !bool {
            const maybe_author = self.id_renderer.getAuthorTag(name);
            const basename = self.id_renderer.stripAuthorTag(name);
            if (std.mem.eql(u8, basename, "VkAccelerationStructureInstance")) {
                try self.writer.print(
                    \\extern struct {{
                    \\    transform: TransformMatrix{s},
                    \\    instance_custom_index_and_mask: packed struct(u32) {{
                    \\        instance_custom_index: u24,
                    \\        mask: u8,
                    \\    }},
                    \\    instance_shader_binding_table_record_offset_and_flags: packed struct(u32) {{
                    \\        instance_shader_binding_table_record_offset: u24,
                    \\        flags: u8, // GeometryInstanceFlagsKHR
                    \\    }},
                    \\    acceleration_structure_reference: u64,
                    \\}};
                    \\
                ,
                    .{maybe_author orelse ""},
                );
                return true;
            } else if (std.mem.eql(u8, basename, "VkAccelerationStructureSRTMotionInstance")) {
                try self.writer.print(
                    \\extern struct {{
                    \\    transform_t0: SRTData{0s},
                    \\    transform_t1: SRTData{0s},
                    \\    instance_custom_index_and_mask: packed struct(u32) {{
                    \\        instance_custom_index: u24,
                    \\        mask: u8,
                    \\    }},
                    \\    instance_shader_binding_table_record_offset_and_flags: packed struct(u32) {{
                    \\        instance_shader_binding_table_record_offset: u24,
                    \\        flags: u8, // GeometryInstanceFlagsKHR
                    \\    }},
                    \\    acceleration_structure_reference: u64,
                    \\}};
                    \\
                ,
                    .{maybe_author orelse ""},
                );
                return true;
            } else if (std.mem.eql(u8, basename, "VkAccelerationStructureMatrixMotionInstance")) {
                try self.writer.print(
                    \\extern struct {{
                    \\    transform_t0: TransformMatrix{0s},
                    \\    transform_t1: TransformMatrix{0s},
                    \\    instance_custom_index_and_mask: packed struct(u32) {{
                    \\        instance_custom_index: u24,
                    \\        mask: u8,
                    \\    }},
                    \\    instance_shader_binding_table_record_offset_and_flags: packed struct(u32) {{
                    \\        instance_shader_binding_table_record_offset: u24,
                    \\        flags: u8, // GeometryInstanceFlagsKHR
                    \\    }},
                    \\    acceleration_structure_reference: u64,
                    \\}};
                    \\
                ,
                    .{maybe_author orelse ""},
                );
                return true;
            }

            return false;
        }

        fn renderContainer(self: *Self, name: []const u8, container: reg.Container) !void {
            try self.writer.writeAll("pub const ");
            try self.renderName(name);
            try self.writer.writeAll(" = ");

            if (try self.renderSpecialContainer(name)) {
                return;
            }

            for (container.fields) |field| {
                if (field.bits != null) {
                    return error.UnhandledBitfieldStruct;
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
                    if (!container.is_union) {
                        try self.renderContainerDefaultField(name, container, field);
                    }
                    try self.writer.writeAll(", ");
                }
            }

            try self.writer.writeAll("};\n");
        }

        fn renderContainerDefaultField(self: *Self, name: []const u8, container: reg.Container, field: reg.Container.Field) !void {
            if (mem.eql(u8, field.name, "sType")) {
                if (container.stype == null) {
                    return;
                }

                const stype = container.stype.?;
                if (!mem.startsWith(u8, stype, "VK_STRUCTURE_TYPE_")) {
                    return error.InvalidRegistry;
                }

                // Some structures dont have a VK_STRUCTURE_TYPE for some reason apparently...
                // See https://github.com/KhronosGroup/Vulkan-Docs/issues/1225
                _ = self.structure_types.get(stype) orelse return;

                try self.writer.writeAll(" = .");
                try self.writeIdentifierWithCase(.snake, stype["VK_STRUCTURE_TYPE_".len..]);
            } else if (field.field_type == .name and mem.eql(u8, "VkBool32", field.field_type.name) and isFeatureStruct(name, container.extends)) {
                try self.writer.writeAll(" = FALSE");
            } else if (field.is_optional) {
                if (field.field_type == .name) {
                    const field_type_name = field.field_type.name;
                    if (self.resolveDeclaration(field_type_name)) |decl_type| {
                        if (decl_type == .handle) {
                            try self.writer.writeAll(" = .null_handle");
                        } else if (decl_type == .bitmask) {
                            try self.writer.writeAll(" = .{}");
                        } else if (decl_type == .typedef and decl_type.typedef == .command_ptr) {
                            try self.writer.writeAll(" = null");
                        } else if ((decl_type == .typedef and builtin_types.has(decl_type.typedef.name)) or
                            (decl_type == .foreign and builtin_types.has(field_type_name)))
                        {
                            try self.writer.writeAll(" = 0");
                        }
                    }
                } else if (field.field_type == .pointer) {
                    try self.writer.writeAll(" = null");
                }
            } else if (field.field_type == .pointer and field.field_type.pointer.is_optional) {
                // pointer nullability could be here or above
                try self.writer.writeAll(" = null");
            }
        }

        fn isFeatureStruct(name: []const u8, maybe_extends: ?[]const []const u8) bool {
            if (std.mem.eql(u8, name, "VkPhysicalDeviceFeatures")) return true;
            if (maybe_extends) |extends| {
                return for (extends) |extend| {
                    if (mem.eql(u8, extend, "VkDeviceCreateInfo")) break true;
                } else false;
            }
            return false;
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
            try self.writer.writeAll(" = enum(i32) {");

            for (enumeration.fields) |field| {
                if (field.value == .alias)
                    continue;

                try self.renderEnumFieldName(name, field.name);
                switch (field.value) {
                    .int => |int| try self.writer.print(" = {}, ", .{int}),
                    .bitpos => |pos| try self.writer.print(" = 1 << {}, ", .{pos}),
                    .bit_vector => |bv| try self.writer.print("= 0x{X}, ", .{bv}),
                    .alias => unreachable,
                }
            }

            try self.writer.writeAll("_,");

            for (enumeration.fields) |field| {
                if (field.value != .alias or field.value.alias.is_compat_alias)
                    continue;

                try self.writer.writeAll("pub const ");
                try self.renderEnumFieldName(name, field.name);
                try self.writer.writeAll(" = ");
                try self.renderName(name);
                try self.writer.writeByte('.');
                try self.renderEnumFieldName(name, field.value.alias.name);
                try self.writer.writeAll(";\n");
            }

            try self.writer.writeAll("};\n");
        }

        fn bitmaskFlagsType(bitwidth: u8) ![]const u8 {
            return switch (bitwidth) {
                32 => "Flags",
                64 => "Flags64",
                else => return error.InvalidRegistry,
            };
        }

        fn renderBitmaskBits(self: *Self, name: []const u8, bits: reg.Enum) !void {
            try self.writer.writeAll("pub const ");
            try self.renderName(name);
            const flags_type = try bitmaskFlagsType(bits.bitwidth);
            try self.writer.print(" = packed struct({s}) {{", .{flags_type});

            const bitflag_name = (try self.extractBitflagName(name)) orelse return error.InvalidRegistry;

            if (bits.fields.len == 0) {
                try self.writer.print("_reserved_bits: {s} = 0,", .{flags_type});
            } else {
                var flags_by_bitpos = [_]?[]const u8{null} ** 64;
                for (bits.fields) |field| {
                    if (field.value == .bitpos) {
                        flags_by_bitpos[field.value.bitpos] = field.name;
                    }
                }

                for (flags_by_bitpos[0..bits.bitwidth], 0..) |maybe_flag_name, bitpos| {
                    if (maybe_flag_name) |flag_name| {
                        const field_name = try extractBitflagFieldName(bitflag_name, flag_name);
                        try self.writeIdentifierWithCase(.snake, field_name);
                    } else {
                        try self.writer.print("_reserved_bit_{}", .{bitpos});
                    }

                    try self.writer.writeAll(": bool = false,");
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

                const flags_type = try bitmaskFlagsType(bitmask.bitwidth);

                try self.writer.writeAll("pub const ");
                try self.renderName(name);
                try self.writer.print(
                    \\ = packed struct {{
                    \\_reserved_bits: {s} = 0,
                    \\pub usingnamespace FlagsMixin(
                , .{flags_type});
                try self.renderName(name);
                try self.writer.writeAll(");\n};\n");
            }
        }

        fn renderHandle(self: *Self, name: []const u8, handle: reg.Handle) !void {
            const backing_type: []const u8 = if (handle.is_dispatchable) "usize" else "u64";

            try self.writer.writeAll("pub const ");
            try self.renderName(name);
            try self.writer.print(" = enum({s}) {{null_handle = 0, _}};\n", .{backing_type});
        }

        fn renderAlias(self: *Self, name: []const u8, alias: reg.Alias) !void {
            if (alias.target == .other_command) {
                return;
            } else if ((try self.extractBitflagName(name)) != null) {
                // Don't make aliases of the bitflag names, as those are replaced by just the flags type
                return;
            }

            try self.writer.writeAll("pub const ");
            try self.renderName(name);
            try self.writer.writeAll(" = ");
            try self.renderName(alias.name);
            try self.writer.writeAll(";\n");
        }

        fn renderExternal(self: *Self, name: []const u8) !void {
            try self.writer.writeAll("pub const ");
            try self.renderName(name);
            try self.writer.writeAll(" = opaque {};\n");
        }

        fn renderForeign(self: *Self, name: []const u8, foreign: reg.Foreign) !void {
            if (mem.eql(u8, foreign.depends, "vk_platform")) {
                return; // Skip built-in types, they are handled differently
            }

            try self.writer.writeAll("pub const ");
            try self.writeIdentifier(name);
            try self.writer.print(" = if (@hasDecl(root, \"{s}\")) root.", .{name});
            try self.writeIdentifier(name);
            try self.writer.writeAll(" else ");

            if (foreign_types.get(name)) |default| {
                try self.writer.writeAll(default);
                try self.writer.writeAll(";\n");
            } else {
                try self.writer.print("opaque {{}};\n", .{});
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
            try self.writeIdentifierFmt("Pfn{s}", .{trimVkNamespace(name)});
        }

        fn renderCommandPtrs(self: *Self) !void {
            for (self.registry.decls) |decl| {
                switch (decl.decl_type) {
                    .command => {
                        try self.writer.writeAll("pub const ");
                        try self.renderCommandPtrName(decl.name);
                        try self.writer.writeAll(" = ");
                        try self.renderCommandPtr(decl.decl_type.command, false);
                        try self.writer.writeAll(";\n");
                    },
                    .alias => |alias| if (alias.target == .other_command) {
                        try self.writer.writeAll("pub const ");
                        try self.renderCommandPtrName(decl.name);
                        try self.writer.writeAll(" = ");
                        try self.renderCommandPtrName(alias.name);
                        try self.writer.writeAll(";\n");
                    },
                    else => {},
                }
            }
        }

        fn renderFeatureInfo(self: *Self) !void {
            try self.writer.writeAll(
                \\pub const features = struct {
                \\
            );
            // The commands in a feature level are not pre-sorted based on if they are instance or device functions.
            var base_commands = std.BufSet.init(self.allocator);
            defer base_commands.deinit();
            var instance_commands = std.BufSet.init(self.allocator);
            defer instance_commands.deinit();
            var device_commands = std.BufSet.init(self.allocator);
            defer device_commands.deinit();
            for (self.registry.features) |feature| {
                try self.writer.writeAll("pub const ");
                try self.writeIdentifierWithCase(.snake, trimVkNamespace(feature.name));
                try self.writer.writeAll("= ApiInfo {\n");
                try self.writer.print(".name = \"{s}\", .version = makeApiVersion(0, {}, {}, 0),", .{
                    trimVkNamespace(feature.name),
                    feature.level.major,
                    feature.level.minor,
                });
                // collect feature information
                for (feature.requires) |require| {
                    for (require.commands) |command_name| {
                        const decl = self.resolveDeclaration(command_name) orelse continue;
                        // If the target type does not exist, it was likely an empty enum -
                        // assume spec is correct and that this was not a function alias.
                        const decl_type = self.resolveAlias(decl) catch continue;
                        const command = switch (decl_type) {
                            .command => |cmd| cmd,
                            else => continue,
                        };
                        const class = classifyCommandDispatch(command_name, command);
                        switch (class) {
                            .base => {
                                try base_commands.insert(command_name);
                            },
                            .instance => {
                                try instance_commands.insert(command_name);
                            },
                            .device => {
                                try device_commands.insert(command_name);
                            },
                        }
                    }
                }
                // and write them out
                // clear command lists for next iteration
                try self.writer.writeAll(".base_commands = ");
                try self.renderCommandFlags(&base_commands);
                base_commands.hash_map.clearRetainingCapacity();

                try self.writer.writeAll(".instance_commands = ");
                try self.renderCommandFlags(&instance_commands);
                instance_commands.hash_map.clearRetainingCapacity();

                try self.writer.writeAll(".device_commands = ");
                try self.renderCommandFlags(&device_commands);
                device_commands.hash_map.clearRetainingCapacity();

                try self.writer.writeAll("};\n");
            }

            try self.writer.writeAll("};\n");
        }

        fn renderExtensionInfo(self: *Self) !void {
            try self.writer.writeAll(
                \\pub const extensions = struct {
                \\
            );
            // The commands in an extension are not pre-sorted based on if they are instance or device functions.
            var base_commands = std.BufSet.init(self.allocator);
            defer base_commands.deinit();
            var instance_commands = std.BufSet.init(self.allocator);
            defer instance_commands.deinit();
            var device_commands = std.BufSet.init(self.allocator);
            defer device_commands.deinit();
            for (self.registry.extensions) |ext| {
                try self.writer.writeAll("pub const ");
                try self.writeIdentifierWithCase(.snake, trimVkNamespace(ext.name));
                try self.writer.writeAll("= ApiInfo {\n");
                try self.writer.print(".name = \"{s}\", .version = {},", .{ ext.name, ext.version });
                // collect extension functions
                for (ext.requires) |require| {
                    for (require.commands) |command_name| {
                        const decl = self.resolveDeclaration(command_name) orelse continue;
                        // If the target type does not exist, it was likely an empty enum -
                        // assume spec is correct and that this was not a function alias.
                        const decl_type = self.resolveAlias(decl) catch continue;
                        const command = switch (decl_type) {
                            .command => |cmd| cmd,
                            else => continue,
                        };
                        const class = classifyCommandDispatch(command_name, command);
                        switch (class) {
                            .base => {
                                try base_commands.insert(command_name);
                            },
                            .instance => {
                                try instance_commands.insert(command_name);
                            },
                            .device => {
                                try device_commands.insert(command_name);
                            },
                        }
                    }
                }
                // and write them out
                try self.writer.writeAll(".base_commands = ");
                try self.renderCommandFlags(&base_commands);
                base_commands.hash_map.clearRetainingCapacity();

                try self.writer.writeAll(".instance_commands = ");
                try self.renderCommandFlags(&instance_commands);
                instance_commands.hash_map.clearRetainingCapacity();

                try self.writer.writeAll(".device_commands = ");
                try self.renderCommandFlags(&device_commands);
                device_commands.hash_map.clearRetainingCapacity();

                try self.writer.writeAll("};\n");
            }
            try self.writer.writeAll("};\n");
        }

        fn renderCommandFlags(self: *Self, commands: *const std.BufSet) !void {
            try self.writer.writeAll(".{\n");
            var iterator = commands.iterator();
            while (iterator.next()) |command_name| {
                try self.writer.writeAll(".");
                try self.writeIdentifierWithCase(.camel, trimVkNamespace(command_name.*));
                try self.writer.writeAll(" = true, \n");
            }
            try self.writer.writeAll("},\n");
        }

        fn renderWrappers(self: *Self) !void {
            try self.writer.writeAll(
                \\pub fn CommandFlagsMixin(comptime CommandFlags: type) type {
                \\    return struct {
                \\        pub fn merge(lhs: CommandFlags, rhs: CommandFlags) CommandFlags {
                \\            var result: CommandFlags = .{};
                \\            @setEvalBranchQuota(10_000);
                \\            inline for (@typeInfo(CommandFlags).Struct.fields) |field| {
                \\                @field(result, field.name) = @field(lhs, field.name) or @field(rhs, field.name);
                \\            }
                \\            return result;
                \\        }
                \\        pub fn intersect(lhs: CommandFlags, rhs: CommandFlags) CommandFlags {
                \\            var result: CommandFlags = .{};
                \\            @setEvalBranchQuota(10_000);
                \\            inline for (@typeInfo(CommandFlags).Struct.fields) |field| {
                \\                @field(result, field.name) = @field(lhs, field.name) and @field(rhs, field.name);
                \\            }
                \\            return result;
                \\        }
                \\        pub fn complement(self: CommandFlags) CommandFlags {
                \\            var result: CommandFlags = .{};
                \\            @setEvalBranchQuota(10_000);
                \\            inline for (@typeInfo(CommandFlags).Struct.fields) |field| {
                \\                @field(result, field.name) = !@field(self, field.name);
                \\            }
                \\            return result;
                \\        }
                \\        pub fn subtract(lhs: CommandFlags, rhs: CommandFlags) CommandFlags {
                \\            var result: CommandFlags = .{};
                \\            @setEvalBranchQuota(10_000);
                \\            inline for (@typeInfo(CommandFlags).Struct.fields) |field| {
                \\                @field(result, field.name) = @field(lhs, field.name) and !@field(rhs, field.name);
                \\            }
                \\            return result;
                \\        }
                \\        pub fn contains(lhs: CommandFlags, rhs: CommandFlags) bool {
                \\            @setEvalBranchQuota(10_000);
                \\            inline for (@typeInfo(CommandFlags).Struct.fields) |field| {
                \\                if (!@field(lhs, field.name) and @field(rhs, field.name)) {
                \\                    return false;
                \\                }
                \\            }
                \\            return true;
                \\        }
                \\        pub usingnamespace FlagFormatMixin(CommandFlags);
                \\    };
                \\}
                \\
            );
            try self.renderWrappersOfDispatchType(.base);
            try self.renderWrappersOfDispatchType(.instance);
            try self.renderWrappersOfDispatchType(.device);
        }

        fn renderWrappersOfDispatchType(self: *Self, dispatch_type: CommandDispatchType) !void {
            const name = dispatch_type.name();
            const name_lower = dispatch_type.nameLower();

            try self.writer.print(
                \\pub const {0s}CommandFlags = packed struct {{
                \\
            , .{name});
            for (self.registry.decls) |decl| {
                // If the target type does not exist, it was likely an empty enum -
                // assume spec is correct and that this was not a function alias.
                const decl_type = self.resolveAlias(decl.decl_type) catch continue;
                const command = switch (decl_type) {
                    .command => |cmd| cmd,
                    else => continue,
                };

                if (classifyCommandDispatch(decl.name, command) == dispatch_type) {
                    try self.writer.writeAll("    ");
                    try self.writeIdentifierWithCase(.camel, trimVkNamespace(decl.name));
                    try self.writer.writeAll(": bool = false,\n");
                }
            }

            try self.writer.print(
                \\pub fn CmdType(comptime tag: std.meta.FieldEnum({0s}CommandFlags)) type {{
                \\    return switch (tag) {{
                \\
            , .{name});
            for (self.registry.decls) |decl| {
                // If the target type does not exist, it was likely an empty enum -
                // assume spec is correct and that this was not a function alias.
                const decl_type = self.resolveAlias(decl.decl_type) catch continue;
                const command = switch (decl_type) {
                    .command => |cmd| cmd,
                    else => continue,
                };

                if (classifyCommandDispatch(decl.name, command) == dispatch_type) {
                    try self.writer.writeByte('.');
                    try self.writeIdentifierWithCase(.camel, trimVkNamespace(decl.name));
                    try self.writer.writeAll(" => ");
                    try self.renderCommandPtrName(decl.name);
                    try self.writer.writeAll(",\n");
                }
            }
            try self.writer.writeAll("    };\n}");

            try self.writer.print(
                \\pub fn cmdName(tag: std.meta.FieldEnum({0s}CommandFlags)) [:0]const u8 {{
                \\    return switch(tag) {{
                \\
            , .{name});
            for (self.registry.decls) |decl| {
                // If the target type does not exist, it was likely an empty enum -
                // assume spec is correct and that this was not a function alias.
                const decl_type = self.resolveAlias(decl.decl_type) catch continue;
                const command = switch (decl_type) {
                    .command => |cmd| cmd,
                    else => continue,
                };

                if (classifyCommandDispatch(decl.name, command) == dispatch_type) {
                    try self.writer.writeByte('.');
                    try self.writeIdentifierWithCase(.camel, trimVkNamespace(decl.name));
                    try self.writer.print(
                        \\ => "{s}",
                        \\
                    , .{decl.name});
                }
            }
            try self.writer.writeAll("    };\n}");

            try self.writer.print(
                \\    pub usingnamespace CommandFlagsMixin({s}CommandFlags);
                \\}};
                \\
            , .{name});

            try self.writer.print(
                \\pub fn {0s}Wrapper(comptime apis: []const ApiInfo) type {{
                \\    return struct {{
                \\        dispatch: Dispatch,
                \\
                \\        const Self = @This();
                \\        pub const commands = blk: {{
                \\            var cmds: {0s}CommandFlags = .{{}};
                \\            for (apis) |api| {{
                \\                cmds = cmds.merge(api.{1s}_commands);
                \\            }}
                \\            break :blk cmds;
                \\        }};
                \\        pub const Dispatch = blk: {{
                \\            @setEvalBranchQuota(10_000);
                \\            const Type = std.builtin.Type;
                \\            const fields_len = fields_len: {{
                \\                var fields_len: u32 = 0;
                \\                for (@typeInfo({0s}CommandFlags).Struct.fields) |field| {{
                \\                    fields_len += @intCast(@intFromBool(@field(commands, field.name)));
                \\                }}
                \\                break :fields_len fields_len;
                \\            }};
                \\            var fields: [fields_len]Type.StructField = undefined;
                \\            var i: usize = 0;
                \\            for (@typeInfo({0s}CommandFlags).Struct.fields) |field| {{
                \\                if (@field(commands, field.name)) {{
                \\                    const field_tag = std.enums.nameCast(std.meta.FieldEnum({0s}CommandFlags), field.name);
                \\                    const PfnType = {0s}CommandFlags.CmdType(field_tag);
                \\                    fields[i] = .{{
                \\                        .name = {0s}CommandFlags.cmdName(field_tag),
                \\                        .type = PfnType,
                \\                        .default_value = null,
                \\                        .is_comptime = false,
                \\                        .alignment = @alignOf(PfnType),
                \\                    }};
                \\                    i += 1;
                \\                }}
                \\            }}
                \\            break :blk @Type(.{{
                \\                .Struct = .{{
                \\                    .layout = .auto,
                \\                    .fields = &fields,
                \\                    .decls = &[_]std.builtin.Type.Declaration{{}},
                \\                    .is_tuple = false,
                \\                }},
                \\            }});
                \\        }};
                \\
            , .{ name, name_lower });

            try self.renderWrapperLoader(dispatch_type);

            for (self.registry.decls) |decl| {
                // If the target type does not exist, it was likely an empty enum -
                // assume spec is correct and that this was not a function alias.
                const decl_type = self.resolveAlias(decl.decl_type) catch continue;
                const command = switch (decl_type) {
                    .command => |cmd| cmd,
                    else => continue,
                };

                if (classifyCommandDispatch(decl.name, command) != dispatch_type) {
                    continue;
                }
                // Note: If this decl is an alias, generate a full wrapper instead of simply an
                // alias like `const old = new;`. This ensures that Vulkan bindings generated
                // for newer versions of vulkan can still invoke extension behavior on older
                // implementations.
                try self.renderWrapper(decl.name, command);
                if (enumerate_functions.has(decl.name)) {
                    try self.renderWrapperAlloc(decl.name, command);
                }
            }

            try self.writer.writeAll("};}\n");
        }

        fn renderWrapperLoader(self: *Self, dispatch_type: CommandDispatchType) !void {
            const params = switch (dispatch_type) {
                .base => "loader: anytype",
                .instance => "instance: Instance, loader: anytype",
                .device => "device: Device, loader: anytype",
            };

            const loader_first_arg = switch (dispatch_type) {
                .base => "Instance.null_handle",
                .instance => "instance",
                .device => "device",
            };

            @setEvalBranchQuota(2000);

            try self.writer.print(
                \\pub fn load({[params]s}) error{{CommandLoadFailure}}!Self {{
                \\    var self: Self = undefined;
                \\    inline for (std.meta.fields(Dispatch)) |field| {{
                \\        const name: [*:0]const u8 = @ptrCast(field.name ++ "\x00");
                \\        if (loader({[first_arg]s}, name)) |cmd_ptr| {{
                \\             @field(self.dispatch, field.name) = @ptrCast(cmd_ptr);
                \\        }} else {{
                \\             std.log.err("Command loading failed for \"{{s}}\".", .{{field.name}});
                \\             return error.CommandLoadFailure;
                \\        }}
                \\    }}
                \\    return self;
                \\}}
                \\pub fn loadNoFail({[params]s}) Self {{
                \\    var self: Self = undefined;
                \\    inline for (std.meta.fields(Dispatch)) |field| {{
                \\        const name: [*:0]const u8 = @ptrCast(field.name ++ "\x00");
                \\        const cmd_ptr = loader({[first_arg]s}, name) orelse undefined;
                \\        @field(self.dispatch, field.name) = @ptrCast(cmd_ptr);
                \\    }}
                \\    return self;
                \\}}
            , .{ .params = params, .first_arg = loader_first_arg });
        }

        fn renderProxies(self: *Self) !void {
            try self.renderProxy(.instance, "VkInstance", true);
            try self.renderProxy(.device, "VkDevice", true);
            try self.renderProxy(.device, "VkCommandBuffer", false);
            try self.renderProxy(.device, "VkQueue", false);
        }

        fn renderProxy(
            self: *Self,
            dispatch_type: CommandDispatchType,
            dispatch_handle: []const u8,
            also_add_other_commands: bool,
        ) !void {
            const loader_name = dispatch_type.name();

            try self.writer.print(
                \\pub fn {0s}Proxy(comptime apis: []const ApiInfo) type {{
                \\    return struct {{
                \\        const Self = @This();
                \\        pub const Wrapper = {1s}Wrapper(apis);
                \\
                \\        handle: {0s},
                \\        wrapper: *const Wrapper,
                \\
                \\        pub fn init(handle: {0s}, wrapper: *const Wrapper) Self {{
                \\            return .{{
                \\                .handle = handle,
                \\                .wrapper = wrapper,
                \\            }};
                \\        }}
            , .{ trimVkNamespace(dispatch_handle), loader_name });

            for (self.registry.decls) |decl| {
                const decl_type = self.resolveAlias(decl.decl_type) catch continue;
                const command = switch (decl_type) {
                    .command => |cmd| cmd,
                    else => continue,
                };

                if (classifyCommandDispatch(decl.name, command) != dispatch_type) {
                    continue;
                }

                switch (command.params[0].param_type) {
                    .name => |name| {
                        const skip = blk: {
                            if (mem.eql(u8, name, dispatch_handle)) {
                                break :blk false;
                            }

                            break :blk !also_add_other_commands;
                        };

                        if (skip) continue;
                    },
                    else => continue, // Not a dispatchable handle
                }

                try self.renderProxyCommand(decl.name, command, dispatch_handle);
                if (enumerate_functions.has(decl.name)) {
                    try self.renderProxyCommandAlloc(decl.name, command, dispatch_handle);
                }
            }

            try self.writer.writeAll(
                \\    };
                \\}
            );
        }

        fn renderProxyCommand(self: *Self, name: []const u8, command: reg.Command, dispatch_handle: []const u8) !void {
            const returns_vk_result = command.return_type.* == .name and mem.eql(u8, command.return_type.name, "VkResult");
            const returns = try self.extractReturns(command);

            if (returns_vk_result) {
                try self.writer.writeAll("pub const ");
                try self.renderErrorSetName(name);
                try self.writer.writeAll(" = Wrapper.");
                try self.renderErrorSetName(name);
                try self.writer.writeAll(";\n");
            }

            if (returns.len > 1) {
                try self.writer.writeAll("pub const ");
                try self.renderReturnStructName(name);
                try self.writer.writeAll(" = Wrapper.");
                try self.renderReturnStructName(name);
                try self.writer.writeAll(";\n");
            }

            try self.renderWrapperPrototype(name, command, returns, dispatch_handle, .proxy);

            try self.writer.writeAll(
                \\{
                \\return self.wrapper.
            );
            try self.writeIdentifierWithCase(.camel, trimVkNamespace(name));
            try self.writer.writeByte('(');

            for (command.params) |param| {
                switch (try self.classifyParam(param)) {
                    .out_pointer => continue,
                    .dispatch_handle => {
                        if (mem.eql(u8, param.param_type.name, dispatch_handle)) {
                            try self.writer.writeAll("self.handle");
                        } else {
                            try self.writeIdentifierWithCase(.snake, param.name);
                        }
                    },
                    else => {
                        try self.writeIdentifierWithCase(.snake, param.name);
                    },
                }
                try self.writer.writeAll(", ");
            }

            try self.writer.writeAll(
                \\);
                \\}
                \\
            );
        }

        // vkFooKHR => vkFooAllocKHR
        fn makeAllocWrapperName(self: *Self, wrapped_name: []const u8) ![]const u8 {
            const tag = self.id_renderer.getAuthorTag(wrapped_name) orelse "";
            const base_len = wrapped_name.len - tag.len;
            return std.mem.concat(self.allocator, u8, &.{ wrapped_name[0..base_len], "Alloc", tag });
        }

        fn renderProxyCommandAlloc(self: *Self, wrapped_name: []const u8, command: reg.Command, dispatch_handle: []const u8) !void {
            const returns_vk_result = command.return_type.* == .name and mem.eql(u8, command.return_type.name, "VkResult");

            const name = try self.makeAllocWrapperName(wrapped_name);
            defer self.allocator.free(name);

            if (command.params.len < 2) {
                return error.InvalidRegistry;
            }
            const params = command.params[0 .. command.params.len - 2];
            const data_type = try getEnumerateFunctionDataType(command);

            try self.writer.writeAll("pub const ");
            try self.renderErrorSetName(name);
            try self.writer.writeAll(" = Wrapper.");
            try self.renderErrorSetName(name);
            try self.writer.writeAll(";\n");

            try self.renderAllocWrapperPrototype(name, params, returns_vk_result, data_type, dispatch_handle, .proxy);
            try self.writer.writeAll(
                \\{
                \\return self.wrapper.
            );
            try self.writeIdentifierWithCase(.camel, trimVkNamespace(name));
            try self.writer.writeByte('(');

            for (params) |param| {
                switch (try self.classifyParam(param)) {
                    .out_pointer => return error.InvalidRegistry,
                    .dispatch_handle => {
                        if (mem.eql(u8, param.param_type.name, dispatch_handle)) {
                            try self.writer.writeAll("self.handle");
                        } else {
                            try self.writeIdentifierWithCase(.snake, param.name);
                        }
                    },
                    else => {
                        try self.writeIdentifierWithCase(.snake, param.name);
                    },
                }
                try self.writer.writeAll(", ");
            }

            try self.writer.writeAll(
                \\allocator,);
                \\}
                \\
            );
        }

        fn derefName(name: []const u8) []const u8 {
            var it = id_render.SegmentIterator.init(name);
            return if (mem.eql(u8, it.next().?, "p"))
                name[1..]
            else
                name;
        }

        const WrapperKind = enum {
            wrapper,
            proxy,
        };

        fn renderWrapperName(
            self: *Self,
            name: []const u8,
            dispatch_handle: []const u8,
            kind: WrapperKind,
        ) !void {
            const trimmed_name = switch (kind) {
                .wrapper => trimVkNamespace(name),
                .proxy => blk: {
                    // Strip additional namespaces: queue for VkQueue and cmd for VkCommandBuffer
                    const no_vk = trimVkNamespace(name);
                    const additional_namespace = additional_namespaces.get(dispatch_handle) orelse break :blk no_vk;
                    if (std.mem.startsWith(u8, no_vk, additional_namespace)) {
                        break :blk no_vk[additional_namespace.len..];
                    }

                    break :blk no_vk;
                },
            };
            try self.writeIdentifierWithCase(.camel, trimmed_name);
        }

        fn renderWrapperParam(self: *Self, param: reg.Command.Param) !void {
            try self.writeIdentifierWithCase(.snake, param.name);
            try self.writer.writeAll(": ");
            try self.renderTypeInfo(param.param_type);
            try self.writer.writeAll(", ");
        }

        fn renderWrapperPrototype(
            self: *Self,
            name: []const u8,
            command: reg.Command,
            returns: []const ReturnValue,
            dispatch_handle: []const u8,
            kind: WrapperKind,
        ) !void {
            try self.writer.writeAll("pub fn ");
            try self.renderWrapperName(name, dispatch_handle, kind);
            try self.writer.writeAll("(self: Self, ");

            for (command.params) |param| {
                const class = try self.classifyParam(param);
                // Skip the dispatch type for proxying wrappers
                if (kind == .proxy and class == .dispatch_handle and mem.eql(u8, param.param_type.name, dispatch_handle)) {
                    continue;
                }

                // This parameter is returned instead.
                if (class == .out_pointer) {
                    continue;
                }

                try self.renderWrapperParam(param);
            }

            try self.writer.writeAll(") ");

            const returns_vk_result = command.return_type.* == .name and mem.eql(u8, command.return_type.name, "VkResult");
            if (returns_vk_result) {
                try self.renderErrorSetName(name);
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

        fn renderWrapperCall(
            self: *Self,
            name: []const u8,
            command: reg.Command,
            returns: []const ReturnValue,
            return_var_name: ?[]const u8,
        ) !void {
            try self.writer.writeAll("self.dispatch.");
            try self.writeIdentifier(name);
            try self.writer.writeAll("(");

            for (command.params) |param| {
                switch (try self.classifyParam(param)) {
                    .out_pointer => {
                        try self.writer.writeByte('&');
                        try self.writeIdentifierWithCase(.snake, return_var_name.?);
                        if (returns.len > 1) {
                            try self.writer.writeByte('.');
                            try self.writeIdentifierWithCase(.snake, derefName(param.name));
                        }
                    },
                    else => {
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

            return try returns.toOwnedSlice();
        }

        fn renderReturnStructName(self: *Self, command_name: []const u8) !void {
            try self.writeIdentifierFmt("{s}Result", .{trimVkNamespace(command_name)});
        }

        fn renderErrorSetName(self: *Self, name: []const u8) !void {
            try self.writeIdentifierWithCase(.title, trimVkNamespace(name));
            try self.writer.writeAll("Error");
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
            const returns_vk_result = command.return_type.* == .name and mem.eql(u8, command.return_type.name, "VkResult");
            const returns_void = command.return_type.* == .name and mem.eql(u8, command.return_type.name, "void");

            const returns = try self.extractReturns(command);

            if (returns.len > 1) {
                try self.renderReturnStruct(name, returns);
            }

            if (returns_vk_result) {
                try self.writer.writeAll("pub const ");
                try self.renderErrorSetName(name);
                try self.writer.writeAll(" = ");
                try self.renderErrorSet(command.error_codes);
                try self.writer.writeAll(";\n");
            }

            try self.renderWrapperPrototype(name, command, returns, "", .wrapper);

            if (returns.len == 1 and returns[0].origin == .inner_return_value) {
                try self.writer.writeAll("{\n\n");

                if (returns_vk_result) {
                    try self.writer.writeAll("const result = ");
                    try self.renderWrapperCall(name, command, returns, null);
                    try self.writer.writeAll(";\n");

                    try self.renderErrorSwitch("result", command);
                    try self.writer.writeAll("return result;\n");
                } else {
                    try self.writer.writeAll("return ");
                    try self.renderWrapperCall(name, command, returns, null);
                    try self.writer.writeAll(";\n");
                }

                try self.writer.writeAll("\n}\n");
                return;
            }

            const return_var_name = if (returns.len == 1)
                try std.fmt.allocPrint(self.allocator, "out_{s}", .{returns[0].name})
            else
                "return_values";

            try self.writer.writeAll("{\n");
            if (returns.len == 1) {
                try self.writer.writeAll("var ");
                try self.writeIdentifierWithCase(.snake, return_var_name);
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
                try self.renderWrapperCall(name, command, returns, return_var_name);
                try self.writer.writeAll(";\n");

                try self.renderErrorSwitch("result", command);
                if (command.success_codes.len > 1) {
                    try self.writer.writeAll("return_values.result = result;\n");
                }
            } else {
                if (!returns_void) {
                    try self.writer.writeAll("return_values.return_value = ");
                }
                try self.renderWrapperCall(name, command, returns, return_var_name);
                try self.writer.writeAll(";\n");
            }

            if (returns.len >= 1) {
                try self.writer.writeAll("return ");
                try self.writeIdentifierWithCase(.snake, return_var_name);
                try self.writer.writeAll(";\n");
            }

            try self.writer.writeAll("}\n");
        }

        fn renderAllocWrapperPrototype(
            self: *Self,
            name: []const u8,
            params: []const reg.Command.Param,
            returns_vk_result: bool,
            data_type: reg.TypeInfo,
            dispatch_handle: []const u8,
            kind: WrapperKind,
        ) !void {
            try self.writer.writeAll("pub fn ");
            try self.renderWrapperName(name, "", .wrapper);
            try self.writer.writeAll("(self: Self, ");
            for (params) |param| {
                const class = try self.classifyParam(param);
                // Skip the dispatch type for proxying wrappers
                if (kind == .proxy and class == .dispatch_handle and mem.eql(u8, param.param_type.name, dispatch_handle)) {
                    continue;
                }
                try self.renderWrapperParam(param);
            }
            try self.writer.writeAll("allocator: Allocator,) ");

            if (returns_vk_result) {
                try self.renderErrorSetName(name);
            } else {
                try self.writer.writeAll("Allocator.Error");
            }

            try self.writer.writeAll("![]");
            try self.renderTypeInfo(data_type);
        }

        fn renderWrapperAlloc(self: *Self, wrapped_name: []const u8, command: reg.Command) !void {
            const returns_vk_result = command.return_type.* == .name and mem.eql(u8, command.return_type.name, "VkResult");

            const name = try self.makeAllocWrapperName(wrapped_name);
            defer self.allocator.free(name);

            if (command.params.len < 2) {
                return error.InvalidRegistry;
            }
            const params = command.params[0 .. command.params.len - 2];
            const data_type = try getEnumerateFunctionDataType(command);

            if (returns_vk_result) {
                try self.writer.writeAll("pub const ");
                try self.renderErrorSetName(name);
                try self.writer.writeAll(" =\n    ");
                try self.renderErrorSetName(wrapped_name);
                try self.writer.writeAll(" || Allocator.Error;\n");
            }

            try self.renderAllocWrapperPrototype(name, params, returns_vk_result, data_type, "", .wrapper);
            try self.writer.writeAll(
                \\{
                \\    var count: u32 = undefined;
            );

            if (returns_vk_result) {
                try self.writer.writeAll("var data: []");
                try self.renderTypeInfo(data_type);
                try self.writer.writeAll(
                    \\ = &.{};
                    \\errdefer allocator.free(data);
                    \\var result = Result.incomplete;
                    \\while (result == .incomplete) {
                    \\    _ = try
                );
            }

            try self.writer.writeAll(" self.");
            try self.renderWrapperName(wrapped_name, "", .wrapper);
            try self.writer.writeAll("(\n");
            for (params) |param| {
                try self.writeIdentifierWithCase(.snake, param.name);
                try self.writer.writeAll(", ");
            }
            try self.writer.writeAll("&count, null);\n");

            if (returns_vk_result) {
                try self.writer.writeAll(
                    \\data = try allocator.realloc(data, count);
                    \\result = try 
                );
            } else {
                try self.writer.writeAll("const data = try allocator.alloc(");
                try self.renderTypeInfo(data_type);
                try self.writer.writeAll(
                    \\, count);
                    \\errdefer allocator.free(data);
                );
            }

            try self.writer.writeAll(" self.");
            try self.renderWrapperName(wrapped_name, "", .wrapper);
            try self.writer.writeAll("(\n");
            for (params) |param| {
                try self.writeIdentifierWithCase(.snake, param.name);
                try self.writer.writeAll(", ");
            }
            try self.writer.writeAll("&count, data.ptr);\n");

            if (returns_vk_result) {
                try self.writer.writeAll("}\n");
            }

            try self.writer.writeAll(
                \\    return if (count == data.len) data else allocator.realloc(data, count);
                \\}
            );
        }

        fn renderErrorSwitch(self: *Self, result_var: []const u8, command: reg.Command) !void {
            try self.writer.writeAll("switch (");
            try self.writeIdentifier(result_var);
            try self.writer.writeAll(") {\n");

            for (command.success_codes) |success| {
                try self.writer.writeAll("Result.");
                try self.renderEnumFieldName("VkResult", success);
                try self.writer.writeAll(" => {},");
            }

            for (command.error_codes) |err| {
                try self.writer.writeAll("Result.");
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
                if (std.mem.eql(u8, name, "VK_ERROR_UNKNOWN")) {
                    continue;
                }
                try self.renderResultAsErrorName(name);
                try self.writer.writeAll(", ");
            }
            try self.writer.writeAll("Unknown, }");
        }

        fn renderResultAsErrorName(self: *Self, name: []const u8) !void {
            const error_prefix = "VK_ERROR_";
            if (mem.startsWith(u8, name, error_prefix)) {
                try self.writeIdentifierWithCase(.title, name[error_prefix.len..]);
            } else {
                // Apparently some commands (VkAcquireProfilingLockInfoKHR) return
                // success codes as error...
                try self.writeIdentifierWithCase(.title, trimVkNamespace(name));
            }
        }
    };
}

pub fn render(writer: anytype, allocator: Allocator, registry: *const reg.Registry, id_renderer: *IdRenderer) !void {
    var renderer = try Renderer(@TypeOf(writer)).init(writer, allocator, registry, id_renderer);
    defer renderer.deinit();
    try renderer.render();
}
