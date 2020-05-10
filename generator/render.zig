const std = @import("std");
const ast = std.zig.ast;
const mem = std.mem;
const Allocator = mem.Allocator;
const reg = @import("registry.zig");
const Registry = reg.Registry;

const base_indent = " " ** 4;

const ForeignType = struct {
    name: []const u8,
    expr: []const u8
};

const foreign_types = [_]ForeignType{
    .{.name = "Display", .expr = "Type(.Opaque)"},
    .{.name = "VisualID", .expr = @typeName(c_uint)},
    .{.name = "Window", .expr = @typeName(c_ulong)},
    .{.name = "RROutput", .expr = @typeName(c_ulong)},
    .{.name = "wl_display", .expr = "@Type(.Opaque)"},
    .{.name = "wl_surface", .expr = "@Type(.Opaque)"},
    .{.name = "HINSTANCE", .expr = "std.os.HINSTANCE"},
    .{.name = "HWND", .expr = "*@OpaqueType()"},
    .{.name = "HMONITOR", .expr = "*OpaqueType()"},
    .{.name = "HANDLE", .expr = "std.os.HANDLE"},
    .{.name = "SECURITY_ATTRIBUTES", .expr = "std.os.SECURITY_ATTRIBUTES"},
    .{.name = "DWORD", .expr = "std.os.DWORD"},
    .{.name = "LPCWSTR", .expr = "std.os.LPCWSTR"},
    .{.name = "xcb_connection_t", .expr = "@OpaqueType()"},
    .{.name = "xcb_visualid_t", .expr = @typeName(u32)},
    .{.name = "xcb_window_t", .expr = @typeName(u32)},
    .{.name = "zx_handle_t", .expr = @typeName(u32)},
    .{.name = "GgpStreamDescriptor", .expr = @typeName(u32)}, // TODO: Remove GGP-related code
    .{.name = "GgpFrameToken", .expr = @typeName(u32)},
    .{.name = "ANativeWindow", .expr = "@Type(.Opaque)"},
    .{.name = "AHardwareBuffer", .expr = "@Type(.Opaque)"},
    .{.name = "CAMetalLayer", .expr = "@Type(.Opaque)"},
};

const foreign_types_namespace = "foreign";

const BuiltinType = struct {
    c_name: []const u8,
    zig_name: []const u8
};

const builtin_types = [_]BuiltinType{
    .{.c_name = "char", .zig_name = @typeName(u8)},
    .{.c_name = "float", .zig_name = @typeName(f32)},
    .{.c_name = "double", .zig_name = @typeName(f64)},
    .{.c_name = "uint8_t", .zig_name = @typeName(u8)},
    .{.c_name = "uint16_t", .zig_name = @typeName(u16)},
    .{.c_name = "uint32_t", .zig_name = @typeName(u32)},
    .{.c_name = "uint64_t", .zig_name = @typeName(u64)},
    .{.c_name = "int32_t", .zig_name = @typeName(i32)},
    .{.c_name = "int64_t", .zig_name = @typeName(i64)},
    .{.c_name = "size_t", .zig_name = @typeName(usize)},
    .{.c_name = "int", .zig_name = @typeName(c_int)},
};

pub fn render(out: var, registry: *Registry) !void {
    try out.writeAll("const std = @import(\"std\");\n\n");
    try renderApiConstants(out, registry);
    try renderForeignTypes(out);
    try out.writeAll("\n");
    try renderDeclarations(out, registry);
    try renderTest(out);
}

fn trimNamespace(name: []const u8) []const u8 {
    const prefixes = [_][]const u8{"VK_", "vk", "Vk", "PFN_vk"};
    for (prefixes) |prefix| {
        if (mem.startsWith(u8, name, prefix)) {
            return name[prefix.len..];
        }
    }

    unreachable;
}

fn getAuthorTag(registry: *Registry, name: []const u8) ?[]const u8 {
    var it = registry.tags.iterator(0);
    while (it.next()) |tag| {
        if (mem.endsWith(u8, name, tag.name)) {
            return tag;
        }
    }

    return null;
}

fn trimTag(registry: *Registry, name: []const u8) []const u8 {
    const tag = getAuthorTag(name) orelse return name;
    return mem.trimRight(u8, name[0 .. name.len - tag.name.len], "_");
}

// Lifted from src-self-hosted/translate_c.zig
fn isValidZigIdentifier(name: []const u8) bool {
    for (name) |c, i| {
        switch (c) {
            '_', 'a'...'z', 'A'...'Z' => {},
            '0' ... '9' => if (i == 0) return false,
            else => return false
        }
    }

    return true;
}

// Lifted from src-self-hosted/translate_c.zig
fn isZigReservedIdentifier(name: []const u8) bool {
    if (name.len > 1 and (name[0] == 'u' or name[0] == 'i')) {
        for (name[1..]) |c| {
            switch (c) {
                '0'...'9' => {},
                else => return false,
            }
        }
        return true;
    }

    // void is invalid in c so it doesn't need to be checked.
    const reserved_names = [_][]const u8 {
        "comptime_float", "comptime_int", "bool", "isize",
        "usize", "f16", "f32", "f64", "f128", "c_longdouble",
        "noreturn", "type", "anyerror", "c_short", "c_ushort",
        "c_int", "c_uint", "c_long", "c_ulong", "c_longlong", "c_ulonglong"
    };

    for (reserved_names) |reserved| {
        if (mem.eql(u8, reserved, name)) {
            return true;
        }
    }

    return false;
}

fn writeIdentifier(out: var, name: []const u8) !void {
    if (!isValidZigIdentifier(name) or isZigReservedIdentifier(name) or std.zig.Token.getKeyword(name) != null) {
        try out.print("@\"{}\"", .{name});
    } else {
        try out.writeAll(name);
    }
}

fn writeConstAssignmemt(out: var, name: []const u8) !void {
    try out.writeAll("pub const ");
    try writeIdentifier(out, name);
    try out.writeAll(" = ");
}

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

fn renderTypeInfo(out: var, registry: *Registry, type_info: reg.TypeInfo) !void {
    if (type_info.array_size) |array_size| {
        try out.print("[{}]", .{trimNamespace(array_size)});
    }

    for (type_info.pointers) |ptr| {
        // Apparently Vulkan optional-ness information is not correct, so every pointer
        // is considered optional
        switch (ptr.size) {
            .One => try out.writeAll("?*"),
            .Many => try out.writeAll("?[*]"),
            .ZeroTerminated => try out.writeAll("?[*:0]")
        }

        if (ptr.is_const) {
            try out.writeAll("const ");
        }
    }

    // If the type is foreign, add the appropriate namespace specifier
    for (foreign_types) |fty| {
        if (mem.eql(u8, type_info.name, fty.name)) {
            try out.print(foreign_types_namespace ++ ".{}", .{type_info.name});
            return;
        }
    }

    // Some types can be mapped directly to a built-in type
    for (builtin_types) |bty| {
        if (mem.eql(u8, type_info.name, bty.c_name)) {
            try out.writeAll(bty.zig_name);
            return;
        }
    }

    // If the type is a `void*`, it needs to be translated to `*c_void`.
    // If its not a pointer, its a return type, so `void` should be emitted.
    if (mem.eql(u8, type_info.name, "void")) {
        if (type_info.pointers.len > 0) {
            try out.writeAll("c_void");
        } else {
            try out.writeAll("void");
        }

        return;
    }

    // Make sure the type is defined by Vulkan otherwise
    if (registry.findDefinitionByName(type_info.name) == null) {
        return error.InvalidType;
    }

    try writeIdentifier(out, trimNamespace(type_info.name));
}

fn renderApiConstantExpr(out: var, constant_expr: []const u8) !void {
    // omit enclosing parenthesis
    const expr = if (constant_expr[0] == '(' and constant_expr[constant_expr.len - 1] == ')')
        constant_expr[1 .. constant_expr.len - 1]
    else
        constant_expr;

    var i: usize = 0;
    while (i < expr.len) {
        switch (expr[i]) {
            '(', ')', '~' => try out.writeByte(expr[i]),
            '-' => try out.writeAll(" - "),
            'a'...'z', 'A'...'Z', '_' => {
                var j = i;
                while (j < expr.len) : (j += 1) {
                    switch (expr[j]) {
                        'a'...'z', 'A'...'Z', '_', '0'...'9' => {},
                        else => break
                    }
                }

                try writeIdentifier(out, trimNamespace(expr[i .. j]));
                i = j;
                continue;
            },
            '0'...'9' => {
                var j = i;
                while (j < expr.len) : (j += 1) {
                    switch (expr[j]) {
                        '0'...'9', '.' => {},
                        else => break
                    }
                }

                if (mem.startsWith(u8, expr[j..], "f")) {
                    try out.print("@as(f32, {})", .{expr[i .. j]});
                    j += "f".len;
                } else if (mem.startsWith(u8, expr[j..], "ULL")) {
                    try out.print("@as(u32, {})", .{expr[i .. j]});
                    j += "ULL".len;
                } else if (mem.startsWith(u8, expr[j..], "U")) {
                    try out.print("@as(u64, {})", .{expr[i .. j]});
                    j += "U".len;
                } else {
                    try out.writeAll(expr[i .. j]);
                }

                i = j;
                continue;
            },
            ' ' => {},
            else => return error.InvalidConstantExpr
        }

        i += 1;
    }
}

fn renderApiConstants(out: var, registry: *Registry) !void {
    var it = registry.api_constants.iterator(0);
    while (it.next()) |constant| {
        try writeConstAssignmemt(out, trimNamespace(constant.name));
        try renderApiConstantExpr(out, constant.expr);
        try out.writeAll(";\n");
    }

    try out.writeAll("\n");
}

fn renderForeignTypes(out: var) !void {
    try writeConstAssignmemt(out, foreign_types_namespace);
    try out.writeAll("struct {\n");

    for (foreign_types) |fty| {
        try out.writeAll(base_indent);
        try writeConstAssignmemt(out, fty.name);
        try out.print("{};\n", .{fty.expr});
    }

    try out.writeAll("};\n");
}

fn renderDeclarations(out: var, registry: *Registry) !void {
    var it = registry.declarations.iterator(0);
    while (it.next()) |decl| {
        if (decl.definition == .Command) continue; // handled seperately

        switch (decl.definition) {
            .Enum => |*info| try renderEnum(out, decl.name, info),
            // .Alias => |alias| try renderAlias(out, registry, decl.name, alias),
            .FnPtr => |*info| try renderFnPtr(out, registry, decl.name, info),
            .Struct => |*info| try renderContainer(out, registry, .Struct, decl.name, info),
            .Union => |*info| try renderContainer(out, registry, .Union, decl.name, info),
            .BaseType => |type_info| {
                try writeConstAssignmemt(out, trimNamespace(decl.name));
                try renderTypeInfo(out, registry, type_info);
                try out.writeAll(";\n\n");
            },
            else => {}
        }
    }
}

fn shouldSkipEnum(enum_info: *reg.EnumInfo) bool {
    // Skip empty declarations (which are unused bitflags)
    return enum_info.variants.count() == 0;
}

fn renderEnum(out: var, name: []const u8, enum_info: *reg.EnumInfo) !void {
    if (shouldSkipEnum(enum_info)) {
        return;
    }

    const trimmed_name = trimNamespace(name);

    try writeConstAssignmemt(out, trimmed_name);
    try out.writeAll("extern enum {\n");

    // Calculate the length of the enum namespace, by iterating through the segments
    // of the variant (in screaming snake case) and comparing it to the name of the enum,
    // until the two differ.
    var prefix_len: usize = 0;
    var snake_prefix_len: usize = 0;
    var segment_it = mem.split(enum_info.variants.at(0).name, "_");
    while (segment_it.next()) |segment| {
        if (prefix_len + segment.len <= name.len and eqlIgnoreCase(segment, name[prefix_len .. prefix_len + segment.len])) {
            prefix_len += segment.len;
            snake_prefix_len += segment.len + 1; // Add one for the underscore
        } else {
            break;
        }
    }

    var it = enum_info.variants.iterator(0);
    while (it.next()) |variant| {
        if (variant.value == .Alias) continue; // Skip aliases

        try out.writeAll(base_indent);
        try writeIdentifier(out, variant.name[snake_prefix_len ..]);

        switch (variant.value) {
            .Value => |value| try out.print(" = {},\n", .{value}),
            .HexValue => |value| try out.print(" = 0x{X},\n", .{value}),
            .Bitpos => |value| try out.print(" = 1 << {},\n", .{value}),
            .Alias => unreachable
        }
    }

    try out.writeAll("};\n\n");
}

fn renderAlias(out: var, registry: *Registry, name: []const u8, alias: []const u8) !void {
    // An declaration may be aliased to a bit flag enum which has no members, in which case
    // the alias also has to be skipped.
    var def = registry.findDefinitionByName(alias).?;
    while (def.* == .Alias) {
        def = registry.findDefinitionByName(def.Alias).?;
    }

    if (def.* == .Enum and shouldSkipEnum(&def.Enum)) {
        return;
    }

    try writeConstAssignmemt(out, trimNamespace(name));
    try writeIdentifier(out, trimNamespace(alias));
    try out.writeAll(";\n\n");
}

fn renderFnPtr(out: var, registry: *Registry, name: []const u8, info: *reg.CommandInfo) !void {
    try writeConstAssignmemt(out, trimNamespace(name));
    try out.writeAll("?fn(");

    if (info.parameters.count() > 0) {
        try out.writeAll("\n");
        var it = info.parameters.iterator(0);
        while (it.next()) |param| {
            try out.writeAll(base_indent);
            try writeIdentifier(out, param.name);
            try out.writeAll(": ");
            try renderTypeInfo(out, registry, param.type_info);
            try out.writeAll(",\n");
        }
    }

    try out.writeAll(") callconv(.C) ");
    try renderTypeInfo(out, registry, info.return_type_info);
    try out.writeAll(";\n\n");
}

fn renderContainer(out: var, registry: *Registry, kind: enum{Struct, Union}, name: []const u8, info: *reg.ContainerInfo) !void {
    try writeConstAssignmemt(out, trimNamespace(name));

    switch (kind) {
        .Struct => try out.writeAll("extern struct {\n"),
        .Union => try out.writeAll("extern union {\n")
    }

    var it = info.members.iterator(0);
    while (it.next()) |member| {
        try out.writeAll(base_indent);
        try writeIdentifier(out, member.name);
        try out.writeAll(": ");
        try renderTypeInfo(out, registry, member.type_info);
        try out.writeAll(",\n");
    }

    try out.writeAll("};\n\n");
}

fn renderTest(out: var) !void {
    try out.writeAll(
        \\test "Semantic analysis" {
        \\    std.meta.refAllDecls(@This());
        \\}
        \\
    );
}
