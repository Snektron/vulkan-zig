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
    .{.name = "Display", .expr = "@OpaqueType()"},
    .{.name = "VisualID", .expr = @typeName(c_uint)},
    .{.name = "Window", .expr = @typeName(c_ulong)},
    .{.name = "RROutput", .expr = @typeName(c_ulong)},
    .{.name = "wl_display", .expr = "@OpaqueType()"},
    .{.name = "wl_surface", .expr = "@OpaqueType()"},
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
    .{.name = "ANativeWindow", .expr = "@OpaqueType()"},
    .{.name = "AHardwareBuffer", .expr = "@OpaqueType()"},
    .{.name = "CAMetalLayer", .expr = "@OpaqueType()"},
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
    try out.write("const std = @import(\"std\");\n\n");
    try renderApiConstants(out, registry);
    try renderForeignTypes(out);
    try out.write("\n");
    try renderDeclarations(out, registry);
    try renderTest(out);
}

fn trimNamespace(name: []const u8) []const u8 {
    if (mem.startsWith(u8, name, "VK_")) {
        return name["VK_".len ..];
    } else if (mem.startsWith(u8, name, "vk")) {
        return name["vk".len ..];
    } else if (mem.startsWith(u8, name, "Vk")) {
        return name["Vk".len ..];
    } else if (mem.startsWith(u8, name, "PFN_vk")) {
        return name["PFN_vk".len..];
    } else {
        unreachable;
    }
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
    return mem.eql(u8, name, "comptime_float") or
        mem.eql(u8, name, "comptime_int") or
        mem.eql(u8, name, "bool") or
        mem.eql(u8, name, "isize") or
        mem.eql(u8, name, "usize") or
        mem.eql(u8, name, "f16") or
        mem.eql(u8, name, "f32") or
        mem.eql(u8, name, "f64") or
        mem.eql(u8, name, "f128") or
        mem.eql(u8, name, "c_longdouble") or
        mem.eql(u8, name, "noreturn") or
        mem.eql(u8, name, "type") or
        mem.eql(u8, name, "anyerror") or
        mem.eql(u8, name, "c_short") or
        mem.eql(u8, name, "c_ushort") or
        mem.eql(u8, name, "c_int") or
        mem.eql(u8, name, "c_uint") or
        mem.eql(u8, name, "c_long") or
        mem.eql(u8, name, "c_ulong") or
        mem.eql(u8, name, "c_longlong") or
        mem.eql(u8, name, "c_ulonglong");
}

fn writeIdentifier(out: var, name: []const u8) !void {
    if (!isValidZigIdentifier(name) or isZigReservedIdentifier(name) or std.zig.Token.getKeyword(name) != null) {
        try out.print("@\"{}\"", .{name});
    } else {
        try out.write(name);
    }
}

fn writeConstAssignmemt(out: var, name: []const u8) !void {
    try out.write("pub const ");
    try writeIdentifier(out, name);
    try out.write(" = ");
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
        try out.print("[{}]", .{array_size});
    }

    for (type_info.pointers) |ptr| {
        // Apparently Vulkan optional-ness information is not correct, so every pointer
        // is considered optional
        switch (ptr.size) {
            .One => try out.write("?*"),
            .Many => try out.write("?[*]"),
            .ZeroTerminated => try out.write("?[*:0]")
        }

        if (ptr.is_const) {
            try out.write("const ");
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
            try out.write(bty.zig_name);
            return;
        }
    }

    // If the type is a `void*`, it needs to be translated to `*c_void`.
    // If its not a pointer, its a return type, so `void` should be emitted.
    if (mem.eql(u8, type_info.name, "void")) {
        if (type_info.pointers.len > 0) {
            try out.write("c_void");
        } else {
            try out.write("void");
        }

        return;
    }

    // Make sure the type is defined by Vulkan otherwise
    if (registry.findDefinitionByName(type_info.name) == null) {
        return error.InvalidType;
    }

    try writeIdentifier(out, trimNamespace(type_info.name));
}

fn renderApiConstantExpr(out: var, constexpr: []const u8) !void {
    // There are only a few different kinds of tokens in the expressions,
    // all of which can be tokenized by the Zig tokenizer. The only parts which cannot
    // be parsed properly are 'f', 'U', and 'ULL' suffixes.
    // Render the C expression by simply substituting those values

    // omit enclosing parenthesis
    const expr = if (constexpr[0] == '(' and constexpr[constexpr.len - 1] == ')')
        constexpr[1 .. constexpr.len - 1]
    else
        constexpr;

    var tokenizer = std.zig.Tokenizer.init(expr);
    var peek_tok: ?std.zig.Token = null;

    while (true) {
        const tok = peek_tok orelse tokenizer.next();
        const text = expr[tok.start .. tok.end];
        peek_tok = null;

        switch (tok.id) {
            .LParen, .RParen, .Tilde => try out.write(text),
            .Identifier => try writeIdentifier(out, trimNamespace(text)),
            .Minus => try out.write(" - "),
            .FloatLiteral => {
                try out.print("@as(f32, {})", .{text});

                // Float literal has to be followed by an 'f' identifier.
                const suffix = tokenizer.next();
                const suffix_text = expr[suffix.start .. suffix.end];
                if (suffix.id != .Identifier or !mem.eql(u8, suffix_text, "f")) {
                    return error.ExpectedFloatSuffix;
                }
            },
            .IntegerLiteral => {
                const suffix = tokenizer.next();
                const suffix_text = expr[suffix.start .. suffix.end];

                if (suffix.id != .Identifier) {
                    // Only need to check here because any identifier following an integer
                    // that is not 'U' or 'ULL' is a syntax error.
                    peek_tok = suffix;
                    try out.write(text);
                } else if (mem.eql(u8, suffix_text, "U")) {
                    try out.print("@as(u32, {})", .{text});
                } else if (mem.eql(u8, suffix_text, "ULL")) {
                    try out.print("@as(u64, {})", .{text});
                } else {
                    return error.InvalidIntSuffix;
                }
            },
            .Eof => return,
            else => return error.UnexpectedToken
        }
    }
}

fn renderApiConstants(out: var, registry: *Registry) !void {
    var it = registry.api_constants.iterator(0);
    while (it.next()) |constant| {
        try writeConstAssignmemt(out, trimNamespace(constant.name));
        try renderApiConstantExpr(out, constant.expr);
        try out.write(";\n");
    }

    try out.write("\n");
}

fn renderForeignTypes(out: var) !void {
    try writeConstAssignmemt(out, foreign_types_namespace);
    try out.write("struct {\n");

    for (foreign_types) |fty| {
        try out.write(base_indent);
        try writeConstAssignmemt(out, fty.name);
        try out.print("{};\n", .{fty.expr});
    }

    try out.write("};\n");
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
                try out.write(";\n\n");
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
    try out.write("extern enum {\n");

    // Calculate the length of the enum namespace, by iterating through the segments
    // of the variant (in screaming snake case) and comparing it to the name of the enum,
    // until the two differ.
    var prefix_len: usize = 0;
    var snake_prefix_len: usize = 0;
    var segment_it = mem.separate(enum_info.variants.at(0).name, "_");
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

        try out.write(base_indent);
        try writeIdentifier(out, variant.name[snake_prefix_len ..]);

        switch (variant.value) {
            .Value => |value| try out.print(" = {},\n", .{value}),
            .HexValue => |value| try out.print(" = 0x{X},\n", .{value}),
            .Bitpos => |value| try out.print(" = 1 << {},\n", .{value}),
            .Alias => unreachable
        }
    }

    try out.write("};\n\n");
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
    try out.write(";\n\n");
}

fn renderFnPtr(out: var, registry: *Registry, name: []const u8, info: *reg.CommandInfo) !void {
    try writeConstAssignmemt(out, trimNamespace(name));
    try out.write("extern fn(");

    if (info.parameters.count() > 0) {
        try out.write("\n");
        var it = info.parameters.iterator(0);
        while (it.next()) |param| {
            try out.write(base_indent);
            try writeIdentifier(out, param.name);
            try out.write(": ");
            try renderTypeInfo(out, registry, param.type_info);
            try out.write(",\n");
        }
    }

    try out.write(") ");
    try renderTypeInfo(out, registry, info.return_type_info);
    try out.write(";\n\n");
}

fn renderContainer(out: var, registry: *Registry, kind: enum{Struct, Union}, name: []const u8, info: *reg.ContainerInfo) !void {
    try writeConstAssignmemt(out, trimNamespace(name));

    switch (kind) {
        .Struct => try out.write("extern struct {\n"),
        .Union => try out.write("extern union {\n")
    }

    var it = info.members.iterator(0);
    while (it.next()) |member| {
        try out.write(base_indent);
        try writeIdentifier(out, member.name);
        try out.write(": ");
        try renderTypeInfo(out, registry, member.type_info);
        try out.write(",\n");
    }

    try out.write("};\n\n");
}

fn renderTest(out: var) !void {
    try out.write(
        \\test "Semantic analysis" {
        \\    std.meta.refAllDecls(@This());
        \\}
        \\
    );
}
