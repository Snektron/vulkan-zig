const std = @import("std");
const ast = std.zig.ast;
const mem = std.mem;
const Allocator = mem.Allocator;
const reg = @import("registry.zig");
const Registry = reg.Registry;

const base_indent = " " ** 4;

pub fn render(out: var, registry: *Registry) !void {
    try renderApiConstants(out, registry);
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

fn writeIdentifier(out: var, name: []const u8) !void {
    if (!isValidZigIdentifier(name) or std.zig.Token.getKeyword(name) != null) {
        try out.print("@\"{}\"", .{name});
    } else {
        try out.write(name);
    }
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
        try out.write("const ");
        try writeIdentifier(out, trimNamespace(constant.name));
        try out.write(" = ");
        try renderApiConstantExpr(out, constant.expr);
        try out.write(";\n");
    }
}

fn renderDeclarations(out: var, registry: *Registry) !void {
    var it = registry.declarations.iterator(0);
    while (it.next()) |decl| {
        if (decl.definition == .Command) continue; // handled seperately

        switch (decl.definition) {
            .Enum => |*info| try renderEnum(out, registry, decl.name, info),
            else => {}
        }
    }
}

fn renderEnum(out: var, registry: *Registry, name: []const u8, enum_info: *reg.EnumInfo) !void {
    if (enum_info.variants.count() == 0) {
        return; // Skip empty declarations, which are unused bitflags
    }

    const trimmed_name = trimNamespace(name);

    try out.write("const ");
    try writeIdentifier(out, trimmed_name);
    try out.write(" = extern enum {\n");

    var prefix_len: usize = 0;
    var snake_prefix_len: usize = 0;
    var segment_it = mem.separate(enum_info.variants.at(0).name, "_");
    while (segment_it.next()) |segment| {
        if (prefix_len + segment.len <= name.len and eqlIgnoreCase(segment, name[prefix_len .. prefix_len + segment.len])) {
            prefix_len += segment.len;
            snake_prefix_len += segment.len + 1;
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

fn renderTest(out: var) !void {
    try out.write(
        \\test "Semantic analysis" {
        \\    @import("std").meta.refAllDecls(@This());
        \\}
        \\
    );
}
