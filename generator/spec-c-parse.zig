const std = @import("std");
const registry = @import("registry-new.zig");
const xml = @import("xml.zig");
const mem = std.mem;
const Allocator = mem.Allocator;
const testing = std.testing;
const ArraySize = registry.Array.ArraySize;

const Token = struct {
    id: Id,
    text: []const u8,

    const Id = enum {
        id, // Any id thats not a keyword
        name, // Vulkan <name>...</name>
        type_name, // Vulkan <type>...</type>
        enum_name, // Vulkan <enum>...</enum>
        int,
        star,
        comma,
        semicolon,
        colon,
        lparen,
        rparen,
        lbracket,
        rbracket,
        kw_typedef,
        kw_const,
        kw_vkapi_ptr,
        kw_struct,
    };
};

const CTokenizer = struct {
    source: []const u8,
    offset: usize = 0,

    fn peek(self: CTokenizer) ?u8 {
        return if (self.offset < self.source.len) self.source[self.offset] else null;
    }

    fn consumeNoEof(self: *CTokenizer) u8 {
        const c = self.peek().?;
        self.offset += 1;
        return c;
    }

    fn consume(self: *CTokenizer) !u8 {
        return if (self.offset < self.source.len)
                return self.consumeNoEof()
            else
                return null;
    }

    fn keyword(self: *CTokenizer) Token {
        const start = self.offset;
        _ = self.consumeNoEof();

        while (true) {
            const c = self.peek() orelse break;
            switch (c) {
                'A'...'Z', 'a'...'z', '_', '0'...'9' => _ = self.consumeNoEof(),
                else => break,
            }
        }

        const token_text = self.source[start .. self.offset];

        const id = if (mem.eql(u8, token_text, "typedef"))
                Token.Id.kw_typedef
            else if (mem.eql(u8, token_text, "const"))
                Token.Id.kw_const
            else if (mem.eql(u8, token_text, "VKAPI_PTR"))
                Token.Id.kw_vkapi_ptr
            else if (mem.eql(u8, token_text, "struct"))
                Token.Id.kw_struct
            else
                Token.Id.id;

        return .{.id = id, .text = token_text};
    }

    fn int(self: *CTokenizer) Token {
        const start = self.offset;
        _ = self.consumeNoEof();

        // TODO: 123ABC is now legal
        while (true) {
            const c = self.peek() orelse break;
            switch (c) {
                '0'...'9' => _ = self.consumeNoEof(),
                else => break,
            }
        }

        return .{
            .id = .int,
            .text = self.source[start .. self.offset],
        };
    }

    fn next(self: *CTokenizer) !?Token {
        while (true) {
            switch (self.peek() orelse return null) {
                ' ', '\t', '\n', '\r' => _ = self.consumeNoEof(),
                else => break,
            }
        }

        const c = self.peek().?;
        var id: Token.Id = undefined;
        switch (c) {
            'A'...'Z', 'a'...'z', '_' => return self.keyword(),
            '0'...'9' => return self.int(),
            '*' => id = .star,
            ',' => id = .comma,
            ';' => id = .semicolon,
            ':' => id = .colon,
            '[' => id = .lbracket,
            ']' => id = .rbracket,
            '(' => id = .lparen,
            ')' => id = .rparen,
            else => return error.UnexpectedCharacter
        }

        const start = self.offset;
        _ = self.consumeNoEof();
        return Token{
            .id = id,
            .text = self.source[start .. self.offset]
        };
    }
};

pub const XmlCTokenizer = struct {
    it: xml.Element.ContentList.Iterator,
    ctok: ?CTokenizer = null,
    current: ?Token = null,

    pub fn init(elem: *xml.Element) XmlCTokenizer {
        return .{
            .it = elem.children.iterator(0),
        };
    }

    fn elemToToken(elem: *xml.Element) !?Token {
        if (elem.children.count() != 1 or elem.children.at(0).* != .CharData) {
            return error.InvalidXml;
        }

        const text = elem.children.at(0).CharData;
        if (mem.eql(u8, elem.tag, "type")) {
            return Token{.id = .type_name, .text = text};
        } else if (mem.eql(u8, elem.tag, "enum")) {
            return Token{.id = .enum_name, .text = text};
        } else if (mem.eql(u8, elem.tag, "name")) {
            return Token{.id = .name, .text = text};
        } else if (mem.eql(u8, elem.tag, "comment")) {
            return null;
        } else {
            return error.InvalidTag;
        }
    }

    fn next(self: *XmlCTokenizer) !?Token {
        if (self.current) |current| {
            const token = current;
            self.current = null;
            return token;
        }

        while (true) {
            if (self.ctok) |*ctok| {
                if (try ctok.next()) |tok| {
                    return tok;
                }
            }

            self.ctok = null;

            if (self.it.next()) |child| {
                switch (child.*) {
                    .CharData => |cdata| self.ctok = CTokenizer{.source = cdata},
                    .Comment => {},
                    .Element => |elem| if (try elemToToken(elem)) |tok| return tok,
                }
            } else {
                return null;
            }
        }
    }

    fn nextNoEof(self: *XmlCTokenizer) !Token {
        return (try self.next()) orelse return error.InvalidSyntax;
    }

    fn peek(self: *XmlCTokenizer) !?Token {
        if (self.current) |current| {
            return current;
        }

        self.current = try self.next();
        return self.current;
    }

    fn peekNoEof(self: *XmlCTokenizer) !Token {
        return (try self.peek()) orelse return error.InvalidSyntax;
    }

    fn expect(self: *XmlCTokenizer, id: Token.Id) !Token {
        const tok = (try self.next()) orelse return error.UnexpectedEof;
        if (tok.id != id) {
            return error.UnexpectedToken;
        }

        return tok;
    }
};

// TYPEDEF = kw_typedef DECLARATION ';'
pub fn parseTypedef(allocator: *Allocator, xctok: *XmlCTokenizer) !registry.Declaration {
    _ = try xctok.expect(.kw_typedef);
    const decl = try parseDeclaration(allocator, xctok);
    _ = try xctok.expect(.semicolon);
    if (try xctok.peek()) |_| {
        return error.InvalidSyntax;
    }

    return decl;
}

// MEMBER = DECLARATION (':' int)?
pub fn parseMember(allocator: *Allocator, xctok: *XmlCTokenizer) !registry.Container.Field {
    const decl = try parseDeclaration(allocator, xctok);
    var field = registry.Container.Field {
        .name = decl.name,
        .field_type = decl.decl_type,
        .bits = null,
    };

    if (try xctok.peek()) |tok| {
        if (tok.id != .colon) {
            return error.InvalidSyntax;
        }

        _ = try xctok.nextNoEof();
        const bits = try xctok.expect(.int);
        field.bits = try std.fmt.parseInt(usize, bits.text, 10);

        // Assume for now that there won't be any invalid C types like `char char* x : 4`.

        if (try xctok.peek()) |_| {
            return error.InvalidSyntax;
        }
    }

    return field;
}

pub fn parseParamOrProto(allocator: *Allocator, xctok: *XmlCTokenizer) !registry.Declaration {
    const decl = try parseDeclaration(allocator, xctok);
    if (try xctok.peek()) |_| {
        return error.InvalidSyntax;
    }
    return decl;
}

// DECLARATION = kw_const? type_name DECLARATOR
// DECLARATOR = POINTERS (id | name) ('[' ARRAY_DECLARATOR ']')*
//     | POINTERS '(' kw_vkapi_ptr '*' name' ')' // TODO
fn parseDeclaration(allocator: *Allocator, xctok: *XmlCTokenizer) !registry.Declaration {
    // Parse declaration constness
    var tok = try xctok.nextNoEof();
    const inner_is_const = tok.id == .kw_const;
    if (inner_is_const) {
        tok = try xctok.nextNoEof();
    }

    if (tok.id == .kw_struct) {
        tok = try xctok.nextNoEof();
    }

    // Parse type name
    if (tok.id != .type_name) return error.InvalidSyntax;
    const type_name = tok.text;

    var type_info = registry.TypeInfo{.alias = type_name};

    // Parse pointers
    type_info = try parsePointers(allocator, xctok, inner_is_const, type_info);

    // Parse name / fn ptr
    tok = try xctok.nextNoEof();
    if (tok.id == .lparen) { // Assume this declaration is a function pointer
        unreachable; // WIP
    } else if (tok.id != .id and tok.id != .name) {
        return error.InvalidSyntax;
    }

    const name = tok.text;
    var inner_type = &type_info;

    while (try parseArrayDeclarator(xctok)) |array_size| {
        // Move the current inner type to a new node on the heap
        const child = try allocator.create(registry.TypeInfo);
        child.* = inner_type.*;

        // Re-assign the previous inner type for the array type info node
        inner_type.* = .{
            .array = .{
                .size = array_size,
                .child = child,
            }
        };

        // update the inner_type pointer so it points to the proper
        // inner type again
        inner_type = child;
    }

    return registry.Declaration {
        .name = name,
        .decl_type = type_info,
    };
}

// POINTERS = (kw_const? '*')*
fn parsePointers(
    allocator: *Allocator,
    xctok: *XmlCTokenizer,
    inner_const: bool,
    inner: registry.TypeInfo,
) !registry.TypeInfo {
    var type_info = inner;
    var first_const = inner_const;

    while (true) {
        var tok = (try xctok.peek()) orelse return type_info;
        var is_const = first_const;
        first_const = false;

        if (tok.id == .kw_const) {
            is_const = true;
            _ = try xctok.nextNoEof();
            tok = (try xctok.peek()) orelse return type_info;
        }

        if (tok.id != .star) {
            // if `is_const` is true at this point, there was a trailing const,
            // and the declaration itself is const.
            return type_info;
        }

        _ = try xctok.nextNoEof();

        const child = try allocator.create(registry.TypeInfo);
        child.* = type_info;

        type_info = .{
            .pointer = .{
                .size = .one, // set elsewhere
                .is_const = is_const or first_const,
                .child = child,
            },
        };
    }
}

// ARRAY_DECLARATOR = '[' (int | enum_name) ']'
fn parseArrayDeclarator(xctok: *XmlCTokenizer) !?ArraySize {
    const lbracket = try xctok.peek();
    if (lbracket == null or lbracket.?.id != .lbracket) {
        return null;
    }

    _ = try xctok.nextNoEof();

    const size_tok = try xctok.nextNoEof();
    const size: ArraySize = switch (size_tok.id) {
        .int => .{.int = try std.fmt.parseInt(usize, size_tok.text, 10)},
        .enum_name => .{.alias = size_tok.text},
        else => return error.InvalidSyntax
    };

    _ = try xctok.expect(.rbracket);
    return size;
}

fn testTokenizer(tokenizer: var, expected_tokens: []const Token) void {
    for (expected_tokens) |expected| {
        const tok = (tokenizer.next() catch unreachable).?;
        testing.expectEqual(expected.id, tok.id);
        testing.expectEqualSlices(u8, expected.text, tok.text);
    }

    if (tokenizer.next() catch unreachable) |_| unreachable;
}

test "CTokenizer" {
    var ctok = CTokenizer {
        .source = "typedef ([const)]** VKAPI_PTR 123,;aaaa"
    };

    testTokenizer(
        &ctok,
        &[_]Token{
            .{.id = .kw_typedef, .text = "typedef"},
            .{.id = .lparen, .text = "("},
            .{.id = .lbracket, .text = "["},
            .{.id = .kw_const, .text = "const"},
            .{.id = .rparen, .text = ")"},
            .{.id = .rbracket, .text = "]"},
            .{.id = .star, .text = "*"},
            .{.id = .star, .text = "*"},
            .{.id = .kw_vkapi_ptr, .text = "VKAPI_PTR"},
            .{.id = .int, .text = "123"},
            .{.id = .comma, .text = ","},
            .{.id = .semicolon, .text = ";"},
            .{.id = .id, .text = "aaaa"},
        }
    );
}

test "XmlCTokenizer" {
    const document = try xml.parse(
        testing.allocator,
        "<root>typedef void (VKAPI_PTR *<name>PFN_vkVoidFunction</name>)(void);</root>"
    );
    defer document.deinit();

    var xctok = XmlCTokenizer.init(document.root);

    testTokenizer(
        &xctok,
        &[_]Token{
            .{.id = .kw_typedef, .text = "typedef"},
            .{.id = .id, .text = "void"},
            .{.id = .lparen, .text = "("},
            .{.id = .kw_vkapi_ptr, .text = "VKAPI_PTR"},
            .{.id = .star, .text = "*"},
            .{.id = .name, .text = "PFN_vkVoidFunction"},
            .{.id = .rparen, .text = ")"},
            .{.id = .lparen, .text = "("},
            .{.id = .id, .text = "void"},
            .{.id = .rparen, .text = ")"},
            .{.id = .semicolon, .text = ";"},
        }
    );
}

test "parseTypedef" {
    const document = try xml.parse(
        testing.allocator,
        "<root>typedef const struct <type>Python</type>* pythons[4];</root>"
    );
    defer document.deinit();

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    var xctok = XmlCTokenizer.init(document.root);
    const decl = try parseTypedef(&arena.allocator, &xctok);

    testing.expectEqualSlices(u8, "pythons", decl.name);
    testing.expectEqual(registry.TypeInfo.array, decl.decl_type);
    testing.expectEqual(ArraySize{.int = 4}, decl.decl_type.array.size);
    const array_child = decl.decl_type.array.child.*;
    testing.expectEqual(registry.TypeInfo.pointer, array_child);
    const ptr = array_child.pointer;
    testing.expectEqual(true, ptr.is_const);
    testing.expectEqual(registry.TypeInfo.alias, ptr.child.*);
    testing.expectEqualSlices(u8, "Python", ptr.child.alias);
}
