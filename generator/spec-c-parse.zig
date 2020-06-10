const std = @import("std");
const registry = @import("registry-new.zig");
const xml = @import("xml.zig");
const mem = std.mem;
const testing = std.testing;

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
        lparen,
        rparen,
        lbracket,
        rbracket,
        kw_typedef,
        kw_const,
        kw_vkapi_ptr,
        whitespace,
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
            else
                Token.Id.id;

        return .{.id = id, .text = token_text};
    }

    fn whitespace(self: *CTokenizer) Token {
        const start = self.offset;
        _ = self.consumeNoEof();

        while (true) {
            const c = self.peek() orelse break;
            switch (c) {
                ' ', '\t', '\n', '\r' => _ = self.consumeNoEof(),
                else => break,
            }
        }

        return .{
            .id = .whitespace,
            .text = self.source[start .. self.offset],
        };
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
        const c = self.peek() orelse return null;
        var id: Token.Id = undefined;

        switch (c) {
            'A'...'Z', 'a'...'z', '_' => return self.keyword(),
            ' ', '\t', '\n', '\r' => return self.whitespace(),
            '0'...'9' => return self.int(),
            '*' => id = .star,
            ',' => id = .comma,
            ';' => id = .semicolon,
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

const XmlCTokenizer = struct {
    it: xml.Element.ContentList.Iterator,
    ctok: ?CTokenizer = null,

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
        } else {
            return error.InvalidTag;
        }
    }

    fn next(self: *XmlCTokenizer) !?Token {
        while (true) {
            if (self.ctok) |*ctok| {
                if (try ctok.next()) |tok| {
                    return tok;
                }
            }

            self.ctok = null;

            if (self.it.next()) |child| {
                switch (child.*) {
                    .CharData => |cdata| {
                        self.ctok = CTokenizer{.source = cdata};
                        continue;
                    },
                    .Comment => continue,
                    .Element => |elem| return try elemToToken(elem)
                }
            }

            return null;
        }
    }

    fn nextIgnoreWs(self: *XmlCTokenizer) !?Token {
        while (try self.next()) |tok| {
            if (tok.id != .whitespace) {
                return tok;
            }
        }

        return null;
    }
};

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
            .{.id = .whitespace, .text = " "},
            .{.id = .lparen, .text = "("},
            .{.id = .lbracket, .text = "["},
            .{.id = .kw_const, .text = "const"},
            .{.id = .rparen, .text = ")"},
            .{.id = .rbracket, .text = "]"},
            .{.id = .star, .text = "*"},
            .{.id = .star, .text = "*"},
            .{.id = .whitespace, .text = " "},
            .{.id = .kw_vkapi_ptr, .text = "VKAPI_PTR"},
            .{.id = .whitespace, .text = " "},
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

    var xctok = XmlCTokenizer{
        .it = document.root.children.iterator(0)
    };

    testTokenizer(
        &xctok,
        &[_]Token{
            .{.id = .kw_typedef, .text = "typedef"},
            .{.id = .whitespace, .text = " "},
            .{.id = .id, .text = "void"},
            .{.id = .whitespace, .text = " "},
            .{.id = .lparen, .text = "("},
            .{.id = .kw_vkapi_ptr, .text = "VKAPI_PTR"},
            .{.id = .whitespace, .text = " "},
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
