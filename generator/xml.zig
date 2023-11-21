const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const Allocator = mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

pub const Attribute = struct {
    name: []const u8,
    value: []const u8,
};

pub const Content = union(enum) {
    char_data: []const u8,
    comment: []const u8,
    element: *Element,
};

pub const Element = struct {
    tag: []const u8,
    attributes: []Attribute = &.{},
    children: []Content = &.{},

    pub fn getAttribute(self: Element, attrib_name: []const u8) ?[]const u8 {
        for (self.attributes) |child| {
            if (mem.eql(u8, child.name, attrib_name)) {
                return child.value;
            }
        }

        return null;
    }

    pub fn getCharData(self: Element, child_tag: []const u8) ?[]const u8 {
        const child = self.findChildByTag(child_tag) orelse return null;
        if (child.children.len != 1) {
            return null;
        }

        return switch (child.children[0]) {
            .char_data => |char_data| char_data,
            else => null,
        };
    }

    pub fn iterator(self: Element) ChildIterator {
        return .{
            .items = self.children,
            .i = 0,
        };
    }

    pub fn elements(self: Element) ChildElementIterator {
        return .{
            .inner = self.iterator(),
        };
    }

    pub fn findChildByTag(self: Element, tag: []const u8) ?*Element {
        var it = self.findChildrenByTag(tag);
        return it.next();
    }

    pub fn findChildrenByTag(self: Element, tag: []const u8) FindChildrenByTagIterator {
        return .{
            .inner = self.elements(),
            .tag = tag,
        };
    }

    pub const ChildIterator = struct {
        items: []Content,
        i: usize,

        pub fn next(self: *ChildIterator) ?*Content {
            if (self.i < self.items.len) {
                self.i += 1;
                return &self.items[self.i - 1];
            }

            return null;
        }
    };

    pub const ChildElementIterator = struct {
        inner: ChildIterator,

        pub fn next(self: *ChildElementIterator) ?*Element {
            while (self.inner.next()) |child| {
                if (child.* != .element) {
                    continue;
                }

                return child.*.element;
            }

            return null;
        }
    };

    pub const FindChildrenByTagIterator = struct {
        inner: ChildElementIterator,
        tag: []const u8,

        pub fn next(self: *FindChildrenByTagIterator) ?*Element {
            while (self.inner.next()) |child| {
                if (!mem.eql(u8, child.tag, self.tag)) {
                    continue;
                }

                return child;
            }

            return null;
        }
    };
};

pub const Document = struct {
    arena: ArenaAllocator,
    xml_decl: ?*Element,
    root: *Element,

    pub fn deinit(self: Document) void {
        var arena = self.arena; // Copy to stack so self can be taken by value.
        arena.deinit();
    }
};

const Parser = struct {
    source: []const u8,
    offset: usize,
    line: usize,
    column: usize,

    fn init(source: []const u8) Parser {
        return .{
            .source = source,
            .offset = 0,
            .line = 0,
            .column = 0,
        };
    }

    fn peek(self: *Parser) ?u8 {
        return if (self.offset < self.source.len) self.source[self.offset] else null;
    }

    fn consume(self: *Parser) !u8 {
        if (self.offset < self.source.len) {
            return self.consumeNoEof();
        }

        return error.UnexpectedEof;
    }

    fn consumeNoEof(self: *Parser) u8 {
        std.debug.assert(self.offset < self.source.len);
        const c = self.source[self.offset];
        self.offset += 1;

        if (c == '\n') {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }

        return c;
    }

    fn eat(self: *Parser, char: u8) bool {
        self.expect(char) catch return false;
        return true;
    }

    fn expect(self: *Parser, expected: u8) !void {
        if (self.peek()) |actual| {
            if (expected != actual) {
                return error.UnexpectedCharacter;
            }

            _ = self.consumeNoEof();
            return;
        }

        return error.UnexpectedEof;
    }

    fn eatStr(self: *Parser, text: []const u8) bool {
        self.expectStr(text) catch return false;
        return true;
    }

    fn expectStr(self: *Parser, text: []const u8) !void {
        if (self.source.len < self.offset + text.len) {
            return error.UnexpectedEof;
        } else if (mem.startsWith(u8, self.source[self.offset..], text)) {
            var i: usize = 0;
            while (i < text.len) : (i += 1) {
                _ = self.consumeNoEof();
            }

            return;
        }

        return error.UnexpectedCharacter;
    }

    fn eatWs(self: *Parser) bool {
        var ws = false;

        while (self.peek()) |ch| {
            switch (ch) {
                ' ', '\t', '\n', '\r' => {
                    ws = true;
                    _ = self.consumeNoEof();
                },
                else => break,
            }
        }

        return ws;
    }

    fn expectWs(self: *Parser) !void {
        if (!self.eatWs()) return error.UnexpectedCharacter;
    }

    fn currentLine(self: Parser) []const u8 {
        var begin: usize = 0;
        if (mem.lastIndexOfScalar(u8, self.source[0..self.offset], '\n')) |prev_nl| {
            begin = prev_nl + 1;
        }

        const end = mem.indexOfScalarPos(u8, self.source, self.offset, '\n') orelse self.source.len;
        return self.source[begin..end];
    }
};

test "xml: Parser" {
    {
        var parser = Parser.init("I like pythons");
        try testing.expectEqual(@as(?u8, 'I'), parser.peek());
        try testing.expectEqual(@as(u8, 'I'), parser.consumeNoEof());
        try testing.expectEqual(@as(?u8, ' '), parser.peek());
        try testing.expectEqual(@as(u8, ' '), try parser.consume());

        try testing.expect(parser.eat('l'));
        try testing.expectEqual(@as(?u8, 'i'), parser.peek());
        try testing.expectEqual(false, parser.eat('a'));
        try testing.expectEqual(@as(?u8, 'i'), parser.peek());

        try parser.expect('i');
        try testing.expectEqual(@as(?u8, 'k'), parser.peek());
        try testing.expectError(error.UnexpectedCharacter, parser.expect('a'));
        try testing.expectEqual(@as(?u8, 'k'), parser.peek());

        try testing.expect(parser.eatStr("ke"));
        try testing.expectEqual(@as(?u8, ' '), parser.peek());

        try testing.expect(parser.eatWs());
        try testing.expectEqual(@as(?u8, 'p'), parser.peek());
        try testing.expectEqual(false, parser.eatWs());
        try testing.expectEqual(@as(?u8, 'p'), parser.peek());

        try testing.expectEqual(false, parser.eatStr("aaaaaaaaa"));
        try testing.expectEqual(@as(?u8, 'p'), parser.peek());

        try testing.expectError(error.UnexpectedEof, parser.expectStr("aaaaaaaaa"));
        try testing.expectEqual(@as(?u8, 'p'), parser.peek());
        try testing.expectError(error.UnexpectedCharacter, parser.expectStr("pytn"));
        try testing.expectEqual(@as(?u8, 'p'), parser.peek());
        try parser.expectStr("python");
        try testing.expectEqual(@as(?u8, 's'), parser.peek());
    }

    {
        var parser = Parser.init("");
        try testing.expectEqual(parser.peek(), null);
        try testing.expectError(error.UnexpectedEof, parser.consume());
        try testing.expectEqual(parser.eat('p'), false);
        try testing.expectError(error.UnexpectedEof, parser.expect('p'));
    }
}

pub const ParseError = error{
    IllegalCharacter,
    UnexpectedEof,
    UnexpectedCharacter,
    UnclosedValue,
    UnclosedComment,
    InvalidName,
    InvalidEntity,
    InvalidStandaloneValue,
    NonMatchingClosingTag,
    InvalidDocument,
    OutOfMemory,
};

pub fn parse(backing_allocator: Allocator, source: []const u8) !Document {
    var parser = Parser.init(source);
    return try parseDocument(&parser, backing_allocator);
}

fn parseDocument(parser: *Parser, backing_allocator: Allocator) !Document {
    var doc = Document{
        .arena = ArenaAllocator.init(backing_allocator),
        .xml_decl = null,
        .root = undefined,
    };

    errdefer doc.deinit();

    const allocator = doc.arena.allocator();

    try skipComments(parser, allocator);

    doc.xml_decl = try parseElement(parser, allocator, .xml_decl);
    _ = parser.eatWs();
    try skipComments(parser, allocator);

    doc.root = (try parseElement(parser, allocator, .element)) orelse return error.InvalidDocument;
    _ = parser.eatWs();
    try skipComments(parser, allocator);

    if (parser.peek() != null) return error.InvalidDocument;

    return doc;
}

fn parseAttrValue(parser: *Parser, alloc: Allocator) ![]const u8 {
    const quote = try parser.consume();
    if (quote != '"' and quote != '\'') return error.UnexpectedCharacter;

    const begin = parser.offset;

    while (true) {
        const c = parser.consume() catch return error.UnclosedValue;
        if (c == quote) break;
    }

    const end = parser.offset - 1;

    return try unescape(alloc, parser.source[begin..end]);
}

fn parseEqAttrValue(parser: *Parser, alloc: Allocator) ![]const u8 {
    _ = parser.eatWs();
    try parser.expect('=');
    _ = parser.eatWs();

    return try parseAttrValue(parser, alloc);
}

fn parseNameNoDupe(parser: *Parser) ![]const u8 {
    // XML's spec on names is very long, so to make this easier
    // we just take any character that is not special and not whitespace
    const begin = parser.offset;

    while (parser.peek()) |ch| {
        switch (ch) {
            ' ', '\t', '\n', '\r' => break,
            '&', '"', '\'', '<', '>', '?', '=', '/' => break,
            else => _ = parser.consumeNoEof(),
        }
    }

    const end = parser.offset;
    if (begin == end) return error.InvalidName;

    return parser.source[begin..end];
}

fn parseCharData(parser: *Parser, alloc: Allocator) !?[]const u8 {
    const begin = parser.offset;

    while (parser.peek()) |ch| {
        switch (ch) {
            '<' => break,
            else => _ = parser.consumeNoEof(),
        }
    }

    const end = parser.offset;
    if (begin == end) return null;

    return try unescape(alloc, parser.source[begin..end]);
}

fn parseContent(parser: *Parser, alloc: Allocator) ParseError!Content {
    if (try parseCharData(parser, alloc)) |cd| {
        return Content{ .char_data = cd };
    } else if (try parseComment(parser, alloc)) |comment| {
        return Content{ .comment = comment };
    } else if (try parseElement(parser, alloc, .element)) |elem| {
        return Content{ .element = elem };
    } else {
        return error.UnexpectedCharacter;
    }
}

fn parseAttr(parser: *Parser, alloc: Allocator) !?Attribute {
    const name = parseNameNoDupe(parser) catch return null;
    _ = parser.eatWs();
    try parser.expect('=');
    _ = parser.eatWs();
    const value = try parseAttrValue(parser, alloc);

    const attr = Attribute{
        .name = try alloc.dupe(u8, name),
        .value = value,
    };
    return attr;
}

const ElementKind = enum {
    xml_decl,
    element,
};

fn parseElement(parser: *Parser, alloc: Allocator, comptime kind: ElementKind) !?*Element {
    const start = parser.offset;

    const tag = switch (kind) {
        .xml_decl => blk: {
            if (!parser.eatStr("<?") or !mem.eql(u8, try parseNameNoDupe(parser), "xml")) {
                parser.offset = start;
                return null;
            }
            break :blk "xml";
        },
        .element => blk: {
            if (!parser.eat('<')) return null;
            const tag = parseNameNoDupe(parser) catch {
                parser.offset = start;
                return null;
            };
            break :blk tag;
        },
    };

    var attributes = std.ArrayList(Attribute).init(alloc);
    defer attributes.deinit();

    var children = std.ArrayList(Content).init(alloc);
    defer children.deinit();

    while (parser.eatWs()) {
        const attr = (try parseAttr(parser, alloc)) orelse break;
        try attributes.append(attr);
    }

    switch (kind) {
        .xml_decl => try parser.expectStr("?>"),
        .element => {
            if (!parser.eatStr("/>")) {
                try parser.expect('>');

                while (true) {
                    if (parser.peek() == null) {
                        return error.UnexpectedEof;
                    } else if (parser.eatStr("</")) {
                        break;
                    }

                    const content = try parseContent(parser, alloc);
                    try children.append(content);
                }

                const closing_tag = try parseNameNoDupe(parser);
                if (!mem.eql(u8, tag, closing_tag)) {
                    return error.NonMatchingClosingTag;
                }

                _ = parser.eatWs();
                try parser.expect('>');
            }
        },
    }

    const element = try alloc.create(Element);
    element.* = .{
        .tag = try alloc.dupe(u8, tag),
        .attributes = try attributes.toOwnedSlice(),
        .children = try children.toOwnedSlice(),
    };
    return element;
}

test "xml: parseElement" {
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    {
        var parser = Parser.init("<= a='b'/>");
        try testing.expectEqual(@as(?*Element, null), try parseElement(&parser, alloc, .element));
        try testing.expectEqual(@as(?u8, '<'), parser.peek());
    }

    {
        var parser = Parser.init("<python size='15' color = \"green\"/>");
        const elem = try parseElement(&parser, alloc, .element);
        try testing.expectEqualSlices(u8, elem.?.tag, "python");

        const size_attr = elem.?.attributes[0];
        try testing.expectEqualSlices(u8, size_attr.name, "size");
        try testing.expectEqualSlices(u8, size_attr.value, "15");

        const color_attr = elem.?.attributes[1];
        try testing.expectEqualSlices(u8, color_attr.name, "color");
        try testing.expectEqualSlices(u8, color_attr.value, "green");
    }

    {
        var parser = Parser.init("<python>test</python>");
        const elem = try parseElement(&parser, alloc, .element);
        try testing.expectEqualSlices(u8, elem.?.tag, "python");
        try testing.expectEqualSlices(u8, elem.?.children[0].char_data, "test");
    }

    {
        var parser = Parser.init("<a>b<c/>d<e/>f<!--g--></a>");
        const elem = try parseElement(&parser, alloc, .element);
        try testing.expectEqualSlices(u8, elem.?.tag, "a");
        try testing.expectEqualSlices(u8, elem.?.children[0].char_data, "b");
        try testing.expectEqualSlices(u8, elem.?.children[1].element.tag, "c");
        try testing.expectEqualSlices(u8, elem.?.children[2].char_data, "d");
        try testing.expectEqualSlices(u8, elem.?.children[3].element.tag, "e");
        try testing.expectEqualSlices(u8, elem.?.children[4].char_data, "f");
        try testing.expectEqualSlices(u8, elem.?.children[5].comment, "g");
    }
}

test "xml: parse prolog" {
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    {
        var parser = Parser.init("<?xmla version='aa'?>");
        try testing.expectEqual(@as(?*Element, null), try parseElement(&parser, a, .xml_decl));
        try testing.expectEqual(@as(?u8, '<'), parser.peek());
    }

    {
        var parser = Parser.init("<?xml version='aa'?>");
        const decl = try parseElement(&parser, a, .xml_decl);
        try testing.expectEqualSlices(u8, "aa", decl.?.getAttribute("version").?);
        try testing.expectEqual(@as(?[]const u8, null), decl.?.getAttribute("encoding"));
        try testing.expectEqual(@as(?[]const u8, null), decl.?.getAttribute("standalone"));
    }

    {
        var parser = Parser.init("<?xml version=\"ccc\" encoding = 'bbb' standalone   \t =   'yes'?>");
        const decl = try parseElement(&parser, a, .xml_decl);
        try testing.expectEqualSlices(u8, "ccc", decl.?.getAttribute("version").?);
        try testing.expectEqualSlices(u8, "bbb", decl.?.getAttribute("encoding").?);
        try testing.expectEqualSlices(u8, "yes", decl.?.getAttribute("standalone").?);
    }
}

fn skipComments(parser: *Parser, alloc: Allocator) !void {
    while ((try parseComment(parser, alloc)) != null) {
        _ = parser.eatWs();
    }
}

fn parseComment(parser: *Parser, alloc: Allocator) !?[]const u8 {
    if (!parser.eatStr("<!--")) return null;

    const begin = parser.offset;
    while (!parser.eatStr("-->")) {
        _ = parser.consume() catch return error.UnclosedComment;
    }

    const end = parser.offset - "-->".len;
    return try alloc.dupe(u8, parser.source[begin..end]);
}

fn unescapeEntity(text: []const u8) !u8 {
    const EntitySubstition = struct { text: []const u8, replacement: u8 };

    const entities = [_]EntitySubstition{
        .{ .text = "&lt;", .replacement = '<' },
        .{ .text = "&gt;", .replacement = '>' },
        .{ .text = "&amp;", .replacement = '&' },
        .{ .text = "&apos;", .replacement = '\'' },
        .{ .text = "&quot;", .replacement = '"' },
    };

    for (entities) |entity| {
        if (mem.eql(u8, text, entity.text)) return entity.replacement;
    }

    return error.InvalidEntity;
}

fn unescape(arena: Allocator, text: []const u8) ![]const u8 {
    const unescaped = try arena.alloc(u8, text.len);

    var j: usize = 0;
    var i: usize = 0;
    while (i < text.len) : (j += 1) {
        if (text[i] == '&') {
            const entity_end = 1 + (mem.indexOfScalarPos(u8, text, i, ';') orelse return error.InvalidEntity);
            unescaped[j] = try unescapeEntity(text[i..entity_end]);
            i = entity_end;
        } else {
            unescaped[j] = text[i];
            i += 1;
        }
    }

    return unescaped[0..j];
}

test "xml: unescape" {
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    try testing.expectEqualSlices(u8, "test", try unescape(a, "test"));
    try testing.expectEqualSlices(u8, "a<b&c>d\"e'f<", try unescape(a, "a&lt;b&amp;c&gt;d&quot;e&apos;f&lt;"));
    try testing.expectError(error.InvalidEntity, unescape(a, "python&"));
    try testing.expectError(error.InvalidEntity, unescape(a, "python&&"));
    try testing.expectError(error.InvalidEntity, unescape(a, "python&test;"));
    try testing.expectError(error.InvalidEntity, unescape(a, "python&boa"));
}

test "xml: top level comments" {
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const doc = try parse(a, "<?xml version='aa'?><!--comment--><python color='green'/><!--another comment-->");
    try testing.expectEqualSlices(u8, "python", doc.root.tag);
}
