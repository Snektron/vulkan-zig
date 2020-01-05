const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const Allocator = mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const SegmentedList = std.SegmentedList;

pub const Attribute = struct {
    name: []const u8,
    value: []const u8
};

pub const Content = union(enum) {
    CharData: []const u8,
    Comment: []const u8,
    Element: *Element
};

// Wrapper to work around compiler crash
pub const Child = struct {
    content: Content
};

pub const Element = struct {
    tag: []const u8,
    attributes: SegmentedList(*Attribute, 0),
    children: SegmentedList(Child, 0),

    fn init(tag: []const u8, alloc: *Allocator) Element {
        return .{
            .tag = tag,
            .attributes = SegmentedList(*Attribute, 0).init(alloc),
            .children = SegmentedList(Child, 0).init(alloc),
        };
    }
};

pub const XmlDecl = struct {
    version: []const u8,
    encoding: ?[]const u8,
    standalone: ?bool
};

pub const Document = struct {
    arena: ArenaAllocator,
    xml_decl: ?*XmlDecl,
    root: *Element,

    pub fn deinit(self: *Document) void {
        self.arena.deinit();
    }
};

const ParseContext = struct {
    source: []const u8,
    offset: usize,
    line: usize,
    column: usize,

    fn init(source: []const u8) ParseContext {
        return .{
            .source = source,
            .offset = 0,
            .line = 0,
            .column = 0
        };
    }

    fn peek(self: *ParseContext) ?u8 {
        return if (self.offset < self.source.len) self.source[self.offset] else null;
    }

    fn consume(self: *ParseContext) !u8 {
        if (self.offset < self.source.len) {
            return self.consumeNoEof();
        }

        return error.UnexpectedEof;
    }

    fn consumeNoEof(self: *ParseContext) u8 {
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

    fn eat(self: *ParseContext, char: u8) bool {
        self.expect(char) catch return false;
        return true;
    }

    fn expect(self: *ParseContext, expected: u8) !void {
        if (self.peek()) |actual| {
            if (expected != actual) {
                return error.UnexpectedCharacter;
            }

            _ = self.consumeNoEof();
            return;
        }

        return error.UnexpectedEof;
    }

    fn eatStr(self: *ParseContext, text: []const u8) bool {
        self.expectStr(text) catch return false;
        return true;
    }

    fn expectStr(self: *ParseContext, text: []const u8) !void {
        if (self.source.len < self.offset + text.len) {
            return error.UnexpectedEof;
        } else if (std.mem.startsWith(u8, self.source[self.offset ..], text)) {
            var i: usize = 0;
            while (i < text.len) : (i += 1) {
                _ = self.consumeNoEof();
            }

            return;
        }

        return error.UnexpectedCharacter;
    }

    fn eatWs(self: *ParseContext) bool {
        var ws = false;

        while (self.peek()) |ch| {
            switch (ch) {
                ' ', '\t', '\n', '\r' => {
                    ws = true;
                    _ = self.consumeNoEof();
                },
                else => break
            }
        }

        return ws;
    }

    fn expectWs(self: *ParseContext) !void {
        if (!self.eatWs()) return error.UnexpectedCharacter;
    }

    fn currentLine(self: ParseContext) []const u8 {
        var begin: usize = 0;
        if (mem.lastIndexOf(u8, self.source[0 .. self.offset], "\n")) |prev_nl| {
            begin = prev_nl + 1;
        }

        var end = mem.indexOfPos(u8, self.source, self.offset, "\n") orelse self.source.len;
        return self.source[begin .. end];
    }
};

test "ParseContext" {
    {
        var ctx = ParseContext.init("I like pythons");
        testing.expectEqual(@as(?u8, 'I'), ctx.peek());
        testing.expectEqual(@as(u8, 'I'), ctx.consumeNoEof());
        testing.expectEqual(@as(?u8, ' '), ctx.peek());
        testing.expectEqual(@as(u8, ' '), try ctx.consume());

        testing.expect(ctx.eat('l'));
        testing.expectEqual(@as(?u8, 'i'), ctx.peek());
        testing.expectEqual(false, ctx.eat('a'));
        testing.expectEqual(@as(?u8, 'i'), ctx.peek());

        try ctx.expect('i');
        testing.expectEqual(@as(?u8, 'k'), ctx.peek());
        testing.expectError(error.UnexpectedCharacter, ctx.expect('a'));
        testing.expectEqual(@as(?u8, 'k'), ctx.peek());

        testing.expect(ctx.eatStr("ke"));
        testing.expectEqual(@as(?u8, ' '), ctx.peek());

        testing.expect(ctx.eatWs());
        testing.expectEqual(@as(?u8, 'p'), ctx.peek());
        testing.expectEqual(false, ctx.eatWs());
        testing.expectEqual(@as(?u8, 'p'), ctx.peek());

        testing.expectEqual(false, ctx.eatStr("aaaaaaaaa"));
        testing.expectEqual(@as(?u8, 'p'), ctx.peek());

        testing.expectError(error.UnexpectedEof, ctx.expectStr("aaaaaaaaa"));
        testing.expectEqual(@as(?u8, 'p'), ctx.peek());
        testing.expectError(error.UnexpectedCharacter, ctx.expectStr("pytn"));
        testing.expectEqual(@as(?u8, 'p'), ctx.peek());
        try ctx.expectStr("python");
        testing.expectEqual(@as(?u8, 's'), ctx.peek());
    }

    {
        var ctx = ParseContext.init("");
        testing.expectEqual(ctx.peek(), null);
        testing.expectError(error.UnexpectedEof, ctx.consume());
        testing.expectEqual(ctx.eat('p'), false);
        testing.expectError(error.UnexpectedEof, ctx.expect('p'));
    }
}

pub const ParseError = error {
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
    OutOfMemory
};

pub fn parse(backing_allocator: *Allocator, source: []const u8) !Document {
    var ctx = ParseContext.init(source);
    return parseDocument(&ctx, backing_allocator) catch |err| {
        std.debug.warn("{}\n", .{ctx.currentLine()});

        var i: usize = 0;
        while (i < ctx.column) : (i += 1) {
            std.debug.warn(" ", .{});
        }

        std.debug.warn("^\n", .{});

        return err;
    };
}

fn parseDocument(ctx: *ParseContext, backing_allocator: *Allocator) !Document {
    var doc = Document{
        .arena = ArenaAllocator.init(backing_allocator),
        .xml_decl = null,
        .root = undefined
    };

    doc.xml_decl = try tryParseProlog(ctx, &doc.arena.allocator);
    _ = ctx.eatWs();
    doc.root = (try tryParseElement(ctx, &doc.arena.allocator)) orelse return error.InvalidDocument;
    _ = ctx.eatWs();

    if (ctx.peek() != null) return error.InvalidDocument;

    return doc;
}

fn parseAttrValue(ctx: *ParseContext, alloc: *Allocator) ![]const u8 {
    const quote = try ctx.consume();
    if (quote != '"' and quote != '\'') return error.UnexpectedCharacter;

    const begin = ctx.offset;

    while (true) {
        const c = ctx.consume() catch return error.UnclosedValue;
        if (c == quote) break;
    }

    const end = ctx.offset - 1;

    return try dupeAndUnescape(alloc, ctx.source[begin .. end]);
}

fn parseEqAttrValue(ctx: *ParseContext, alloc: *Allocator) ![]const u8 {
    _ = ctx.eatWs();
    try ctx.expect('=');
    _ = ctx.eatWs();

    return try parseAttrValue(ctx, alloc);
}

fn parseNameNoDupe(ctx: *ParseContext) ![]const u8 {
    // XML's spec on names is very long, so to make this easier
    // we just take any character that is not special and not whitespace

    const begin = ctx.offset;

    while (ctx.peek()) |ch| {
        switch (ch) {
            ' ', '\t', '\n', '\r' => break,
            '&', '"', '\'', '<', '>', '?', '=', '/' => break,
            else => _ = ctx.consumeNoEof()
        }
    }

    const end = ctx.offset;
    if (begin == end) return error.InvalidName;

    return ctx.source[begin .. end];
}

fn tryParseCharData(ctx: *ParseContext, alloc: *Allocator) !?[]const u8 {
    const begin = ctx.offset;

    while (ctx.peek()) |ch| {
        switch (ch) {
            '<', '>' => break,
            else => _ = ctx.consumeNoEof()
        }
    }

    const end = ctx.offset;
    if (begin == end) return null;

    return try dupeAndUnescape(alloc, ctx.source[begin .. end]);
}

fn parseContent(ctx: *ParseContext, alloc: *Allocator) ParseError!Content {
    if (try tryParseCharData(ctx, alloc)) |cd| {
        return Content{.CharData = cd};
    } else if (try tryParseComment(ctx, alloc)) |comment| {
        return Content{.Comment = comment};
    } else if (try tryParseElement(ctx, alloc)) |elem| {
        return Content{.Element = elem};
    } else {
        return error.UnexpectedCharacter;
    }
}

fn tryParseAttr(ctx: *ParseContext, alloc: *Allocator) !?*Attribute {
    const name = parseNameNoDupe(ctx) catch return null;
    _ = ctx.eatWs();
    try ctx.expect('=');
    _ = ctx.eatWs();
    const value = try parseAttrValue(ctx, alloc);

    const attr = try alloc.create(Attribute);
    attr.name = try mem.dupe(alloc, u8, name);
    attr.value = value;
    return attr;
}

fn tryParseElement(ctx: *ParseContext, alloc: *Allocator) !?*Element {
    const start = ctx.offset;
    if (!ctx.eat('<')) return null;
    const tag = parseNameNoDupe(ctx) catch {
        ctx.offset = start;
        return null;
    };

    const element = try alloc.create(Element);
    element.* = Element.init(try std.mem.dupe(alloc, u8, tag), alloc);

    while (ctx.eatWs()) {
        const attr = (try tryParseAttr(ctx, alloc)) orelse break;
        try element.attributes.push(attr);
    }

    if (ctx.eatStr("/>")) {
        return element;
    }

    try ctx.expect('>');

    while (true) {
        if (ctx.peek() == null) {
            return error.UnexpectedEof;
        } else if (ctx.eatStr("</")) {
            break;
        }

        const content = try parseContent(ctx, alloc);
        try element.children.push(.{.content = content});
    }

    const closing_tag = try parseNameNoDupe(ctx);
    if (!std.mem.eql(u8, tag, closing_tag)) {
        return error.NonMatchingClosingTag;
    }

    _ = ctx.eatWs();
    try ctx.expect('>');
    return element;
}

test "tryParseElement" {
    {
        var ctx = ParseContext.init("<= a='b'/>");
        testing.expectEqual(@as(?*Element, null), try tryParseElement(&ctx, std.debug.global_allocator));
        testing.expectEqual(@as(?u8, '<'), ctx.peek());
    }

    {
        var ctx = ParseContext.init("<python size='15' color = \"green\"/>");
        const elem = try tryParseElement(&ctx, std.debug.global_allocator);
        testing.expectEqualSlices(u8, elem.?.tag, "python");

        const size_attr = elem.?.attributes.at(0).*;
        testing.expectEqualSlices(u8, size_attr.name, "size");
        testing.expectEqualSlices(u8, size_attr.value, "15");

        const color_attr = elem.?.attributes.at(1).*;
        testing.expectEqualSlices(u8, color_attr.name, "color");
        testing.expectEqualSlices(u8, color_attr.value, "green");
    }

    {
        var ctx = ParseContext.init("<python>test</python>");
        const elem = try tryParseElement(&ctx, std.debug.global_allocator);
        testing.expectEqualSlices(u8, elem.?.tag, "python");
        testing.expectEqualSlices(u8, elem.?.children.at(0).content.CharData, "test");
    }

    {
        var ctx = ParseContext.init("<a>b<c/>d<e/>f<!--g--></a>");
        const elem = try tryParseElement(&ctx, std.debug.global_allocator);
        testing.expectEqualSlices(u8, elem.?.tag, "a");
        testing.expectEqualSlices(u8, elem.?.children.at(0).content.CharData, "b");
        testing.expectEqualSlices(u8, elem.?.children.at(1).content.Element.tag, "c");
        testing.expectEqualSlices(u8, elem.?.children.at(2).content.CharData, "d");
        testing.expectEqualSlices(u8, elem.?.children.at(3).content.Element.tag, "e");
        testing.expectEqualSlices(u8, elem.?.children.at(4).content.CharData, "f");
        testing.expectEqualSlices(u8, elem.?.children.at(5).content.Comment, "g");
    }
}

fn tryParseProlog(ctx: *ParseContext, alloc: *Allocator) !?*XmlDecl {
    const start = ctx.offset;
    if (!ctx.eatStr("<?") or !mem.eql(u8, try parseNameNoDupe(ctx), "xml")) {
        ctx.offset = start;
        return null;
    }

    const decl = try alloc.create(XmlDecl);
    decl.encoding = null;
    decl.standalone = null;

    // Version info is mandatory
    try ctx.expectWs();
    try ctx.expectStr("version");
    decl.version = try parseEqAttrValue(ctx, alloc);

    if (ctx.eatWs()) {
        // Optional encoding and standalone info
        var require_ws = false;

        if (ctx.eatStr("encoding")) {
            decl.encoding = try parseEqAttrValue(ctx, alloc);
            require_ws = true;
        }

        if (require_ws == ctx.eatWs() and ctx.eatStr("standalone")) {
            const standalone = try parseEqAttrValue(ctx, alloc);
            if (std.mem.eql(u8, standalone, "yes")) {
                decl.standalone = true;
            } else if (std.mem.eql(u8, standalone, "no")) {
                decl.standalone = false;
            } else {
                return error.InvalidStandaloneValue;
            }
        }

        _ = ctx.eatWs();
    }

    try ctx.expectStr("?>");
    return decl;
}

test "tryParseProlog" {
    {
        var ctx = ParseContext.init("<?xmla version='aa'?>");
        testing.expectEqual(@as(?*XmlDecl, null), try tryParseProlog(&ctx, std.debug.global_allocator));
        testing.expectEqual(@as(?u8, '<'), ctx.peek());
    }

    {
        var ctx = ParseContext.init("<?xml version='aa'?>");
        const decl = try tryParseProlog(&ctx, std.debug.global_allocator);
        testing.expectEqualSlices(u8, "aa", decl.?.version);
        testing.expectEqual(@as(?[]const u8, null), decl.?.encoding);
        testing.expectEqual(@as(?bool, null), decl.?.standalone);
    }

    {
        var ctx = ParseContext.init("<?xml version=\"aa\" encoding = 'bbb' standalone   \t =   'yes'?>");
        const decl = try tryParseProlog(&ctx, std.debug.global_allocator);
        testing.expectEqualSlices(u8, "aa", decl.?.version);
        testing.expectEqualSlices(u8, "bbb", decl.?.encoding.?);
        testing.expectEqual(@as(?bool, true), decl.?.standalone.?);
    }
}

fn tryParseComment(ctx: *ParseContext, alloc: *Allocator) !?[]const u8 {
    if (!ctx.eatStr("<!--")) return null;

    const begin = ctx.offset;
    while (!ctx.eatStr("-->")) {
        _ = ctx.consume() catch return error.UnclosedComment;
    }

    const end = ctx.offset - "-->".len;
    return try mem.dupe(alloc, u8, ctx.source[begin .. end]);
}

fn unescapeEntity(text: []const u8) !u8 {
    const EntitySubstition = struct {
        text: []const u8,
        replacement: u8
    };

    const entities = [_]EntitySubstition{
        .{.text = "&lt;", .replacement = '<'},
        .{.text = "&gt;", .replacement = '>'},
        .{.text = "&amp;", .replacement = '&'},
        .{.text = "&apos;", .replacement = '\''},
        .{.text = "&quot;", .replacement = '"'}
    };

    for (entities) |entity| {
        if (std.mem.eql(u8, text, entity.text)) return entity.replacement;
    }

    return error.InvalidEntity;
}

fn dupeAndUnescape(alloc: *Allocator, text: []const u8) ![]const u8 {
    const str = try alloc.alloc(u8, text.len);

    var j: usize = 0;
    var i: usize = 0;
    while (i < text.len) : (j += 1) {
        if (text[i] == '&') {
            const entity_end = 1 + (mem.indexOfPos(u8, text, i, ";") orelse return error.InvalidEntity);
            str[j] = try unescapeEntity(text[i .. entity_end]);
            i = entity_end;
        } else {
            str[j] = text[i];
            i += 1;
        }
    }

    return alloc.shrink(str, j);
}

test "dupeAndUnescape" {
    testing.expectEqualSlices(u8, "test", try dupeAndUnescape(std.debug.global_allocator, "test"));
    testing.expectEqualSlices(u8, "a<b&c>d\"e'f<", try dupeAndUnescape(std.debug.global_allocator, "a&lt;b&amp;c&gt;d&quot;e&apos;f&lt;"));
    testing.expectError(error.InvalidEntity, dupeAndUnescape(std.debug.global_allocator, "python&"));
    testing.expectError(error.InvalidEntity, dupeAndUnescape(std.debug.global_allocator, "python&&"));
    testing.expectError(error.InvalidEntity, dupeAndUnescape(std.debug.global_allocator, "python&test;"));
    testing.expectError(error.InvalidEntity, dupeAndUnescape(std.debug.global_allocator, "python&boa"));
}
