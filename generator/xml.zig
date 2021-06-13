const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const Allocator = mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;

pub const Attribute = struct {
    name: []const u8,
    value: []const u8,
};

pub const Content = union(enum) {
    CharData: []const u8,
    Comment: []const u8,
    Element: *Element,
};

pub const Element = struct {
    pub const AttributeList = ArrayList(*Attribute);
    pub const ContentList = ArrayList(Content);

    tag: []const u8,
    attributes: AttributeList,
    children: ContentList,

    fn init(tag: []const u8, alloc: *Allocator) Element {
        return .{
            .tag = tag,
            .attributes = AttributeList.init(alloc),
            .children = ContentList.init(alloc),
        };
    }

    pub fn getAttribute(self: *Element, attrib_name: []const u8) ?[]const u8 {
        for (self.attributes.items) |child| {
            if (mem.eql(u8, child.name, attrib_name)) {
                return child.value;
            }
        }

        return null;
    }

    pub fn getCharData(self: *Element, child_tag: []const u8) ?[]const u8 {
        const child = self.findChildByTag(child_tag) orelse return null;
        if (child.children.items.len != 1) {
            return null;
        }

        return switch (child.children.items[0]) {
            .CharData => |char_data| char_data,
            else => null,
        };
    }

    pub fn iterator(self: *Element) ChildIterator {
        return .{
            .items = self.children.items,
            .i = 0,
        };
    }

    pub fn elements(self: *Element) ChildElementIterator {
        return .{
            .inner = self.iterator(),
        };
    }

    pub fn findChildByTag(self: *Element, tag: []const u8) ?*Element {
        return self.findChildrenByTag(tag).next();
    }

    pub fn findChildrenByTag(self: *Element, tag: []const u8) FindChildrenByTagIterator {
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
                if (child.* != .Element) {
                    continue;
                }

                return child.*.Element;
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

pub const XmlDecl = struct {
    version: []const u8,
    encoding: ?[]const u8,
    standalone: ?bool,
};

pub const Document = struct {
    arena: ArenaAllocator,
    xml_decl: ?*XmlDecl,
    root: *Element,

    pub fn deinit(self: Document) void {
        var arena = self.arena; // Copy to stack so self can be taken by value.
        arena.deinit();
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
            .column = 0,
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
        } else if (std.mem.startsWith(u8, self.source[self.offset..], text)) {
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
                else => break,
            }
        }

        return ws;
    }

    fn expectWs(self: *ParseContext) !void {
        if (!self.eatWs()) return error.UnexpectedCharacter;
    }

    fn currentLine(self: ParseContext) []const u8 {
        var begin: usize = 0;
        if (mem.lastIndexOfScalar(u8, self.source[0..self.offset], '\n')) |prev_nl| {
            begin = prev_nl + 1;
        }

        var end = mem.indexOfScalarPos(u8, self.source, self.offset, '\n') orelse self.source.len;
        return self.source[begin..end];
    }
};

test "ParseContext" {
    {
        var ctx = ParseContext.init("I like pythons");
        try testing.expectEqual(@as(?u8, 'I'), ctx.peek());
        try testing.expectEqual(@as(u8, 'I'), ctx.consumeNoEof());
        try testing.expectEqual(@as(?u8, ' '), ctx.peek());
        try testing.expectEqual(@as(u8, ' '), try ctx.consume());

        try testing.expect(ctx.eat('l'));
        try testing.expectEqual(@as(?u8, 'i'), ctx.peek());
        try testing.expectEqual(false, ctx.eat('a'));
        try testing.expectEqual(@as(?u8, 'i'), ctx.peek());

        try ctx.expect('i');
        try testing.expectEqual(@as(?u8, 'k'), ctx.peek());
        try testing.expectError(error.UnexpectedCharacter, ctx.expect('a'));
        try testing.expectEqual(@as(?u8, 'k'), ctx.peek());

        try testing.expect(ctx.eatStr("ke"));
        try testing.expectEqual(@as(?u8, ' '), ctx.peek());

        try testing.expect(ctx.eatWs());
        try testing.expectEqual(@as(?u8, 'p'), ctx.peek());
        try testing.expectEqual(false, ctx.eatWs());
        try testing.expectEqual(@as(?u8, 'p'), ctx.peek());

        try testing.expectEqual(false, ctx.eatStr("aaaaaaaaa"));
        try testing.expectEqual(@as(?u8, 'p'), ctx.peek());

        try testing.expectError(error.UnexpectedEof, ctx.expectStr("aaaaaaaaa"));
        try testing.expectEqual(@as(?u8, 'p'), ctx.peek());
        try testing.expectError(error.UnexpectedCharacter, ctx.expectStr("pytn"));
        try testing.expectEqual(@as(?u8, 'p'), ctx.peek());
        try ctx.expectStr("python");
        try testing.expectEqual(@as(?u8, 's'), ctx.peek());
    }

    {
        var ctx = ParseContext.init("");
        try testing.expectEqual(ctx.peek(), null);
        try testing.expectError(error.UnexpectedEof, ctx.consume());
        try testing.expectEqual(ctx.eat('p'), false);
        try testing.expectError(error.UnexpectedEof, ctx.expect('p'));
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

pub fn parse(backing_allocator: *Allocator, source: []const u8) !Document {
    var ctx = ParseContext.init(source);
    return try parseDocument(&ctx, backing_allocator);
}

fn parseDocument(ctx: *ParseContext, backing_allocator: *Allocator) !Document {
    var doc = Document{
        .arena = ArenaAllocator.init(backing_allocator),
        .xml_decl = null,
        .root = undefined,
    };

    errdefer doc.deinit();

    try trySkipComments(ctx, &doc.arena.allocator);

    doc.xml_decl = try tryParseProlog(ctx, &doc.arena.allocator);
    _ = ctx.eatWs();
    try trySkipComments(ctx, &doc.arena.allocator);

    doc.root = (try tryParseElement(ctx, &doc.arena.allocator)) orelse return error.InvalidDocument;
    _ = ctx.eatWs();
    try trySkipComments(ctx, &doc.arena.allocator);

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

    return try dupeAndUnescape(alloc, ctx.source[begin..end]);
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
            else => _ = ctx.consumeNoEof(),
        }
    }

    const end = ctx.offset;
    if (begin == end) return error.InvalidName;

    return ctx.source[begin..end];
}

fn tryParseCharData(ctx: *ParseContext, alloc: *Allocator) !?[]const u8 {
    const begin = ctx.offset;

    while (ctx.peek()) |ch| {
        switch (ch) {
            '<' => break,
            else => _ = ctx.consumeNoEof(),
        }
    }

    const end = ctx.offset;
    if (begin == end) return null;

    return try dupeAndUnescape(alloc, ctx.source[begin..end]);
}

fn parseContent(ctx: *ParseContext, alloc: *Allocator) ParseError!Content {
    if (try tryParseCharData(ctx, alloc)) |cd| {
        return Content{ .CharData = cd };
    } else if (try tryParseComment(ctx, alloc)) |comment| {
        return Content{ .Comment = comment };
    } else if (try tryParseElement(ctx, alloc)) |elem| {
        return Content{ .Element = elem };
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
        try element.attributes.append(attr);
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
        try element.children.append(content);
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
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var alloc = &arena.allocator;

    {
        var ctx = ParseContext.init("<= a='b'/>");
        try testing.expectEqual(@as(?*Element, null), try tryParseElement(&ctx, alloc));
        try testing.expectEqual(@as(?u8, '<'), ctx.peek());
    }

    {
        var ctx = ParseContext.init("<python size='15' color = \"green\"/>");
        const elem = try tryParseElement(&ctx, alloc);
        try testing.expectEqualSlices(u8, elem.?.tag, "python");

        const size_attr = elem.?.attributes.items[0];
        try testing.expectEqualSlices(u8, size_attr.name, "size");
        try testing.expectEqualSlices(u8, size_attr.value, "15");

        const color_attr = elem.?.attributes.items[1];
        try testing.expectEqualSlices(u8, color_attr.name, "color");
        try testing.expectEqualSlices(u8, color_attr.value, "green");
    }

    {
        var ctx = ParseContext.init("<python>test</python>");
        const elem = try tryParseElement(&ctx, alloc);
        try testing.expectEqualSlices(u8, elem.?.tag, "python");
        try testing.expectEqualSlices(u8, elem.?.children.items[0].CharData, "test");
    }

    {
        var ctx = ParseContext.init("<a>b<c/>d<e/>f<!--g--></a>");
        const elem = try tryParseElement(&ctx, alloc);
        try testing.expectEqualSlices(u8, elem.?.tag, "a");
        try testing.expectEqualSlices(u8, elem.?.children.items[0].CharData, "b");
        try testing.expectEqualSlices(u8, elem.?.children.items[1].Element.tag, "c");
        try testing.expectEqualSlices(u8, elem.?.children.items[2].CharData, "d");
        try testing.expectEqualSlices(u8, elem.?.children.items[3].Element.tag, "e");
        try testing.expectEqualSlices(u8, elem.?.children.items[4].CharData, "f");
        try testing.expectEqualSlices(u8, elem.?.children.items[5].Comment, "g");
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
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var alloc = &arena.allocator;

    {
        var ctx = ParseContext.init("<?xmla version='aa'?>");
        try testing.expectEqual(@as(?*XmlDecl, null), try tryParseProlog(&ctx, alloc));
        try testing.expectEqual(@as(?u8, '<'), ctx.peek());
    }

    {
        var ctx = ParseContext.init("<?xml version='aa'?>");
        const decl = try tryParseProlog(&ctx, alloc);
        try testing.expectEqualSlices(u8, "aa", decl.?.version);
        try testing.expectEqual(@as(?[]const u8, null), decl.?.encoding);
        try testing.expectEqual(@as(?bool, null), decl.?.standalone);
    }

    {
        var ctx = ParseContext.init("<?xml version=\"aa\" encoding = 'bbb' standalone   \t =   'yes'?>");
        const decl = try tryParseProlog(&ctx, alloc);
        try testing.expectEqualSlices(u8, "aa", decl.?.version);
        try testing.expectEqualSlices(u8, "bbb", decl.?.encoding.?);
        try testing.expectEqual(@as(?bool, true), decl.?.standalone.?);
    }
}

fn trySkipComments(ctx: *ParseContext, alloc: *Allocator) !void {
    while (try tryParseComment(ctx, alloc)) |_| {
        _ = ctx.eatWs();
    }
}

fn tryParseComment(ctx: *ParseContext, alloc: *Allocator) !?[]const u8 {
    if (!ctx.eatStr("<!--")) return null;

    const begin = ctx.offset;
    while (!ctx.eatStr("-->")) {
        _ = ctx.consume() catch return error.UnclosedComment;
    }

    const end = ctx.offset - "-->".len;
    return try mem.dupe(alloc, u8, ctx.source[begin..end]);
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
            const entity_end = 1 + (mem.indexOfScalarPos(u8, text, i, ';') orelse return error.InvalidEntity);
            str[j] = try unescapeEntity(text[i..entity_end]);
            i = entity_end;
        } else {
            str[j] = text[i];
            i += 1;
        }
    }

    return alloc.shrink(str, j);
}

test "dupeAndUnescape" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var alloc = &arena.allocator;

    try testing.expectEqualSlices(u8, "test", try dupeAndUnescape(alloc, "test"));
    try testing.expectEqualSlices(u8, "a<b&c>d\"e'f<", try dupeAndUnescape(alloc, "a&lt;b&amp;c&gt;d&quot;e&apos;f&lt;"));
    try testing.expectError(error.InvalidEntity, dupeAndUnescape(alloc, "python&"));
    try testing.expectError(error.InvalidEntity, dupeAndUnescape(alloc, "python&&"));
    try testing.expectError(error.InvalidEntity, dupeAndUnescape(alloc, "python&test;"));
    try testing.expectError(error.InvalidEntity, dupeAndUnescape(alloc, "python&boa"));
}

test "Top level comments" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var alloc = &arena.allocator;

    const doc = try parse(alloc, "<?xml version='aa'?><!--comment--><python color='green'/><!--another comment-->");
    try testing.expectEqualSlices(u8, "python", doc.root.tag);
}
