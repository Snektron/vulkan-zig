const std = @import("std");
const unicode = std.unicode;
const testing = std.testing;
const InStream = std.io.InStream;

pub const Text = struct {
    pub raw: []const u8,

    pub fn iter_unescaped(self: *Text) UnescapeIterator {
        return UnescapeIterator{
            .raw = self.raw,
            .i = 0
        };
    }
};

pub const UnescapeIterator = struct{
    raw: []const u8,
    i: usize,

    pub fn init(raw: []const u8) UnescapeIterator {
        return UnescapeIterator {
            .raw = raw,
            .i = 0
        };
    }

    fn nextCodepoint(self: *UnescapeIterator) !?u32 {
        if (self.i >= self.raw.len) {
            return null;
        }

        const cp_len = try unicode.utf8ByteSequenceLength(self.raw[self.i]);
        if (cp_len + self.i > self.raw.len) {
            return error.InvalidUtf8;
        }

        const cp = try unicode.utf8Decode(self.raw[self.i .. self.i + cp_len]);
        self.i += cp_len;
        return cp;
    }

    fn unescape(self: *UnescapeIterator) !u32 {
        var entity: [5]u8 = undefined;

        var offset: usize = 0;
        while (try self.nextCodepoint()) |cp| {
            entity[offset] = std.math.cast(u8, cp) catch return error.InvalidEntity;
            if (cp == ';') {
                break;
            }

            offset += 1;
            if (offset == entity.len) {
                return error.InvalidEntity;
            }
        } else {
            return error.InvalidEntity;
        }

        if (std.mem.startsWith(u8, entity, "lt;")) {
            return '<';
        } else if (std.mem.startsWith(u8, entity, "gt;")) {
            return '>';
        } else if (std.mem.startsWith(u8, entity, "amp;")) {
            return '&';
        } else if (std.mem.startsWith(u8, entity, "apos;")) {
            return '\'';
        } else if (std.mem.startsWith(u8, entity, "quot;")) {
            return '"';
        } else {
            return error.InvalidEntity;
        }
    }

    pub fn next(self: *UnescapeIterator) !?u32 {
        const cp = (try self.nextCodepoint()) orelse return null;

        if (cp == '&') {
            return try self.unescape();
        } else {
            return cp;
        }
    }
};

fn testUnescapeIterator(text: []const u8, expected: []const u8) !void {
    var it = UnescapeIterator.init(text);
    var i: usize = 0;
    defer testing.expect(i == expected.len);

    while (try it.next()) |cp| {
        testing.expect(cp == expected[i]);
        i += 1;
    }
}

test "unescape iterator" {
    try testUnescapeIterator("simpleabc", "simpleabc");
    try testUnescapeIterator("a&lt;b&amp;c&gt;d&quot;e&apos;f&lt;", "a<b&c>d\"e'f<");
    testing.expectError(error.InvalidEntity, testUnescapeIterator("oof&&", "oof"));
    testing.expectError(error.InvalidEntity, testUnescapeIterator("oof&test;", "oof"));
    testing.expectError(error.InvalidEntity, testUnescapeIterator("oof&pythons", "oof"));
}

pub const Event = union(enum) {
    open_tag,
    close_tag,
    attribute,
    text,
};

pub const Parser = struct {
    source: []const u8,

    pub fn init(source: []const u8) Parser {
        return Parser{
            .source = source
        };
    }
};
