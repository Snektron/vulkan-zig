const std = @import("std");
const unicode = std.unicode;
const testing = std.testing;

const Entity = struct {
    substitute: []const u8,
    char: u8
};

const entities = [_]Entity{
    Entity{.substitute = "amp;", .char = '&'},
    Entity{.substitute = "lt;", .char = '<'},
    Entity{.substitute = "gt;", .char = '>'},
    Entity{.substitute = "quot;", .char = '"'},
    Entity{.substitute = "apos;", .char = '\''},
};

fn XmlUtf8Parser(comptime ReadError: type) type {
    return struct {
        pub const Self = @This();
        pub const Stream = std.io.InStream(ReadError);

        in: *Stream,
        current: ?u32,

        pub fn init(in: *Stream) Self {
            return Self{
                .in = in,
                .current = null
            };
        }

        pub fn consume(self: *Self) void {
            self.current = null;
        }

        pub fn peek(self: *Self) !?u32 {
            if (self.current) |cp| {
                return cp;
            }

            const cp = (try self.nextCodepoint()) orelse return null;
            self.current = if (cp == '&') try self.nextEntity() else cp;
            return self.current.?;
        }

        pub fn consumeAndPeek(self: *Self) !?u32 {
            self.consume();
            return try self.peek();
        }

        fn nextEntity(self: *Self) !u32 {
            var entity = [_]u8{0} ** 5;

            for (entity) |*c| {
                const cp = (try self.nextCodepoint()) orelse return error.InvalidEntity;
                c.* = std.math.cast(u8, cp) catch return error.InvalidEntity;

                if (cp == ';') {
                    break;
                }
            } else {
                return error.InvalidEntity;
            }

            for (entities) |*e| {
                if (std.mem.startsWith(u8, entity, e.substitute)) {
                    return e.char;
                }
            }

            return error.InvalidEntity;
        }

        fn nextCodepoint(self: *Self) !?u32 {
            var cp: [4]u8 = undefined;
            if ((try self.in.readFull(cp[0 .. 1])) != 1) {
                return null;
            }

            const cp_len = try unicode.utf8ByteSequenceLength(cp[0]);
            try self.in.readNoEof(cp[1 .. cp_len]);
            return try unicode.utf8Decode(cp[0 .. cp_len]);
        }
    };
}

fn testXmlUtf8Parser(text: []const u8, expected: []const u8) !void {
    var slice_in = std.io.SliceInStream.init(text);
    var p = XmlUtf8Parser(std.io.SliceInStream.Error).init(&slice_in.stream);

    var i: usize = 0;
    defer testing.expect(i == expected.len);

    while (try p.consumeAndPeek()) |cp| {
        testing.expect(cp == expected[i]);
        i += 1;
    }

    testing.expect((try p.consumeAndPeek()) == null);
}

test "XmlUtf8Parser" {
    try testXmlUtf8Parser("simpleabc", "simpleabc");
    try testXmlUtf8Parser("a&lt;b&amp;c&gt;d&quot;e&apos;f&lt;", "a<b&c>d\"e'f<");
    testing.expectError(error.InvalidEntity, testXmlUtf8Parser("python&", "python"));
    testing.expectError(error.InvalidEntity, testXmlUtf8Parser("python&&", "python"));
    testing.expectError(error.InvalidEntity, testXmlUtf8Parser("python&test;", "python"));
    testing.expectError(error.InvalidEntity, testXmlUtf8Parser("python&boa", "python"));

    var slice_in = std.io.SliceInStream.init("test");
    var p = XmlUtf8Parser(std.io.SliceInStream.Error).init(&slice_in.stream);

    testing.expect((try p.peek()).? == 't');
    testing.expect((try p.peek()).? == 't');
    p.consume();
    testing.expect((try p.peek()).? == 'e');
    testing.expect((try p.peek()).? == 'e');
    testing.expect((try p.consumeAndPeek()).? == 's');
    testing.expect((try p.consumeAndPeek()).? == 't');
    testing.expect((try p.consumeAndPeek()) == null);
    testing.expect((try p.peek()) == null);
}
