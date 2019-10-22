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

fn XmlUtf8Iterator(comptime ReadError: type) type {
    return struct {
        const Self = @This();
        pub const Stream = std.io.InStream(ReadError);

        in: *Stream,

        pub fn init(in: *Stream) Self {
            return Self{
                .in = in
            };
        }

        pub fn next(self: *Self) !?u32 {
            const cp = (try self.nextCodepoint()) orelse return null;
            return if (cp == '&') try self.nextEntity() else cp;
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

fn testXmlUtf8Iterator(text: []const u8, expected: []const u8) !void {
    var slice_in = std.io.SliceInStream.init(text);
    var it = XmlUtf8Iterator(std.io.SliceInStream.Error).init(&slice_in.stream);

    var i: usize = 0;
    defer testing.expect(i == expected.len);

    while (try it.next()) |cp| {
        testing.expect(cp == expected[i]);
        i += 1;
    }
}

test "XmlUtf8Iterator" {
    try testXmlUtf8Iterator("simpleabc", "simpleabc");
    try testXmlUtf8Iterator("a&lt;b&amp;c&gt;d&quot;e&apos;f&lt;", "a<b&c>d\"e'f<");
    testing.expectError(error.InvalidEntity, testXmlUtf8Iterator("python&", "python"));
    testing.expectError(error.InvalidEntity, testXmlUtf8Iterator("python&&", "python"));
    testing.expectError(error.InvalidEntity, testXmlUtf8Iterator("python&test;", "python"));
    testing.expectError(error.InvalidEntity, testXmlUtf8Iterator("python&boa", "python"));
}

pub fn PeekXmlUtf8Iterator(comptime buffer_size: usize, comptime ReadError: type) type {
    return struct {
        const Self = @This();
        pub const Stream = XmlUtf8Iterator(ReadError).Stream;

        it: XmlUtf8Iterator(ReadError),
        buf: [buffer_size]u32,
        head: usize,
        size: usize,

        pub fn init(in: *XmlUtf8Iterator(ReadError).Stream) Self {
            return Self{
                .it = XmlUtf8Iterator(ReadError).init(in),
                .buf = undefined,
                .head = 0,
                .size = 0
            };
        }

        pub fn next(self: *Self) !?u32 {
            if (self.size > 0) {
                const cp = self.buf[self.head];
                self.head = (self.head + 1) % buffer_size;
                self.size -= 1;
                return cp;
            }

            return try self.it.next();
        }

        pub fn peek(self: *Self, offset: usize) !?u32 {
            std.debug.assert(offset < buffer_size);
            while (self.size <= offset) {
                self.buf[(self.head + self.size) % buffer_size] = (try self.it.next()) orelse return null;
                self.size += 1;
            }

            return self.buf[(self.head + offset) % buffer_size];
        }

        pub fn peekCheckStr(self: *Self, str: []const u8) !bool {
            for (str) |c, i| {
                if (try self.peek(i) != c) {
                    return false;
                }
            }

            return true;
        }
    };
}

test "PeekXmlUtf8Iterator" {
    var slice_in = std.io.SliceInStream.init("abcd");
    var it = PeekXmlUtf8Iterator(4, std.io.SliceInStream.Error).init(&slice_in.stream);
    testing.expect((try it.peek(0)).? == 'a');
    testing.expect((try it.peek(1)).? == 'b');
    testing.expect((try it.peek(2)).? == 'c');
    testing.expect((try it.peek(3)).? == 'd');

    testing.expect((try it.next()).? == 'a');
    testing.expect((try it.peek(0)).? == 'b');
    testing.expect((try it.peek(1)).? == 'c');
    testing.expect((try it.peek(2)).? == 'd');

    testing.expect((try it.next()).? == 'b');
    testing.expect((try it.peek(0)).? == 'c');
    testing.expect((try it.peek(1)).? == 'd');
    testing.expect((try it.peek(2)) == null);

    testing.expect((try it.next()).? == 'c');
    testing.expect((try it.peek(0)).? == 'd');
    testing.expect((try it.peek(1)) == null);
}

pub const Event = enum {
    open,
    open_pi,
    open_comment,
    close,
    close_slash,
    close_pi,
    close_comment,
    attrib_key,
    attrib_value,
    content
};

pub fn Parser(ReadError: type) type {
    return struct {
        const Self = @This();
        const Iterator = PeekXmlUtf8Iterator(4, ReadError);

        in: Iterator,
        state: Event,

        pub fn init(in: *Iterator.Stream) Self {
            return Self{
                .in = Iterator.init(in),
                .state = .content
            };
        }

        pub fn nextEvent(self: *Self) !?Event {
            const state = (try self.parseNextEvent()) orelse return null;
            return state;
        }

        fn parseNextEvent(self: *Self) !?Event {
            while (true) {
                switch ((try self.in.peek(0)) orelse return null) {
                    ' ', '\t', '\n', '\r' => {
                        self.discard(1);
                        continue;
                    },
                    '<' => {
                        switch (try self.peekNoEof(1)) {
                            '?' => {
                                self.discard(2);
                                return .open_pi;
                            },
                            '!' => {
                                self.discard(2);
                                try self.matchStr("--");
                                return .open_comment;
                            },
                            else => {
                                self.discard(1);
                                return .open;
                            }
                        }
                    },
                    '>' => {
                        self.discard(1);
                        return .close;
                    },
                    '?' => {
                        self.discard(1);
                        try self.matchChar('>');
                        return .close_pi;
                    },
                    '/' => {
                        self.discard(1);
                        try self.matchChar('>');
                        return .close_slash;
                    },
                    '-' => {
                        self.discard(1);
                        try self.matchStr("-!>");
                        return .close_comment;
                    },
                    else => return .content
                }
            }
        }

        pub fn next(self: *Self) !?u32 {
            switch (self.state) {
                .open => {
                    switch (try self.peekNoEof(0)) {
                        '>' => return null,
                        '/' => {
                            if ((try self.peekNoEof(1)) == '>') {
                                return null;
                            }
                        },
                        else => |cp| {
                            self.discard(1);
                            return cp;
                        }
                    }
                },
                .open_pi => {
                    switch (try self.peekNoEof(0)) {
                        '?' => {
                            if ((try self.peekNoEof(1)) == '>') {
                                return null;
                            }
                        },
                        else => |cp| {
                            self.discard(1);
                            return cp;
                        }
                    }
                },
                .open_comment => {
                    if (try self.in.peekCheckStr("--!>")) {
                        return null;
                    }

                    const cp = try self.peekNoEof(0);
                    self.discard(1);
                    return cp;
                }
            }
        }

        fn matchStr(self: *Self, seq: []const u8) !void {
            for (seq) |c| {
                try self.matchChar(c);
            }
        }

        fn matchChar(self: *Self, expected: u32) !void {
            if ((try self.peekNoEof(0)) != expected) {
                return error.Syntax;
            }

            self.discard(1);
        }

        fn peekNoEof(self: *Self, offset: usize) !u32 {
            return (try self.in.peek(offset)) orelse error.EndOfStream;
        }

        fn discard(self: *Self, num: usize) void {
            var i: usize = 0;
            while (i < num) : (i += 1) {
                _ = self.in.next();
            }
        }
    };
}
