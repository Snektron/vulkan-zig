const std = @import("std");
const unicode = std.unicode;
const testing = std.testing;

fn Utf8Iterator(comptime ReadError: type) type {
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

pub fn PeekUtf8Iterator(comptime buffer_size: usize, comptime ReadError: type) type {
    return struct {
        const Self = @This();
        pub const Stream = Utf8Iterator(ReadError).Stream;

        it: Utf8Iterator(ReadError),
        buf: [buffer_size]u32,
        head: usize,
        size: usize,

        pub fn init(in: *Utf8Iterator(ReadError).Stream) Self {
            return Self{
                .it = Utf8Iterator(ReadError).init(in),
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
                const cp = (try self.it.next()) orelse return null;
                self.buf[(self.head + self.size) % buffer_size] = cp;
                self.size += 1;
            }

            return self.buf[(self.head + offset) % buffer_size];
        }

        pub fn peekNoEof(self: *Self, offset: usize) !?u32 {
            return (try self.peek(offset)) orelse return error.EndOfStream;
        }

        pub fn discard(self: *Self, amount: usize) void {
            std.debug.assert(amount < self.size);

            var i: usize = 0;
            while (i < amount) : (i += 1) {
                _ = self.next();
            }
        }
    };
}

test "PeekUtf8Iterator" {
    var slice_in = std.io.SliceInStream.init("abcd");
    var it = PeekUtf8Iterator(4, std.io.SliceInStream.Error).init(&slice_in.stream);
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
    testing.expectError(error.EndOfStream, it.peekNoEof(1));
}
