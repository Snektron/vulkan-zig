const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

pub const CaseStyle = enum {
    snake,
    screaming_snake,
    title,
    camel,
};

pub const SegmentIterator = struct {
    text: []const u8,
    offset: usize,

    pub fn init(text: []const u8) SegmentIterator {
        return .{
            .text = text,
            .offset = 0,
        };
    }

    fn nextBoundary(self: SegmentIterator) usize {
        var i = self.offset + 1;

        while (true) {
            if (i == self.text.len or self.text[i] == '_') {
                return i;
            }

            const prev_lower = std.ascii.isLower(self.text[i - 1]);
            const next_lower = std.ascii.isLower(self.text[i]);

            if (prev_lower and !next_lower) {
                return i;
            } else if (i != self.offset + 1 and !prev_lower and next_lower) {
                return i - 1;
            }

            i += 1;
        }
    }

    pub fn next(self: *SegmentIterator) ?[]const u8 {
        while (self.offset < self.text.len and self.text[self.offset] == '_') {
            self.offset += 1;
        }

        if (self.offset == self.text.len) {
            return null;
        }

        const end = self.nextBoundary();
        const word = self.text[self.offset .. end];
        self.offset = end;
        return word;
    }

    pub fn rest(self: SegmentIterator) []const u8 {
        if (self.offset >= self.text.len) {
            return &[_]u8{};
        } else {
            return self.text[self.offset..];
        }
    }
};

pub const IdRenderer = struct {
    tags: []const []const u8,
    text_cache: std.ArrayList(u8),

    pub fn init(allocator: *Allocator, tags: []const []const u8) IdRenderer {
        return .{
            .tags = tags,
            .text_cache = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn deinit(self: IdRenderer) void {
        self.text_cache.deinit();
    }

    fn renderSnake(self: *IdRenderer, screaming: bool, id: []const u8, tag: ?[]const u8) !void {
        var it = SegmentIterator.init(id);
        var first = true;
        const transform = if (screaming) std.ascii.toUpper else std.ascii.toLower;

        while (it.next()) |segment| {
            if (first) {
                first = false;
            } else {
                try self.text_cache.append('_');
            }

            for (segment) |c| {
                try self.text_cache.append(transform(c));
            }
        }

        if (tag) |name| {
            try self.text_cache.append('_');

            for (name) |c| {
                try self.text_cache.append(transform(c));
            }
        }
    }

    fn renderCamel(self: *IdRenderer, title: bool, id: []const u8, tag: ?[]const u8) !void {
        var it = SegmentIterator.init(id);
        var lower_first = !title;

        while (it.next()) |segment| {
            var i: usize = 0;
            while (i < segment.len and std.ascii.isDigit(segment[i])) {
                try self.text_cache.append(segment[i]);
                i += 1;
            }

            if (i == segment.len) {
                continue;
            }

            if (i == 0 and lower_first) {
                try self.text_cache.append(std.ascii.toLower(segment[i]));
            } else {
                try self.text_cache.append(std.ascii.toUpper(segment[i]));
            }
            lower_first = false;

            for (segment[i + 1..]) |c| {
                try self.text_cache.append(std.ascii.toLower(c));
            }
        }

        if (tag) |name| {
            try self.text_cache.appendSlice(name);
        }
    }

    pub fn render(self: IdRenderer, out: anytype, id: []const u8) !void {
        try out.print("{}", .{ std.zig.fmtId(id) });
    }

    pub fn renderFmt(self: *IdRenderer, out: anytype, comptime fmt: []const u8, args: anytype) !void {
        self.text_cache.items.len = 0;
        try std.fmt.format(self.text_cache.writer(), fmt, args);
        try out.print("{}", .{ std.zig.fmtId(self.text_cache.items) });
    }

    pub fn renderWithCase(self: *IdRenderer, out: anytype, case_style: CaseStyle, id: []const u8) !void {
        const tag = self.getAuthorTag(id);
        // The trailing underscore doesn't need to be removed here as its removed by the SegmentIterator.
        const adjusted_id = if (tag) |name| id[0 .. id.len - name.len] else id;

        self.text_cache.items.len = 0;

        switch (case_style) {
            .snake => try self.renderSnake(false, adjusted_id, tag),
            .screaming_snake => try self.renderSnake(true, adjusted_id, tag),
            .title => try self.renderCamel(true, adjusted_id, tag),
            .camel => try self.renderCamel(false, adjusted_id, tag),
        }

        try out.print("{}", .{ std.zig.fmtId(self.text_cache.items) });
    }

    pub fn getAuthorTag(self: IdRenderer, id: []const u8) ?[]const u8 {
        for (self.tags) |tag| {
            if (mem.endsWith(u8, id, tag)) {
                return tag;
            }
        }

        return null;
    }

    pub fn stripAuthorTag(self: IdRenderer, id: []const u8) []const u8 {
        if (self.getAuthorTag(id)) |tag| {
            return mem.trimRight(u8, id[0 .. id.len - tag.len], "_");
        }

        return id;
    }
};
