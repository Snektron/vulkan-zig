const std = @import("std");
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

pub const TagType = enum {
    tag,
    proc_instr
};

pub const Node = struct {
    pub name: []const u8,
    pub type: TagType,
    pub attributes: []Attribute,
    pub children: []Element
};

pub const Attribute = struct {
    pub key: []const u8,
    pub value: []const u8
};

pub const Element = union(enum) {
    pub text: []const u8,
    pub node: *Node
};

pub const Document = struct {
    arena: ArenaAllocator,

    pub xml_decl: ?*Node,
    pub root: *Node,

    pub fn deinit(self: *Document) void {
        self.arena.deinit();
    }
};

const Parser = struct {
    alloc: *Allocator,
    source: []const u8,
    offset: usize,

    fn element(self: *Self) !Element {

    }

    fn text(self: *Self) ![]const u8 {
        const start = self.offset;
        const end = if (std.mem.indexOfPos(self.source, self.offset, "<")) |offset| offset else self.source.len;
        self.offset = end;
        return self.source[start .. end]; // TODO: Decode
    }

    fn node(self: *Self) !*Node {
        std.debug.assert(try self.peekNoEof(0) == '<');
    }

    fn peekNoEof(self: *const Self, offset: usize) !u8 {
        if (offset + self.offset >= self.source.len) {
            return error.EndOfStream;
        }

        return self.source[offset + self.offset];
    }
};

pub fn parse(alloc: *Allocator, source: []const u8) !Document {
    var arena = ArenaAllocator.init(alloc);
    var parser = Parser {
        .alloc = alloc,
        .source = source,
        .offset = 0
    };
}
