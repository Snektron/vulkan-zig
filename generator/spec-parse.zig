const std = @import("std");
const registry = @import("registry-new.zig");
const xml = @import("xml.zig");
const mem = std.mem;
const Allocator = mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

pub const ParseResult = struct {
    arena: ArenaAllocator,
    registry: registry.Registry,

    pub fn deinit(self: ParseResult) void {
        self.arena.deinit();
    }
};

pub fn parseXml(allocator: *Allocator, root: *xml.Element) !ParseResult {
    var arena = ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    var reg = registry.Registry{
        .decls = &[_]registry.Declaration{},
        .api_constants = &[_]registry.ApiConstant{},
        .tags = &[_]registry.Tag{},
    };

    reg.api_constants = try parseApiConstants(&arena.allocator, root);
    reg.tags = try parseTags(&arena.allocator, root);

    return ParseResult{
        .arena = arena,
        .registry = reg,
    };
}

fn parseApiConstants(allocator: *Allocator, root: *xml.Element) ![]registry.ApiConstant {
    var enums = blk: {
        var it = root.elements();
        while (it.next()) |child| {
            const name = child.getAttribute("name") orelse continue;
            if (mem.eql(u8, name, "API Constants")) {
                break :blk child;
            }
        }

        return error.InvalidRegistry;
    };

    var constants = try allocator.alloc(registry.ApiConstant, enums.children.count());
    errdefer allocator.free(constants);

    var i: usize = 0;
    var it = enums.findChildrenByTag("enum");
    while (it.next()) |constant| {
        const value = if (constant.getAttribute("value")) |expr|
                registry.ApiConstant.Value{.expr = expr}
            else if (constant.getAttribute("alias")) |alias|
                registry.ApiConstant.Value{.alias = alias}
            else
                return error.InvalidRegistry;

        constants[i] = .{
            .name = constant.getAttribute("name") orelse return error.InvalidRegistry,
            .value = value,
        };

        i += 1;
    }

    return allocator.shrink(constants, i);
}

fn parseTags(allocator: *Allocator, root: *xml.Element) ![]registry.Tag {
    var tags_elem = root.findChildByTag("tags") orelse return error.InvalidRegistry;
    var tags = try allocator.alloc(registry.Tag, tags_elem.children.count());
    errdefer allocator.free(tags);

    var i: usize = 0;
    var it = tags_elem.findChildrenByTag("tag");
    while (it.next()) |tag| {
        tags[i] = .{
            .name = tag.getAttribute("name") orelse return error.InvalidRegistry,
            .author = tag.getAttribute("author") orelse return error.InvalidRegistry,
        };

        i += 1;
    }

    return allocator.shrink(tags, i);
}
