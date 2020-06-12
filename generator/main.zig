const std = @import("std");
const xml = @import("xml.zig");
const parseXml = @import("registry/parse.zig").parseXml;
const registry = @import("registry.zig");

fn renderType(type_info: registry.TypeInfo) void {
    switch (type_info) {
        .container => |c| {
            if (c.is_union) {
                std.debug.warn("union {{ ... }}", .{});
            } else {
                std.debug.warn("struct {{ ... }}", .{});

                for (c.fields) |field| {
                    std.debug.warn("{}: ", .{field.name});
                    renderType(field.field_type);
                    std.debug.warn(", ", .{});
                }

                std.debug.warn("}}", .{});
            }
        },
        .enumeration => |e| {
            if (e.is_bitmask) {
                std.debug.warn("bitmask {{ ... }}", .{});
            } else {
                std.debug.warn("enum {{ ... }}", .{});
            }
        },
        .bitmask => std.debug.warn("flags", .{}),
        .handle => |h| {
            if (h.is_dispatchable) {
                std.debug.warn("dispatchable handle", .{});
            } else {
                std.debug.warn("handle", .{});
            }
        },
        .command => |c| std.debug.warn("{}", .{c}),
        .array => |a| std.debug.warn("{}", .{a}),
        .pointer => |a| std.debug.warn("{}", .{a}),
        .alias => |alias| std.debug.warn("{}", .{alias}),
        .foreign => |f| std.debug.warn("foreign", .{}),
        .opaque => std.debug.warn("opaque", .{}),
        else => std.debug.warn("unhandled", .{}),
    }
}

fn dumpRegistry(reg: registry.Registry) void {
    for (reg.tags) |tag| {
        std.debug.warn("tag: name = {}, author = {}\n", .{tag.name, tag.author});
    }

    for (reg.api_constants) |api_constant| {
        std.debug.warn("constant: name = {}, ", .{api_constant.name});
        switch (api_constant.value) {
            .expr => |expr| std.debug.warn("expr = {}\n", .{expr}),
            .alias => |alias| std.debug.warn("alias = {}\n", .{alias}),
        }
    }

    for (reg.decls) |decl| {
        std.debug.warn("decl: name = {}; ", .{decl.name});
        renderType(decl.decl_type);
        std.debug.warn("\n", .{});
    }
}

pub fn main() !void {
    // if (std.os.argv.len <= 1) {
    //     std.debug.warn("Usage: vulkan-zig-gen <path-to-vk.xml>\n", .{});
    //     return;
    // }

    const file = try std.fs.cwd().openFileZ(std.os.argv[1], .{});
    defer file.close();

    const size = try file.seekableStream().getEndPos();
    const source = try std.heap.page_allocator.alloc(u8, size);
    defer std.heap.page_allocator.free(source);

    _ = try file.inStream().read(source);

    const spec = try xml.parse(std.heap.page_allocator, source);
    defer spec.deinit();

    const result = try parseXml(std.heap.page_allocator, spec.root);
    defer result.deinit();

    dumpRegistry(result.registry);
}

test "main" {
    _ = @import("xml.zig");
    _ = @import("registry/c-parse.zig");
}
