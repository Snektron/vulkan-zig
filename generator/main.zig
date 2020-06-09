const std = @import("std");
const xml = @import("xml.zig");
const parseXml = @import("spec-parse.zig").parseXml;
const registry = @import("registry-new.zig");

pub fn dumpRegistry(reg: registry.Registry) void {
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
}

pub fn main() !void {
    if (std.os.argv.len <= 1) {
        std.debug.warn("Usage: vulkan-zig-gen <path-to-vk.xml>\n", .{});
        return;
    }

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
}
