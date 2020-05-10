const std = @import("std");
const xml = @import("xml.zig");
const Registry = @import("registry.zig").Registry;
const vk_render = @import("render.zig").render;

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

    const registry = Registry.fromXml(std.heap.page_allocator, spec.root);
    defer registry.deinit();
    // registry.dump();

    const stdout_file = std.io.getStdOut();
    var stdout = stdout_file.outStream();
    try vk_render(stdout, registry);
}

test "main" {
    _ = @import("xml.zig");
}
