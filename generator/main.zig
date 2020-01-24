const std = @import("std");
const xml = @import("xml.zig");
const Registry = @import("registry.zig").Registry;

pub fn main() !void {
    if (std.os.argv.len <= 1) {
        std.debug.warn("Usage: vulkan-zig-gen <path-to-vk.xml>\n", .{});
        return;
    }

    const file = try std.fs.cwd().openFileC(std.os.argv[1], .{});
    defer file.close();

    const size = try file.seekableStream().stream.getEndPos();
    const source = try std.heap.page_allocator.alloc(u8, size);
    defer std.heap.page_allocator.free(source);

    _ = try file.inStream().stream.read(source);

    const spec = try xml.parse(std.heap.page_allocator, source);
    defer spec.deinit();

    const registry = Registry.fromXml(std.heap.page_allocator, spec.root);
    defer registry.deinit();

    registry.dump();
}

test "main" {
    _ = @import("xml.zig");
}
