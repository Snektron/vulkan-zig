const std = @import("std");
const xml = @import("xml.zig");

pub fn main() !void {
    const file = try std.fs.cwd().openFileC(std.os.argv[1], .{});
    defer file.close();

    const size = try file.seekableStream().stream.getEndPos();
    const source = try std.heap.page_allocator.alloc(u8, size);
    defer std.heap.page_allocator.free(source);

    _ = try file.inStream().stream.read(source);

    var doc = try xml.parse(std.heap.page_allocator, source);
    defer doc.deinit();
}

test "main" {
    _ = @import("xml.zig");
}
