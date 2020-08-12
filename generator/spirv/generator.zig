const std = @import("std");
const reg = @import("registry.zig");
const Allocator = std.mem.Allocator;

pub fn generate(allocator: *Allocator, spec_jsons: []const []const u8, writer: anytype) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const registries = try arena.allocator.alloc(reg.Registry, spec_jsons.len);
    for (registries) |*registry, i| {
        var tokens = std.json.TokenStream.init(spec_jsons[i]);
        registry.* = try std.json.parse(reg.Registry, &tokens, .{.allocator = &arena.allocator});
    }
}
