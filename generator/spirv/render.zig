const std = @import("std");
const reg = @import("registry.zig");
const IdRenderer = @import("../id_render.zig").IdRenderer;
const Allocator = std.mem.Allocator;

// The SPIR-V spec doesn't contain any tag information like vulkan.xml does,
// so the tags are just hardcoded. They are retrieved from
// https://github.com/KhronosGroup/SPIRV-Registry/tree/master/extensions
const tags = [_][]const u8{
    "AMD",
    "EXT",
    "GOOGLE",
    "INTEL",
    "KHR",
    "NV",
};

fn Renderer(comptime WriterType: type) type {
    return struct {
        const Self = @This();

        writer: WriterType,
        allocator: *Allocator,
        registry: *const reg.CoreRegistry,
        id_renderer: IdRenderer,

        fn deinit(self: Self) void {

        }

        fn render(self: *Self) !void {

        }
    };
}

pub fn render(writer: anytype, allocator: *Allocator, registry: *const reg.CoreRegistry) !void {
    const id_renderer = IdRenderer.init(allocator, &tags);
    var renderer = Renderer(@TypeOf(writer)) {
        .writer = writer,
        .allocator = allocator,
        .registry = registry,
        .id_renderer = id_renderer,
    };
    defer renderer.deinit();
    try renderer.render();
}
