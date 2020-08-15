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

const preamble =
    \\
    \\ This file is generated from the SPIR-V JSON registry
    ;

fn stripOpPrefix(name: []const u8) []const u8 {
    // Some instructions (those from the core) are prefixed with 'Op'
    const prefix = "Op";
    return if (std.mem.startsWith(u8, name, prefix))
            name[prefix.len ..]
        else
            name;
}

fn Renderer(comptime WriterType: type) type {
    return struct {
        const Self = @This();

        writer: WriterType,
        registry: *const reg.CoreRegistry,
        id_renderer: IdRenderer,

        fn render(self: *Self) !void {
            for (self.registry.copyright) |line| {
                try self.writer.print("// {}\n", .{ line });
            }
            try self.writer.writeAll(preamble);
            try self.renderOpcodes();
        }

        fn renderOpcodes(self: *Self) !void {
            try self.writer.writeAll("pub const Opcode = enum(u16) {\n");
            for (self.registry.instructions) |instr| {
                try self.id_renderer.renderWithCase(self.writer, .snake, stripOpPrefix(instr.opname));
                try self.writer.print(" = {},\n", .{ instr.opcode });
            }
            try self.writer.writeAll("};\n");
        }
    };
}

pub fn render(writer: anytype, allocator: *Allocator, registry: *const reg.CoreRegistry) !void {
    const id_renderer = IdRenderer.init(allocator, &tags);
    defer id_renderer.deinit();
    var renderer = Renderer(@TypeOf(writer)) {
        .writer = writer,
        .registry = registry,
        .id_renderer = id_renderer,
    };
    try renderer.render();
}

