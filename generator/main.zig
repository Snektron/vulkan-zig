const std = @import("std");
const xml = @import("xml.zig");
const parseXml = @import("registry/parse.zig").parseXml;
const reg = @import("registry.zig");
const vkgen = @import("generator.zig");
const Allocator = std.mem.Allocator;

fn renderType(type_info: reg.TypeInfo) void {
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

fn dumpRegistry(registry: reg.Registry) void {
    for (registry.tags) |tag| {
        std.debug.warn("tag: name = {}, author = {}\n", .{tag.name, tag.author});
    }

    for (registry.api_constants) |api_constant| {
        std.debug.warn("constant: name = {}, ", .{api_constant.name});
        switch (api_constant.value) {
            .expr => |expr| std.debug.warn("expr = {}\n", .{expr}),
            .alias => |alias| std.debug.warn("alias = {}\n", .{alias}),
        }
    }

    for (registry.decls) |decl| {
        std.debug.warn("decl: name = {}; ", .{decl.name});
        renderType(decl.decl_type);
        std.debug.warn("\n", .{});
    }
}

const ProfilingAllocator = struct {
    allocator: Allocator,
    parent_allocator: *Allocator,
    max_usage: usize,
    current_usage: usize,

    fn init(parent_allocator: *Allocator) ProfilingAllocator {
        return ProfilingAllocator{
            .allocator = Allocator{
                .allocFn = alloc,
                .resizeFn = resize,
            },
            .parent_allocator = parent_allocator,
            .max_usage = 0,
            .current_usage = 0,
        };
    }

    fn alloc(allocator: *Allocator, len: usize, ptr_align: u29, len_align: u29) ![]u8 {
        const self = @fieldParentPtr(ProfilingAllocator, "allocator", allocator);
        const buf = try self.parent_allocator.allocFn(self.parent_allocator, len, ptr_align, len_align);
        self.current_usage += buf.len;
        if (self.current_usage > self.max_usage) {
            self.max_usage = self.current_usage;
        }

        return buf;
    }

    fn resize(allocator: *Allocator, buf: []u8, new_len: usize, len_align: u29) !usize {
        const self = @fieldParentPtr(ProfilingAllocator, "allocator", allocator);
        const actual_new_len = try self.parent_allocator.resizeFn(self.parent_allocator, buf, new_len, len_align);
        self.current_usage = self.current_usage - buf.len + actual_new_len;
        if (self.current_usage > self.max_usage) {
            self.max_usage = self.current_usage;
        }

        return actual_new_len;
    }
};

pub fn main() !void {
    if (std.os.argv.len <= 1) {
        std.debug.warn("Usage: vulkan-zig-gen <path-to-vk.xml>\n", .{});
        return;
    }

    var prof_alloc = ProfilingAllocator.init(std.heap.page_allocator);
    const allocator = &prof_alloc.allocator;

    const file = try std.fs.cwd().openFileZ(std.os.argv[1], .{});
    defer file.close();

    const stdout = std.io.getStdOut().writer();
    try vkgen.generate(&prof_alloc.allocator, file.reader(), stdout, .{
        .feature_level = .{.major = 1, .minor = 0},
        .extensions = .all,
    });

    std.debug.warn("Total memory usage: {} KiB\n", .{@divTrunc(prof_alloc.max_usage, 1024)});
}

test "main" {
    _ = @import("xml.zig");
    _ = @import("registry/c-parse.zig");
}
