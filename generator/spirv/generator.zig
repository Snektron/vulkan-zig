const std = @import("std");
const reg = @import("registry.zig");
const Allocator = std.mem.Allocator;

pub fn generate(allocator: *Allocator, spec_jsons: []const []const u8, writer: anytype) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    // Only one of the passed specs may be core (and one of them _must_ be core) -
    // the others must be exensions.
    var core_registry: reg.CoreRegistry = undefined;
    const num_ext_registries = spec_jsons.len - 1;
    const ext_registries = try arena.allocator.alloc(reg.ExtensionRegistry, num_ext_registries);

    var ext_registry_i: usize = 0;
    for (spec_jsons) |spec_json| {
        var tokens = std.json.TokenStream.init(spec_json);
        const registry = try std.json.parse(reg.Registry, &tokens, .{.allocator = &arena.allocator});
        switch (registry) {
            .core => |parsed_core_registry| core_registry = parsed_core_registry,
            .extension => |parsed_ext_registry| {
                if (ext_registry_i == num_ext_registries) {
                    return error.NoCoreRegistry;
                }

                ext_registries[ext_registry_i] = parsed_ext_registry;
                ext_registry_i += 1;
            },
        }
    }

    if (ext_registry_i != num_ext_registries) {
        return error.MultipleCoreRegistries;
    }
}
