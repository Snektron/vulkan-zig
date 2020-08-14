const std = @import("std");
const reg = @import("registry.zig");
const renderSpirv = @import("render.zig").render;
const Allocator = std.mem.Allocator;

pub fn mergeRegistries(allocator: *Allocator, core: reg.CoreRegistry, extensions: []const reg.ExtensionRegistry) !reg.CoreRegistry {
    var merged = core;

    var total_instructions = core.instructions.len;
    var total_operand_kinds = core.operand_kinds.len;
    for (extensions) |ext| {
        total_instructions += ext.instructions.len;
        total_operand_kinds += ext.operand_kinds.len;
    }

    merged.instructions = try allocator.alloc(reg.Instruction, total_instructions);
    merged.operand_kinds = try allocator.alloc(reg.OperandKind, total_operand_kinds);

    // Assume the core and extension registries contain no common operand kinds and instructions.
    for (core.instructions) |inst, i| merged.instructions[i] = inst;
    for (core.operand_kinds) |operand_kind, i| merged.operand_kinds[i] = operand_kind;

    var inst_i = core.instructions.len;
    var oper_i = core.operand_kinds.len;

    for (extensions) |ext| {
        for (ext.instructions) |inst| {
            merged.instructions[inst_i] = inst;
            inst_i += 1;
        }

        for (ext.operand_kinds) |operand_kind| {
            merged.operand_kinds[oper_i] = operand_kind;
            oper_i += 1;
        }
    }

    return merged;
}

pub fn generate(allocator: *Allocator, spec_jsons: []const []const u8, writer: anytype) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    // Exactly one of the passed specs must be core - the others must be exensions.
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

    const merged = try mergeRegistries(&arena.allocator, core_registry, ext_registries);
    try renderSpirv(writer, &merged);
}
