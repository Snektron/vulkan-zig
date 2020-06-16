const std = @import("std");
const reg = @import("registry.zig");
const xml = @import("xml.zig");
const renderRegistry = @import("render.zig").render;
const parseXml = @import("registry/parse.zig").parseXml;
const Allocator = std.mem.Allocator;
const FeatureLevel = reg.FeatureLevel;

fn cmpFeatureLevels(a: FeatureLevel, b: FeatureLevel) std.math.Order {
    if (a.major > b.major) {
        return .gt;
    } if (a.major < b.major) {
        return .lt;
    }

    if (a.minor > b.minor) {
        return .gt;
    } else if (a.minor < b.minor) {
        return .lt;
    }

    return .eq;
}

const DeclarationResolver = struct {
    const DeclarationSet = std.StringHashMap(void);
    const EnumExtensionMap = std.StringHashMap(std.ArrayList(reg.Enum.Field));
    const FieldSet = std.StringHashMap(void);

    allocator: *Allocator,
    reg_arena: *Allocator,
    registry: *reg.Registry,
    declarations: DeclarationSet,
    enum_extensions: EnumExtensionMap,
    field_set: FieldSet,

    fn init(allocator: *Allocator, reg_arena: *Allocator, registry: *reg.Registry) DeclarationResolver {
        return .{
            .allocator = allocator,
            .reg_arena = reg_arena,
            .registry = registry,
            .declarations = DeclarationSet.init(allocator),
            .enum_extensions = EnumExtensionMap.init(allocator),
            .field_set = FieldSet.init(allocator),
        };
    }

    fn deinit(self: DeclarationResolver) void {
        var it = self.enum_extensions.iterator();
        while (it.next()) |kv| {
            kv.value.deinit();
        }

        self.field_set.deinit();
        self.enum_extensions.deinit();
        self.declarations.deinit();
    }

    fn putEnumExtension(self: *DeclarationResolver, enum_name: []const u8, field: reg.Enum.Field) !void {
        const res = try self.enum_extensions.getOrPut(enum_name);
        if (!res.found_existing) {
            res.kv.value = std.ArrayList(reg.Enum.Field).init(self.allocator);
        }

        try res.kv.value.append(field);
    }

    fn addRequire(self: *DeclarationResolver, req: reg.Require) !void {
        for (req.types) |type_name| {
            _ = try self.declarations.put(type_name, {});
        }

        for (req.commands) |command| {
            _ = try self.declarations.put(command, {});
        }

        for (req.extends) |enum_ext| {
            try self.putEnumExtension(enum_ext.extends, enum_ext.field);
        }
    }

    fn mergeEnumFields(self: *DeclarationResolver, name: []const u8, base_enum: *reg.Enum) !void {
        // If there are no extensions for this enum, assume its valid.
        const extensions = self.enum_extensions.get(name) orelse return;

        self.field_set.clear();

        const n_fields_upper_bound = base_enum.fields.len + extensions.value.items.len;
        const new_fields = try self.reg_arena.alloc(reg.Enum.Field, n_fields_upper_bound);
        var i: usize = 0;

        for (base_enum.fields) |field| {
            const existing = try self.field_set.put(field.name, {});
            if (existing == null) {
                new_fields[i] = field;
                i += 1;
            }
        }

        // Assume that if a field name clobbers, the value is the same
        for (extensions.value.items) |field| {
            const existing = try self.field_set.put(field.name, {});
            if (existing == null) {
                new_fields[i] = field;
                i += 1;
            }
        }

        // Existing base_enum.fields was allocatued by `self.reg_arena`, so
        // it gets cleaned up whenever that is deinited.
        base_enum.fields = self.reg_arena.shrink(new_fields, i);
    }

    fn resolve(self: *DeclarationResolver) !void {
        for (self.registry.features) |feature| {
            for (feature.requires) |req| {
                try self.addRequire(req);
            }
        }

        for (self.registry.extensions) |ext| {
            for (ext.requires) |req| {
                try self.addRequire(req);
            }
        }

        // Merge all the enum fields.
        // Assume that all keys of enum_extensions appear in `self.registry.decls`
        for (self.registry.decls) |*decl| {
            if (decl.decl_type == .enumeration) {
                try self.mergeEnumFields(decl.name, &decl.decl_type.enumeration);
            }
        }

        // Remove all declarations that are not required.
        //  Some declarations may exist in `self.declarations` that do not exit in
        // `self.registry.decls`, these are mostly macros and other stuff not pa
        var read_index: usize = 0;
        var write_index: usize = 0;
        while (read_index < self.registry.decls.len) {
            const decl = self.registry.decls[read_index];
            const is_required = self.declarations.contains(decl.name);
            const is_empty_enum = decl.decl_type == .enumeration and decl.decl_type.enumeration.fields.len == 0;
            if (decl.decl_type == .foreign or (is_required and !is_empty_enum)) {
                self.registry.decls[write_index] = decl;
                write_index += 1;
            }

            read_index += 1;
        }

        self.registry.decls = self.reg_arena.shrink(self.registry.decls, write_index);
    }
};

pub const Generator = struct {
    gpa: *Allocator,
    registry_arena: std.heap.ArenaAllocator,
    registry: reg.Registry,

    pub fn init(allocator: *Allocator, spec: *xml.Element) !Generator {
        const result = try parseXml(allocator, spec);
        return Generator{
            .gpa = allocator,
            .registry_arena = result.arena,
            .registry = result.registry,
        };
    }

    pub fn deinit(self: Generator) void {
        self.registry_arena.deinit();
    }

    // Solve `registry.declarations` according to `registry.extensions` and `registry.features`.
    pub fn resolveDeclarations(self: *Generator) !void {
        var resolver = DeclarationResolver.init(self.gpa, &self.registry_arena.allocator, &self.registry);
        defer resolver.deinit();
        try resolver.resolve();
    }

    pub fn render(self: *Generator, out_stream: var) !void {
        try renderRegistry(out_stream, self.gpa, &self.registry);
    }
};
