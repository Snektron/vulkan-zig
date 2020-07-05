const std = @import("std");
const reg = @import("registry.zig");
const xml = @import("../xml.zig");
const renderRegistry = @import("render.zig").render;
const parseXml = @import("parse.zig").parseXml;
const util = @import("util.zig");
const mem = std.mem;
const Allocator = mem.Allocator;
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

    gpa: *Allocator,
    reg_arena: *Allocator,
    registry: *reg.Registry,
    declarations: DeclarationSet,
    enum_extensions: EnumExtensionMap,
    field_set: FieldSet,

    fn init(gpa: *Allocator, reg_arena: *Allocator, registry: *reg.Registry) DeclarationResolver {
        return .{
            .gpa = gpa,
            .reg_arena = reg_arena,
            .registry = registry,
            .declarations = DeclarationSet.init(gpa),
            .enum_extensions = EnumExtensionMap.init(gpa),
            .field_set = FieldSet.init(gpa),
        };
    }

    fn deinit(self: *DeclarationResolver) void {
        var it = self.enum_extensions.iterator();
        while (it.next()) |entry| {
            entry.value.deinit();
        }

        self.field_set.deinit();
        self.enum_extensions.deinit();
        self.declarations.deinit();
    }

    fn putEnumExtension(self: *DeclarationResolver, enum_name: []const u8, field: reg.Enum.Field) !void {
        const res = try self.enum_extensions.getOrPut(enum_name);
        if (!res.found_existing) {
            res.entry.value = std.ArrayList(reg.Enum.Field).init(self.gpa);
        }

        try res.entry.value.append(field);
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

        self.field_set.clearRetainingCapacity();

        const n_fields_upper_bound = base_enum.fields.len + extensions.items.len;
        const new_fields = try self.reg_arena.alloc(reg.Enum.Field, n_fields_upper_bound);
        var i: usize = 0;

        for (base_enum.fields) |field| {
            const res = try self.field_set.getOrPut(field.name);
            if (!res.found_existing) {
                new_fields[i] = field;
                i += 1;
            }
        }

        // Assume that if a field name clobbers, the value is the same
        for (extensions.items) |field| {
            const res = try self.field_set.getOrPut(field.name);
            if (!res.found_existing) {
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
            if (decl.decl_type == .foreign or is_required) {
                self.registry.decls[write_index] = decl;
                write_index += 1;
            }

            read_index += 1;
        }

        self.registry.decls = self.reg_arena.shrink(self.registry.decls, write_index);
    }
};

const TagFixerUpper = struct {
    const NameInfo = struct {
        tagged_name: ?[]const u8,
        tagless_name_exists: bool,
    };

    gpa: *Allocator,
    registry: *reg.Registry,
    names: std.StringHashMap(NameInfo),

    fn init(gpa: *Allocator, registry: *reg.Registry) TagFixerUpper {
        return .{
            .gpa = gpa,
            .registry = registry,
            .names = std.StringHashMap(NameInfo).init(gpa),
        };
    }

    fn deinit(self: *TagFixerUpper) void {
        self.names.deinit();
    }

    fn insertName(self: *TagFixerUpper, name: []const u8) !void {
        const tagless = util.stripAuthorTag(name, self.registry.tags);
        const is_tagged = tagless.len != name.len;
        const result = try self.names.getOrPut(tagless);

        if (result.found_existing) {
            if (is_tagged) {
                result.entry.value.tagged_name = name;
            } else {
                result.entry.value.tagless_name_exists = true;
            }
        } else {
            result.entry.value = .{
                .tagged_name = if (is_tagged) name else null,
                .tagless_name_exists = !is_tagged,
            };
        }
    }

    fn extractNames(self: *TagFixerUpper) !void {
        for (self.registry.decls) |decl| {
            try self.insertName(decl.name);

            switch (decl.decl_type) {
                .enumeration => |enumeration| {
                    for (enumeration.fields) |field| {
                        try self.insertName(field.name);
                    }
                },
                else => {},
            }
        }
    }

    fn fixName(self: *TagFixerUpper, name: *[]const u8) !void {
        const tagless = util.stripAuthorTag(name.*, self.registry.tags);
        const info = self.names.get(tagless) orelse return error.InvalidRegistry;
        if (info.tagless_name_exists) {
            name.* = tagless;
        } else if (info.tagged_name) |tagged| {
            name.* = tagged;
        } else {
            return error.InvalidRegistry;
        }
    }

    fn fixCommand(self: *TagFixerUpper, command: *reg.Command) !void {
        for (command.params) |*param| {
            try self.fixTypeInfo(&param.param_type);
        }

        try self.fixTypeInfo(command.return_type);
        for (command.success_codes) |*code| {
            try self.fixName(code);
        }

        for (command.error_codes) |*code| {
            try self.fixName(code);
        }
    }

    fn fixTypeInfo(self: *TagFixerUpper, type_info: *reg.TypeInfo) error{InvalidRegistry}!void {
        switch (type_info.*) {
            .name => |*name| try self.fixName(name),
            .command_ptr => |*command| try self.fixCommand(command),
            .pointer => |ptr| try self.fixTypeInfo(ptr.child),
            .array => |arr| try self.fixTypeInfo(arr.child),
        }
    }

    fn fixNames(self: *TagFixerUpper) !void {
        for (self.registry.decls) |*decl| {
            switch (decl.decl_type) {
                .container => |*container| {
                    for (container.fields) |*field| {
                        try self.fixTypeInfo(&field.field_type);
                    }
                },
                .enumeration => |*enumeration| {
                    for (enumeration.fields) |*field| {
                        if (field.value == .alias) {
                            try self.fixName(&field.value.alias.name);
                        }
                    }
                },
                .bitmask => |*bitmask| {
                    if (bitmask.bits_enum) |*bits| {
                        try self.fixName(bits);
                    }
                },
                .command => |*command| try self.fixCommand(command),
                .alias => |*alias| try self.fixName(&alias.name),
                .typedef => |*type_info| try self.fixTypeInfo(type_info),
                else => {},
            }
        }
    }

    fn fixup(self: *TagFixerUpper) !void {
        try self.extractNames();
        try self.fixNames();
    }
};

pub const Generator = struct {
    gpa: *Allocator,
    reg_arena: std.heap.ArenaAllocator,
    registry: reg.Registry,

    fn init(allocator: *Allocator, spec: *xml.Element) !Generator {
        const result = try parseXml(allocator, spec);
        return Generator{
            .gpa = allocator,
            .reg_arena = result.arena,
            .registry = result.registry,
        };
    }

    fn deinit(self: Generator) void {
        self.reg_arena.deinit();
    }

    fn removePromotedExtensions(self: *Generator) void {
        var write_index: usize = 0;
        for (self.registry.extensions) |ext| {
            if (ext.promoted_to == .none) {
                self.registry.extensions[write_index] = ext;
                write_index += 1;
            }
        }
        self.registry.extensions.len = write_index;
    }

    // Solve `registry.declarations` according to `registry.extensions` and `registry.features`.
    fn resolveDeclarations(self: *Generator) !void {
        var resolver = DeclarationResolver.init(self.gpa, &self.reg_arena.allocator, &self.registry);
        defer resolver.deinit();
        try resolver.resolve();
    }

    fn fixupTags(self: *Generator) !void {
        var fixer_upper = TagFixerUpper.init(self.gpa, &self.registry);
        defer fixer_upper.deinit();
        try fixer_upper.fixup();
    }

    fn render(self: *Generator, out_stream: var) !void {
        try renderRegistry(out_stream, &self.reg_arena.allocator, &self.registry);
    }
};

pub fn generate(allocator: *Allocator, spec_xml: []const u8, writer: var) !void {
    const spec = try xml.parse(allocator, spec_xml);
    defer spec.deinit();

    var gen = try Generator.init(allocator, spec.root);
    defer gen.deinit();

    gen.removePromotedExtensions();
    try gen.resolveDeclarations();
    try gen.fixupTags();
    try gen.render(writer);
}
