const std = @import("std");
const registry = @import("registry-new.zig");
const xml = @import("xml.zig");
const mem = std.mem;
const Allocator = mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const api_constants_name = "API Constants";

pub const ParseResult = struct {
    arena: ArenaAllocator,
    registry: registry.Registry,

    pub fn deinit(self: ParseResult) void {
        self.arena.deinit();
    }
};

pub fn parseXml(backing_allocator: *Allocator, root: *xml.Element) !ParseResult {
    var arena = ArenaAllocator.init(backing_allocator);
    errdefer arena.deinit();

    const allocator = &arena.allocator;

    var reg = registry.Registry{
        .decls = try parseDeclarations(allocator, root),
        .api_constants = try parseApiConstants(allocator, root),
        .tags = try parseTags(allocator, root),
    };

    return ParseResult{
        .arena = arena,
        .registry = reg,
    };
}

fn parseDeclarations(allocator: *Allocator, root: *xml.Element) ![]registry.Declaration {
    var types_elem = root.findChildByTag("types") orelse return error.InvalidRegistry;
    var commands_elem = root.findChildByTag("commands") orelse return error.InvalidRegistry;

    const decl_upper_bound = types_elem.children.count() + commands_elem.children.count();
    const decls = try allocator.alloc(registry.Declaration, decl_upper_bound);
    errdefer allocator.free(decls);

    var count: usize = 0;
    count += try parseTypes(allocator, decls, types_elem);
    count += try parseEnums(allocator, decls[count..], root);
    count += try parseCommands(allocator, decls[count..], commands_elem);
    return allocator.shrink(decls, count);
}

fn parseTypes(allocator: *Allocator, out: []registry.Declaration, types_elem: *xml.Element) !usize {
    var i: usize = 0;
    var it = types_elem.findChildrenByTag("type");
    while (it.next()) |ty| {
        // TODO: Handle foreign types
        const category = ty.getAttribute("category") orelse continue;

        // Enums are handled later, in parseEnums. This also has the effect of filtering
        // out any enums which have no elements, and should be unused by other parts of the API.
        out[i] = if (mem.eql(u8, category, "bitmask"))
                try parseBitmaskType(ty)
            else if (mem.eql(u8, category, "handle"))
                try parseHandleType(ty)
            else
                continue;

        i += 1;
    }

    return i;
}

fn parseBitmaskType(ty: *xml.Element) !registry.Declaration {
    if (ty.getAttribute("name")) |name| {
        const alias = ty.getAttribute("alias") orelse return error.InvalidRegistry;
        return registry.Declaration{
            .name = name,
            .decl_type = .{.Alias = alias},
        };
    } else {
        return registry.Declaration{
            .name = ty.getCharData("name") orelse return error.InvalidRegistry,
            .decl_type = .{.Bitmask = .{.bits_enum = ty.getAttribute("requires")}},
        };
    }
}

fn parseHandleType(ty: *xml.Element) !registry.Declaration {
    // Parent is not handled in case of an alias
    if (ty.getAttribute("name")) |name| {
        const alias = ty.getAttribute("alias") orelse return error.InvalidRegistry;
        return registry.Declaration{
            .name = name,
            .decl_type = .{.Alias = alias},
        };
    } else {
        const name = ty.getCharData("name") orelse return error.InvalidRegistry;
        const handle_type = ty.getCharData("type") orelse return error.InvalidRegistry;
        const dispatchable = mem.eql(u8, handle_type, "VK_DEFINE_HANDLE");
        if (!dispatchable and !mem.eql(u8, handle_type, "VK_DEFINE_NON_DISPATCHABLE_HANDLE")) {
            return error.InvalidRegistry;
        }

        return registry.Declaration{
            .name = name,
            .decl_type = .{
                .Handle = .{
                    .parent = ty.getAttribute("parent"),
                    .is_dispatchable = dispatchable,
                }
            }
        };
    }
}

fn parseEnums(allocator: *Allocator, out: []registry.Declaration, root: *xml.Element) !usize {
    var i: usize = 0;
    var it = root.findChildrenByTag("enums");
    while (it.next()) |enums| {
        const name = enums.getAttribute("name") orelse return error.InvalidRegistry;
        if (mem.eql(u8, name, api_constants_name)) {
            continue;
        }

        out[i] = .{
            .name = name,
            .decl_type = .{.Enum = try parseEnumFields(allocator, enums)},
        };
        i += 1;
    }

    return i;
}

fn parseEnumFields(allocator: *Allocator, elem: *xml.Element) !registry.Enum {
    // TODO: `type` was added recently, fall back to checking endswith FlagBits for older versions?
    const enum_type = elem.getAttribute("type") orelse return error.InvalidRegistry;
    const is_bitmask = mem.eql(u8, enum_type, "bitmask");
    if (!is_bitmask and !mem.eql(u8, enum_type, "enum")) {
        return error.InvalidRegistry;
    }

    const fields = try allocator.alloc(registry.Enum.Field, elem.children.count());
    errdefer allocator.free(fields);

    var i: usize = 0;
    var it = elem.findChildrenByTag("enum");
    while (it.next()) |field| {
        fields[i] = try parseEnumField(field);
        i += 1;
    }

    return registry.Enum{
        .fields = allocator.shrink(fields, i),
        .is_bitmask = is_bitmask,
    };
}

fn parseEnumField(field: *xml.Element) !registry.Enum.Field {
    const is_compat_alias = if (field.getAttribute("comment")) |comment|
        mem.eql(u8, comment, "Backwards-compatible alias containing a typo") or
            mem.eql(u8, comment, "Deprecated name for backwards compatibility")
    else
        false;

    const name = field.getAttribute("name") orelse return error.InvalidRegistry;
    const value: registry.Enum.Value = blk: {
        // An enum variant's value could be defined by any of the following attributes:
        // - value: Straight up value of the enum variant, in either base 10 or 16 (prefixed with 0x).
        // - bitpos: Used for bitmasks, and can also be set in extensions.
        // - alias: The field is an alias of another variant within the same enum.
        // - offset: Used with features and extensions, where a non-bitpos value is added to an enum.
        //     The value is given by `1e9 + (extr_nr - 1) * 1e3 + offset`, where `ext_nr` is either
        //     given by the `extnumber` field (in the case of a feature), or given in the parent <extension>
        //     tag. In the latter case its passed via the `ext_nr` parameter.
        // TODO: Handle `offset` elsewhere
        if (field.getAttribute("value")) |value| {
            if (mem.startsWith(u8, value, "0x")) {
                break :blk .{.bit_vector = try std.fmt.parseInt(i32, value[2..], 16)};
            } else {
                break :blk .{.int = try std.fmt.parseInt(i32, value, 10)};
            }
        } else if (field.getAttribute("bitpos")) |bitpos| {
            break :blk .{.bitpos = try std.fmt.parseInt(u5, bitpos, 10)};
        } else if (field.getAttribute("alias")) |alias| {
            break :blk .{.alias = .{.alias_name = alias, .is_compat_alias = is_compat_alias}};
        } else {
            return error.InvalidRegistry;
        }
    };

    return registry.Enum.Field{
        .name = name,
        .value = value,
    };
}

fn parseCommands(allocator: *Allocator, out: []registry.Declaration, commmands_elem: *xml.Element) !usize {
    return 0;
}

fn parseApiConstants(allocator: *Allocator, root: *xml.Element) ![]registry.ApiConstant {
    var enums = blk: {
        var it = root.findChildrenByTag("enums");
        while (it.next()) |child| {
            const name = child.getAttribute("name") orelse continue;
            if (mem.eql(u8, name, api_constants_name)) {
                break :blk child;
            }
        }

        return error.InvalidRegistry;
    };

    const constants = try allocator.alloc(registry.ApiConstant, enums.children.count());
    errdefer allocator.free(constants);

    var i: usize = 0;
    var it = enums.findChildrenByTag("enum");
    while (it.next()) |constant| {
        const value = if (constant.getAttribute("value")) |expr|
                registry.ApiConstant.Value{.expr = expr}
            else if (constant.getAttribute("alias")) |alias|
                registry.ApiConstant.Value{.alias = alias}
            else
                return error.InvalidRegistry;

        constants[i] = .{
            .name = constant.getAttribute("name") orelse return error.InvalidRegistry,
            .value = value,
        };

        i += 1;
    }

    return allocator.shrink(constants, i);
}

fn parseTags(allocator: *Allocator, root: *xml.Element) ![]registry.Tag {
    var tags_elem = root.findChildByTag("tags") orelse return error.InvalidRegistry;
    const tags = try allocator.alloc(registry.Tag, tags_elem.children.count());
    errdefer allocator.free(tags);

    var i: usize = 0;
    var it = tags_elem.findChildrenByTag("tag");
    while (it.next()) |tag| {
        tags[i] = .{
            .name = tag.getAttribute("name") orelse return error.InvalidRegistry,
            .author = tag.getAttribute("author") orelse return error.InvalidRegistry,
        };

        i += 1;
    }

    return allocator.shrink(tags, i);
}
