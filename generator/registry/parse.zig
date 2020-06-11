const std = @import("std");
const registry = @import("../registry.zig");
const xml = @import("../xml.zig");
const xmlc = @import("c-parse.zig");
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
        .features = try parseFeatures(allocator, root),
        .extensions = try parseExtensions(allocator, root),
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
        out[i] = blk: {
            const category = ty.getAttribute("category") orelse {
                break :blk try parseForeigntype(ty);
            };

            // Enums are handled later, in parseEnums. This also has the effect of filtering
            // out any enums which have no elements, and should be unused by other parts of the API.
            if (mem.eql(u8, category, "bitmask")) {
                break :blk try parseBitmaskType(ty);
            } else if (mem.eql(u8, category, "handle")) {
                break :blk try parseHandleType(ty);
            } else if (mem.eql(u8, category, "basetype")) {
                break :blk try parseBaseType(allocator, ty);
            } else if (mem.eql(u8, category, "struct")) {
                break :blk try parseContainer(allocator, ty, false);
            } else if (mem.eql(u8, category, "union")) {
                break :blk try parseContainer(allocator, ty, true);
            } else if (mem.eql(u8, category, "funcpointer")) {
                break :blk try parseFuncPointer(allocator, ty);
            }

            continue;
        };

        i += 1;
    }

    return i;
}

fn parseForeigntype(ty: *xml.Element) !registry.Declaration {
    const name = ty.getAttribute("name") orelse return error.InvalidRegistry;
    const depends = ty.getAttribute("requires") orelse if (mem.eql(u8, name, "int"))
            "vk_platform"  // for some reason, int doesn't depend on vk_platform (but the other c types do)
        else
            return error.InvalidRegistry;

    return registry.Declaration{
        .name = name,
        .decl_type = .{.foreign = .{.depends = depends}},
    };
}

fn parseBitmaskType(ty: *xml.Element) !registry.Declaration {
    if (ty.getAttribute("name")) |name| {
        const alias = ty.getAttribute("alias") orelse return error.InvalidRegistry;
        return registry.Declaration{
            .name = name,
            .decl_type = .{.alias = alias},
        };
    } else {
        return registry.Declaration{
            .name = ty.getCharData("name") orelse return error.InvalidRegistry,
            .decl_type = .{.bitmask = .{.bits_enum = ty.getAttribute("requires")}},
        };
    }
}

fn parseHandleType(ty: *xml.Element) !registry.Declaration {
    // Parent is not handled in case of an alias
    if (ty.getAttribute("name")) |name| {
        const alias = ty.getAttribute("alias") orelse return error.InvalidRegistry;
        return registry.Declaration{
            .name = name,
            .decl_type = .{.alias = alias},
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
                .handle = .{
                    .parent = ty.getAttribute("parent"),
                    .is_dispatchable = dispatchable,
                }
            },
        };
    }
}

fn parseBaseType(allocator: *Allocator, ty: *xml.Element) !registry.Declaration {
    const name = ty.getCharData("name") orelse return error.InvalidRegistry;
    if (ty.getCharData("type")) |_| { // TODO: Parse as full type?
        var tok = xmlc.XmlCTokenizer.init(ty);
        return try xmlc.parseTypedef(allocator, &tok);
    } else {
        // Either ANativeWindow, AHardwareBuffer or CAMetalLayer. The latter has a lot of
        // macros, which is why this part is not built into the xml/c parser.
        return registry.Declaration{
            .name = name,
            .decl_type = .{.opaque = {}},
        };
    }
}

fn parseContainer(allocator: *Allocator, ty: *xml.Element, is_union: bool) !registry.Declaration {
    const name = ty.getAttribute("name") orelse return error.InvalidRegistry;

    if (ty.getAttribute("alias")) |alias| {
        return registry.Declaration{
            .name = name,
            .decl_type = .{.alias = alias},
        };
    }

    var members = try allocator.alloc(registry.Container.Field, ty.children.count());

    var i: usize = 0;
    var it = ty.findChildrenByTag("member");
    while (it.next()) |member| {
        var xctok = xmlc.XmlCTokenizer.init(member);
        members[i] = try xmlc.parseMember(allocator, &xctok);
        try parsePointerMeta(&members[i].field_type, member);
        i += 1;
    }

    return registry.Declaration{
        .name = name,
        .decl_type = .{
            .container = .{
                .fields = allocator.shrink(members, i),
                .is_union = is_union,
            }
        },
    };
}

fn parseFuncPointer(allocator: *Allocator, ty: *xml.Element) !registry.Declaration {
    var xctok = xmlc.XmlCTokenizer.init(ty);
    return try xmlc.parseTypedef(allocator, &xctok);
}

fn lenToPointerSize(len: []const u8) registry.Pointer.PointerSize {
    if (mem.eql(u8, len, "null-terminated")) {
        return .zero_terminated;
    } else {
        return .many;
    }
}

fn parsePointerMeta(type_info: *registry.TypeInfo, elem: *xml.Element) !void {
    if (elem.getAttribute("len")) |lens| {
        var it = mem.split(lens, ",");
        var current_type_info = type_info;
        while (current_type_info.* == .pointer) {
            const size = if (it.next()) |len_str| lenToPointerSize(len_str) else .one;
            current_type_info.pointer.size = size;
            current_type_info = current_type_info.pointer.child;
        }

        if (it.next()) |_| {
            // There are more elements in the `len` attribute than there are pointers
            // Something probably went wrong
            return error.InvalidRegistry;
        }
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
            .decl_type = .{.enumeration = try parseEnumFields(allocator, enums)},
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

fn parseCommands(allocator: *Allocator, out: []registry.Declaration, commands_elem: *xml.Element) !usize {
    var i: usize = 0;
    var it = commands_elem.findChildrenByTag("command");
    while (it.next()) |elem| {
        out[i] = try parseCommand(allocator, elem);
        i += 1;
    }

    return i;
}

fn splitCommaAlloc(allocator: *Allocator, text: []const u8) ![]const []const u8 {
    var n_codes: usize = 1;
    for (text) |c| {
        if (c == ',') n_codes += 1;
    }

    const codes = try allocator.alloc([]const u8, n_codes);
    var it = mem.split(text, ",");
    for (codes) |*code| {
        code.* = it.next().?;
    }

    return codes;
}

fn parseCommand(allocator: *Allocator, elem: *xml.Element) !registry.Declaration {
    if (elem.getAttribute("alias")) |alias| {
        const name = elem.getAttribute("name") orelse return error.InvalidRegistry;
        return registry.Declaration{
            .name = name,
            .decl_type = .{.alias = alias}
        };
    }

    const proto = elem.findChildByTag("proto") orelse return error.InvalidRegistry;
    var proto_xctok = xmlc.XmlCTokenizer.init(proto);
    const command_decl = try xmlc.parseParamOrProto(allocator, &proto_xctok);

    var params = try allocator.alloc(registry.Command.Param, elem.children.count());

    var i: usize = 0;
    var it = elem.findChildrenByTag("param");
    while (it.next()) |param| {
        var xctok = xmlc.XmlCTokenizer.init(param);
        const decl = try xmlc.parseParamOrProto(allocator, &xctok);
        params[i] = .{.name = decl.name, .param_type = decl.decl_type};
        try parsePointerMeta(&params[i].param_type, param);
        i += 1;
    }

    const return_type = try allocator.create(registry.TypeInfo);
    return_type.* = command_decl.decl_type;

    const success_codes = if (elem.getAttribute("successcodes")) |codes|
            try splitCommaAlloc(allocator, codes)
        else
            &[_][]const u8{};

    const error_codes = if (elem.getAttribute("errorcodes")) |codes|
            try splitCommaAlloc(allocator, codes)
        else
            &[_][]const u8{};

    return registry.Declaration{
        .name = command_decl.name,
        .decl_type = .{
            .command = .{
                .params = allocator.shrink(params, i),
                .return_type = return_type,
                .success_codes = success_codes,
                .error_codes = error_codes,
            }
        }
    };
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

fn parseFeatures(allocator: *Allocator, root: *xml.Element) ![]registry.Feature {
    var it = root.findChildrenByTag("feature");
    var count: usize = 0;
    while (it.next()) |_| count += 1;

    const features = try allocator.alloc(registry.Feature, count);
    var i: usize = 0;
    it = root.findChildrenByTag("feature");
    while (it.next()) |feature| {
        features[i] = try parseFeature(allocator, feature);
        i += 1;
    }

    return features;
}

fn parseFeature(allocator: *Allocator, feature: *xml.Element) !registry.Feature {
    const name = feature.getAttribute("name") orelse return error.InvalidRegistry;
    const number = feature.getAttribute("number") orelse return error.InvalidRegistry;

    var requires = try allocator.alloc(registry.Require, feature.children.count());
    var i: usize = 0;
    var it = feature.findChildrenByTag("require");
    while (it.next()) |require| {
        requires[i] = try parseRequire(allocator, require, null);
        i += 1;
    }

    return registry.Feature{
        .name = name,
        .number = number,
        .requires = allocator.shrink(requires, i)
    };
}

fn parseEnumExtension(elem: *xml.Element, parent_extnumber: ?u31) !?registry.Require.EnumExtension {
    // check for either _SPEC_VERSION or _EXTENSION_NAME
    const extends = elem.getAttribute("extends") orelse return null;

    if (elem.getAttribute("offset")) |offset_str| {
        const offset = try std.fmt.parseInt(u31, offset_str, 10);
        const name = elem.getAttribute("name") orelse return error.InvalidRegistry;
        const extnumber = if (elem.getAttribute("extnumber")) |num|
            try std.fmt.parseInt(u31, num, 10)
        else
            null;

        const actual_extnumber = extnumber orelse parent_extnumber orelse return error.InvalidRegistry;
        const value = blk: {
            const abs_value = enumExtOffsetToValue(actual_extnumber, offset);
            if (elem.getAttribute("dir")) |dir| {
                if (mem.eql(u8, dir, "-")) {
                    break :blk -@as(i32, abs_value);
                } else {
                    return error.InvalidRegistry;
                }
            }

            break :blk @as(i32, abs_value);
        };

        return registry.Require.EnumExtension{
            .extends = extends,
            .extnumber = actual_extnumber,
            .field = .{.name = name, .value = .{.int = value}},
        };
    }

    return registry.Require.EnumExtension{
        .extends = extends,
        .extnumber = parent_extnumber,
        .field = try parseEnumField(elem),
    };
}

fn enumExtOffsetToValue(extnumber: u31, offset: u31) u31 {
    const extension_value_base = 1000000000;
    const extension_block = 1000;
    return extension_value_base + (extnumber - 1) * extension_block + offset;
}

fn parseRequire(allocator: *Allocator, require: *xml.Element, extnumber: ?u31) !registry.Require {
    var n_extends: usize = 0;
    var n_types: usize = 0;
    var n_commands: usize = 0;

    var it = require.elements();
    while (it.next()) |elem| {
        if (mem.eql(u8, elem.tag, "enum")) {
            n_extends += 1;
        } else if (mem.eql(u8, elem.tag, "type")) {
            n_types += 1;
        } else if (mem.eql(u8, elem.tag, "command")) {
            n_commands += 1;
        }
    }

    const extends = try allocator.alloc(registry.Require.EnumExtension, n_extends);
    const types = try allocator.alloc([]const u8, n_types);
    const commands = try allocator.alloc([]const u8, n_commands);

    var i_extends: usize = 0;
    var i_types: usize = 0;
    var i_commands: usize = 0;

    it = require.elements();
    while (it.next()) |elem| {
        if (mem.eql(u8, elem.tag, "enum")) {
            if (try parseEnumExtension(elem, extnumber)) |ext| {
                extends[i_extends] = ext;
                i_extends += 1;
            }
        } else if (mem.eql(u8, elem.tag, "type")) {
            types[i_types] = elem.getAttribute("name") orelse return error.InvalidRegistry;
            i_types += 1;
        } else if (mem.eql(u8, elem.tag, "command")) {
            commands[i_commands] = elem.getAttribute("name") orelse return error.InvalidRegistry;
            i_commands += 1;
        }
    }

    return registry.Require{
        .extends = extends,
        .types = types,
        .commands = commands,
        .required_feature = require.getAttribute("feature"),
        .required_extension = require.getAttribute("extension"),
    };
}

fn parseExtensions(allocator: *Allocator, root: *xml.Element) ![]registry.Extension {
    const extensions_elem = root.findChildByTag("extensions") orelse return error.InvalidRegistry;

    const extensions = try allocator.alloc(registry.Extension, extensions_elem.children.count());
    var i: usize = 0;
    var it = extensions_elem.findChildrenByTag("extension");
    while (it.next()) |extension| {
        if (try parseExtension(allocator, extension)) |ext| {
            extensions[i] = ext;
            i += 1;
        }
    }

    return allocator.shrink(extensions, i);
}

fn findExtVersion(extension: *xml.Element) !u32 {
    var req_it = extension.findChildrenByTag("require");
    while (req_it.next()) |req| {
        var enum_it = req.findChildrenByTag("enum");
        while (enum_it.next()) |e| {
            const name = e.getAttribute("name") orelse continue;
            const value = e.getAttribute("value") orelse continue;
            if (mem.endsWith(u8, name, "_SPEC_VERSION")) {
                return try std.fmt.parseInt(u32, value, 10);
            }
        }
    }

    return error.InvalidRegistry;
}

fn parseExtension(allocator: *Allocator, extension: *xml.Element) !?registry.Extension {
    // Some extensions (in particular 94) are disabled, so just skip them
    if (extension.getAttribute("supported")) |supported| {
        if (mem.eql(u8, supported, "disabled")) return null;
    }

    const name = extension.getAttribute("name") orelse return error.InvalidRegistry;
    const platform = extension.getAttribute("platform");
    const promoted_to = extension.getAttribute("promotedto");
    const version = try findExtVersion(extension);

    const number = blk: {
        const number_str = extension.getAttribute("number") orelse return error.InvalidRegistry;
        break :blk try std.fmt.parseInt(u31, number_str, 10);
    };

    const ext_type: ?registry.Extension.ExtensionType = blk: {
        const ext_type_str = extension.getAttribute("type") orelse break :blk null;
        if (mem.eql(u8, ext_type_str, "instance")) {
            break :blk .instance;
        } else if (mem.eql(u8, ext_type_str, "device")) {
            break :blk .device;
        } else {
            return error.InvalidRegistry;
        }
    };

    const depends = blk: {
        const requires_str = extension.getAttribute("requires") orelse break :blk &[_][]const u8{};
        break :blk try splitCommaAlloc(allocator, requires_str);
    };

    var requires = try allocator.alloc(registry.Require, extension.children.count());
    var i: usize = 0;
    var it = extension.findChildrenByTag("require");
    while (it.next()) |require| {
        requires[i] = try parseRequire(allocator, require, number);
        i += 1;
    }

    return registry.Extension{
        .name = name,
        .number = number,
        .version = version,
        .extension_type = ext_type,
        .depends = depends,
        .promoted_to = promoted_to,
        .platform = platform,
        .requires = allocator.shrink(requires, i)
    };
}
