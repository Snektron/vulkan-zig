const std = @import("std");
const xml = @import("xml.zig");
const mem = std.mem;
const Allocator = mem.Allocator;
const SegmentedList = std.SegmentedList;

const Spec = struct {
    backing_allocator: *Allocator,
    enums: std.StringHashMap(Enum),
    bitmasks: std.StringHashMap(Bitmask),
    extensions: std.ArrayList(ExtensionInfo),

    fn deinit(self: Spec) void {
        self.enums.deinit();
    }

    fn dump(self: Spec) void {
        {
            std.debug.warn("Enums:\n", .{});
            var it = self.enums.iterator();
            while (it.next()) |e| {
                const kind_text = if (e.value.kind == .Bitmask) " (bitmask)" else "";
                std.debug.warn("    {}{}:\n", .{e.key, kind_text});

                for (e.value.fields.toSlice()) |field| {
                    std.debug.warn("        {}\n", .{field.name});
                }
            }
        }

        {
            std.debug.warn("Bitmasks:\n", .{});
            var it = self.bitmasks.iterator();
            while (it.next()) |b| {
                std.debug.warn("    {}", .{b.key});

                switch (b.value) {
                    .None => std.debug.warn("\n", .{}),
                    .Enum => |bits| std.debug.warn(" [bits: {}]\n", .{bits}),
                    .Alias => |alias| std.debug.warn(" [alias of: {}]\n", .{alias})
                }
            }
        }

        {
            std.debug.warn("Extensions:\n", .{});
            for (self.extensions.toSlice()) |ext| {
                std.debug.warn("    {}: {}, version {}\n", .{ext.number, ext.name, ext.version});
            }
        }
    }
};

const Bitmask = union(enum) {
    None,
    Enum: []const u8,
    Alias: []const u8
};

const ExtensionInfo = struct {
    name: []const u8,
    number: u32,
    version: u32
};

const Enum = struct {
    const Kind = enum {
        Bitmask,
        Enum,

        fn parse(str: []const u8) !Kind {
            if (mem.eql(u8, str, "bitmask")) {
                return .Bitmask;
            } else if (mem.eql(u8, str, "enum")) {
                return .Enum;
            } else {
                return error.InvalidEnumKind;
            }
        }
    };

    const Value = union(enum) {
        Bitpos: u5, //log2(u32.bit_count)
        Value: i32,
        Alias: []const u8,

        fn fromXml(elem: *xml.Element) !Value {
            if (elem.getAttribute("value")) |value_str| {
                if (mem.startsWith(u8, value_str, "0x")) {
                    return Value{.Value = try std.fmt.parseInt(i32, value_str[2..], 16)};
                } else {
                    return Value{.Value = try std.fmt.parseInt(i32, value_str, 10)};
                }
            } else if (elem.getAttribute("bitpos")) |bitpos_str| {
                return Value{.Bitpos = try std.fmt.parseInt(u5, bitpos_str, 10)};
            } else if (elem.getAttribute("alias")) |alias| {
                return Value{.Alias = alias};
            } else {
                return error.InvalidValueElement;
            }
        }
    };

    const Field = struct {
        name: []const u8,
        value: Value
    };

    const backwards_compat_note = "Backwards-compatible alias containing a typo";
    const deprecation_note = "Deprecated name for backwards compatibility";

    kind: Kind,
    fields: std.ArrayList(Field),

    fn init(backing_allocator: *Allocator, kind: Kind) Enum {
        return .{
            .kind = kind,
            .fields = std.ArrayList(Field).init(backing_allocator)
        };
    }

    fn fromXml(backing_allocator: *Allocator, enums: *xml.Element) Enum {
        const kind = Enum.Kind.parse(enums.getAttribute("type").?) catch unreachable;
        var e = Enum.init(backing_allocator, kind);
        var it = enums.findChildrenByTag("enum");
        while (it.next()) |field| {
            e.processFieldFromXml(field, null);
        }

        return e;
    }

    fn addField(self: *Enum, name: []const u8, value: Value) void {
        const ptr = self.fields.append(.{.name = name, .value = value}) catch unreachable;
    }

    fn processFieldFromXml(self: *Enum, field: *xml.Element, ext_nr: ?u32) void {
        if (Enum.isBackwardsCompatAlias(field)) return;
        const name = field.getAttribute("name").?;
        const value = blk: {
            if (field.getAttribute("value")) |value_str| {
                const value = if (mem.startsWith(u8, value_str, "0x"))
                        std.fmt.parseInt(i32, value_str[2..], 16) catch unreachable
                    else
                        std.fmt.parseInt(i32, value_str, 10) catch unreachable;

                break :blk Value{.Value = value};
            } else if (field.getAttribute("bitpos")) |bitpos_str| {
                break :blk Value{.Bitpos = std.fmt.parseInt(u5, bitpos_str, 10) catch unreachable};
            } else if (field.getAttribute("alias")) |alias| {
                break :blk Value{.Alias = alias};
            } else if (field.getAttribute("offset")) |offset_str| {
                const offset = std.fmt.parseInt(u32, offset_str, 10) catch unreachable;

                const actual_ext_nr = ext_nr orelse blk: {
                    const ext_nr_str = field.getAttribute("extnumber").?;
                    break :blk std.fmt.parseInt(u32, ext_nr_str, 10) catch unreachable;
                };

                const abs_value = Enum.extensionEnumValue(actual_ext_nr, offset);
                const value = if (field.getAttribute("dir")) |_| -@intCast(i32, abs_value) else @intCast(i32, abs_value);

                break :blk Value{.Value = value};
            } else {
                unreachable;
            }
        };

        self.addField(name, value);
    }

    fn isBackwardsCompatAlias(field: *xml.Element) bool {
        if (field.getAttribute("comment")) |comment| {
            return mem.eql(u8, comment, Enum.backwards_compat_note) or
                mem.eql(u8, comment, Enum.deprecation_note);
        }

        return false;
    }

    fn extensionEnumValue(ext_nr: u32, offset: u32) u32 {
        const extension_value_base = 1000000000;
        const extension_block = 1000;
        return extension_value_base + (ext_nr - 1) * extension_block + offset;
    }
};

pub fn generate(backing_allocator: *Allocator, registry: xml.Document) Spec {
    std.debug.assert(mem.eql(u8, registry.root.tag, "registry"));

    var spec = Spec{
        .backing_allocator = backing_allocator,
        .enums = std.StringHashMap(Enum).init(backing_allocator),
        .bitmasks = std.StringHashMap(Bitmask).init(backing_allocator),
        .extensions = std.ArrayList(ExtensionInfo).init(backing_allocator),
    };

    errdefer spec.deinit();

    processTypes(&spec, registry);
    processEnums(&spec, registry);
    processFeatures(&spec, registry);
    processExtensions(&spec, registry);

    return spec;
}

fn processTypes(spec: *Spec, registry: xml.Document) void {
    var types = registry.root.findChildByTag("types").?;
    var it = types.findChildrenByTag("type");
    while (it.next()) |ty| {
        const category = ty.getAttribute("category") orelse continue;
        if (mem.eql(u8, category, "bitmask")) {
            processBitmaskType(spec, ty);
        }
    }
}

fn processBitmaskType(spec: *Spec, ty: *xml.Element) void {
    if (ty.getAttribute("name")) |name| {
        const alias = ty.getAttribute("alias").?;
        if (spec.bitmasks.put(name, .{.Alias = alias}) catch unreachable) |_| unreachable;
    } else {
        const name = ty.findChildByTag("name").?.children.at(0).CharData;
        const bits: Bitmask = if (ty.getAttribute("requires")) |bits_name| .{.Enum = bits_name} else .None;
        if (spec.bitmasks.put(name, bits) catch unreachable) |_| unreachable;
    }
}

fn processEnums(spec: *Spec, registry: xml.Document) void {
    var it = registry.root.findChildrenByTag("enums");
    while (it.next()) |enums| {
        const name = enums.getAttribute("name").?;
        if (!mem.eql(u8, name, "API Constants")) {
            const e = Enum.fromXml(spec.backing_allocator, enums);
            if (spec.enums.put(name, e) catch unreachable) |_| unreachable;
        }
    }
}

fn processExtensions(spec: *Spec, registry: xml.Document) void {
    var extensions = registry.root.findChildByTag("extensions").?;
    var ext_it = extensions.findChildrenByTag("extension");
    while (ext_it.next()) |ext| {
        if (ext.getAttribute("supported")) |support| {
            if (mem.eql(u8, support, "disabled")) continue;
        }

        processExtension(spec, ext);
    }
}

fn processExtension(spec: *Spec, ext: *xml.Element) void {
    const ext_nr_str = ext.getAttribute("number").?;
    const ext_nr = std.fmt.parseInt(u32, ext_nr_str, 10) catch unreachable;

    var version: ?u32 = null;

    var req_it = ext.findChildrenByTag("require");
    while (req_it.next()) |req| {
        var it = req.findChildrenByTag("enum");
        while (it.next()) |field| {
            if (field.getAttribute("extends")) |enum_name| {
                // Some extensions define fields for other extensions,
                // these are also defined in those extensions, so just skip them
                if (field.getAttribute("extnumber")) |_| continue;

                const kv = spec.enums.get(enum_name).?;
                kv.value.processFieldFromXml(field, ext_nr);
            } else if (field.getAttribute("name")) |name| {
                if (mem.endsWith(u8, name, "_SPEC_VERSION")) {
                    const version_str = field.getAttribute("value").?;
                    version = std.fmt.parseInt(u32, version_str, 10) catch unreachable;
                }
            }
        }
    }

    std.debug.warn("{}\n", .{ext.getAttribute("name")});

    var ext_info = ExtensionInfo{
        .name = ext.getAttribute("name").?,
        .number = ext_nr,
        .version = version.?
    };

    spec.extensions.append(ext_info) catch unreachable;
}

fn processFeatures(spec: *Spec, registry: xml.Document) void {
    var feature_it = registry.root.findChildrenByTag("feature");
    while (feature_it.next()) |feature| {
        var req_it = feature.findChildrenByTag("require");
        while (req_it.next()) |req| {
            var enum_it = req.findChildrenByTag("enum");
            while (enum_it.next()) |field| {
                const enum_name = field.getAttribute("extends") orelse continue;
                const kv = spec.enums.get(enum_name).?;
                kv.value.processFieldFromXml(field, null);
            }
        }
    }
}
