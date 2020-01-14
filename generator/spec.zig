const std = @import("std");
const xml = @import("xml.zig");
const mem = std.mem;
const Allocator = mem.Allocator;
const SegmentedList = std.SegmentedList;

const Spec = struct {
    backing_allocator: *Allocator,
    enums: std.StringHashMap(Enum),

    fn deinit(self: Spec) void {
        self.enums.deinit();
    }

    fn dump(self: Spec) void {
        {
            std.debug.warn("enums:\n", .{});
            var it = self.enums.iterator();
            while (it.next()) |e| {
                const kind_text = if (e.value.kind == .Bitmask) " (bitmask)" else "";
                std.debug.warn("    {}{}:\n", .{e.key, kind_text});

                for (e.value.fields.toSlice()) |field| {
                    std.debug.warn("        {}\n", .{field.name});
                }
            }
        }
    }
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

    fn processFieldFromXml(self: *Enum, field: *xml.Element, ext_nr: ?i32) void {
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
                const offset = std.fmt.parseInt(i32, offset_str, 10) catch unreachable;

                const actual_ext_nr = ext_nr orelse blk: {
                    const ext_nr_str = field.getAttribute("extnumber").?;
                    break :blk std.fmt.parseInt(i32, ext_nr_str, 10) catch unreachable;
                };

                var value = Enum.extensionEnumValue(actual_ext_nr, offset);

                if (field.getAttribute("dir")) |_| {
                    // Special case for VkResult
                    value = -value;
                }

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

    fn extensionEnumValue(ext_nr: i32, offset: i32) i32 {
        const extension_value_base = 1000000000;
        const extension_block = 1000;
        return extension_value_base + (ext_nr - 1) * extension_block + offset;
    }
};

pub fn generate(backing_allocator: *Allocator, registry: xml.Document) Spec {
    std.debug.assert(mem.eql(u8, registry.root.tag, "registry"));

    var spec = Spec{
        .backing_allocator = backing_allocator,
        .enums = std.StringHashMap(Enum).init(backing_allocator)
    };

    errdefer spec.deinit();

    processEnums(&spec, registry);
    processFeatures(&spec, registry);
    processExtensions(&spec, registry);

    return spec;
}

fn readChildTextOrFail(element: *xml.Element, child_name: []const u8) ![]const u8 {
    const child = element.findChildByTag(child_name).?;
    const content = child.children.at(0).*;
    if (content != .CharData) {
        return error.InvalidRegistry;
    }

    return content.CharData;
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
        var req_it = ext.findChildrenByTag("require");
        while (req_it.next()) |req| {
            processExtension(spec, ext, req);
        }
    }
}

fn processExtension(spec: *Spec, ext: *xml.Element, req: *xml.Element) void {
    const ext_nr_str = ext.getAttribute("number").?;
    const ext_nr = std.fmt.parseInt(i32, ext_nr_str, 10) catch unreachable;

    var it = req.findChildrenByTag("enum");
    while (it.next()) |field| {
        const enum_name = field.getAttribute("extends") orelse continue;
        if (Enum.isBackwardsCompatAlias(field)) continue;

        // Some extensions define fields for other extensions,
        // these are also defined in those extensions, so just skip them
        if (field.getAttribute("extnumber")) |_| continue;
        const kv = spec.enums.get(enum_name).?;
        kv.value.processFieldFromXml(field, ext_nr);
    }
}

fn processFeatures(spec: *Spec, registry: xml.Document) void {
    var feature_it = registry.root.findChildrenByTag("feature");
    while (feature_it.next()) |feature| {
        var req_it = feature.findChildrenByTag("require");
        while (req_it.next()) |req| {
            var enum_it = req.findChildrenByTag("enum");
            while (enum_it.next()) |field| {
                const enum_name = field.getAttribute("extends") orelse continue;
                if (Enum.isBackwardsCompatAlias(field)) continue;

                const kv = spec.enums.get(enum_name).?;
                kv.value.processFieldFromXml(field, null);
            }
        }
    }
}
