const std = @import("std");
const xml = @import("xml.zig");
const mem = std.mem;
const Allocator = mem.Allocator;
const SegmentedList = std.SegmentedList;
const StringHashMap = std.StringHashMap;

pub const Registry = struct {
    arena: std.heap.ArenaAllocator,

    enums: StringHashMap(EnumInfo),
    bitmasks: StringHashMap(BitmaskInfo),
    handles: StringHashMap(HandleInfo),
    structs: StringHashMap(StructInfo),

    extensions: SegmentedList(ExtensionInfo, 0),

    fn init(allocator: *Allocator) !*Registry {
        // Use this construction to make sure that the extensions list contains a valid pointer to an allocator
        const registry = blk: {
            var arena = std.heap.ArenaAllocator.init(allocator);
            errdefer arena.deinit();

            const registry = try arena.allocator.create(Registry);
            registry.* = .{
                .arena = arena,
                .enums = StringHashMap(EnumInfo).init(allocator),
                .bitmasks = StringHashMap(BitmaskInfo).init(allocator),
                .handles = StringHashMap(HandleInfo).init(allocator),
                .structs = StringHashMap(StructInfo).init(allocator),
                .extensions = undefined
            };

            break :blk registry;
        };

        registry.extensions = SegmentedList(ExtensionInfo, 0).init(&registry.arena.allocator);

        return registry;
    }

    fn deinit(self: *Registry) void {
        self.enums.deinit();
        self.bitmasks.deinit();
        self.handles.deinit();
        self.structs.deinit();

        // Copy to stack so that the arena doesn't destroy itself
        var arena = self.arena;
        arena.deinit();
    }

    fn dump(self: *Registry) void {
        {
            std.debug.warn("Enums:\n", .{});
            var it = self.enums.iterator();
            while (it.next()) |e| {
                const kind_text = if (e.value.kind == .Bitmask) " (bitmask)" else "";
                std.debug.warn("    {}{}:\n", .{ e.key, kind_text });

                var variant_it = e.value.variants.iterator(0);
                while (variant_it.next()) |variant| {
                    std.debug.warn("        {}\n", .{variant.name});
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
                    .Bits => |bits| std.debug.warn(" [bits: {}]\n", .{bits}),
                    .Alias => |alias| std.debug.warn(" [alias of: {}]\n", .{alias}),
                }
            }
        }

        {
            std.debug.warn("Handles:\n", .{});
            var it = self.handles.iterator();
            while (it.next()) |kv| {
                std.debug.warn("    {}", .{kv.key});

                switch (kv.value) {
                    .Alias => |alias| std.debug.warn(" (alias of {})\n", .{alias}),
                    .NonDispatchable => std.debug.warn(" (non-dispatchable)\n", .{}),
                    else => std.debug.warn("\n", .{}),
                }
            }
        }

        {
            std.debug.warn("Structs:\n", .{});
            var it = self.structs.iterator();
            while (it.next()) |kv| {
                std.debug.warn("    {} ({} fields)\n", .{kv.key, kv.value.members.count()});
            }
        }

        {
            std.debug.warn("Extensions:\n", .{});
            var it = self.extensions.iterator(0);
            while (it.next()) |ext| {
                std.debug.warn("    {}: {}, version {}\n", .{ext.number, ext.name, ext.version});
            }
        }
    }
};

// Type info of fields, function parameters, and return types.
const TypeInfo = struct {
    const PointerSize = enum {
        One,
        Many, // The length is either given by some expression
        ZeroTerminated
    };

    const Pointer = struct {
        is_const: bool,
        size: PointerSize
    };

    name: []const u8,
    pointers: []Pointer, // Outer-most pointer is the first element
    array_size: ?[]const u8,

    fn fromXml(allocator: *Allocator, elem: *xml.Element) TypeInfo {
        var type_info = TypeInfo {
            .name = elem.getCharData("type").?,
            .pointers = &[_]Pointer{},
            .array_size = elem.getCharData("enum")
        };

        // Find the element which contains the stars of the pointers
        var stars: ?[]const u8 = null;
        var child_it = elem.children.iterator(0);
        while (child_it.next()) |child| {
            if (child.* == .CharData and mem.indexOf(u8, child.CharData, "*") != null) {
                stars = child.CharData;
                break;
            }
        }

        if (stars) |ptr_text| {
            var npointers: usize = 0;
            for (ptr_text) |c| {
                if (c == '*') npointers += 1;
            }

            type_info.pointers = allocator.alloc(TypeInfo.Pointer, npointers) catch unreachable;

            // Read the sizes of each pointer
            if (elem.getAttribute("len")) |lens| {
                var len_it = std.mem.separate(lens, ",");
                for (type_info.pointers) |*ptr, i| {
                    ptr.size = if (len_it.next()) |len| lenToPointerSize(len) else .One;
                    ptr.is_const = false;
                }
            } else {
                for (type_info.pointers) |*ptr| {
                    ptr.size = .One;
                    ptr.is_const = false;
                }
            }

            // Read the constness of each pointer
            // Beware: the const of the inner pointer is given before the type name
            // while the others are in the `ptr_text`.

            // Check the inner-most pointer
            const first_child = elem.children.at(0);
            const first_const = first_child.* == .CharData and mem.indexOf(u8, first_child.CharData, "const") != null;
            type_info.pointers[npointers - 1].is_const = first_const;

            // Check the outer pointers
            var const_it = std.mem.separate(ptr_text, "*");
            _ = const_it.next().?; // Skip the first field
            var i = npointers - 1;
            while (i > 0) {
                i -= 1;
                const is_const = mem.indexOf(u8, const_it.next().?, "const") != null;
                type_info.pointers[npointers - i - 1].is_const = is_const;
            }
        }

        return type_info;
    }

    fn dump(self: TypeInfo) void {
        for (self.pointers) |ptr| {
            switch (ptr.size) {
                .One => std.debug.warn("*", .{}),
                .Many => std.debug.warn("[*]", .{}),
                .ZeroTerminated => std.debug.warn("[*:0]", .{})
            }

            if (ptr.is_const) {
                std.debug.warn("const ", .{});
            }
        }

        if (self.array_size) |array_size| {
            std.debug.warn("[{}]", .{array_size});
        }

        std.debug.warn("{}", .{self.name});
    }

    fn lenToPointerSize(len: []const u8) PointerSize {
        if (mem.eql(u8, len, "null-terminated")) {
            return .ZeroTerminated;
        } else if (mem.eql(u8, len, "1")) {
            return .One;
        } else {
            return .Many;
        }
    }
};

const StructInfo = struct {
    const Member = struct {
        name: []const u8,
        ty: TypeInfo,
    };

    members: std.SegmentedList(Member, 0),
    extends: ?[]const u8,

    fn init(allocator: *Allocator, extends: ?[]const u8) StructInfo {
        return .{
            .members = std.SegmentedList(Member, 0).init(allocator),
            .extends = extends,
        };
    }
};

const HandleInfo = union(enum) {
    Dispatchable,
    NonDispatchable,
    Alias: []const u8
};

const BitmaskInfo = union(enum) {
    None,
    Bits: []const u8,
    Alias: []const u8
};

const ExtensionInfo = struct {
    name: []const u8,
    number: u32,
    version: u32,
};

const EnumInfo = struct {
    const Kind = enum {
        Bitmask,
        EnumInfo,

        fn parse(str: []const u8) !Kind {
            if (mem.eql(u8, str, "bitmask")) {
                return .Bitmask;
            } else if (mem.eql(u8, str, "enum")) {
                return .EnumInfo;
            } else {
                return error.InvalidEnumInfoKind;
            }
        }
    };

    const Value = union(enum) {
        Bitpos: u5, //log2(u32.bit_count)
        Value: i32,
        Alias: []const u8,
    };

    const Variant = struct {
        name: []const u8,
        value: Value
    };

    kind: Kind,
    variants: std.SegmentedList(Variant, 0),

    fn init(allocator: *Allocator, kind: Kind) EnumInfo {
        return .{
            .kind = kind,
            .variants = std.SegmentedList(Variant, 0).init(allocator)
        };
    }

    fn fromXml(allocator: *Allocator, enums: *xml.Element) EnumInfo {
        const kind = EnumInfo.Kind.parse(enums.getAttribute("type").?) catch unreachable;
        var e = EnumInfo.init(allocator, kind);
        var it = enums.findChildrenByTag("enum");
        while (it.next()) |variant| {
            e.processVariantFromXml(variant, null);
        }

        return e;
    }

    fn addVariant(self: *EnumInfo, name: []const u8, value: Value) void {
        const ptr = self.variants.push(.{.name = name, .value = value}) catch unreachable;
    }

    fn processVariantFromXml(self: *EnumInfo, variant: *xml.Element, ext_nr: ?u32) void {
        if (EnumInfo.isBackwardsCompatAlias(variant)) return;
        const name = variant.getAttribute("name").?;
        const value = blk: {
            if (variant.getAttribute("value")) |value_str| {
                const value = if (mem.startsWith(u8, value_str, "0x"))
                    std.fmt.parseInt(i32, value_str[2..], 16) catch unreachable
                else
                    std.fmt.parseInt(i32, value_str, 10) catch unreachable;

                break :blk Value{.Value = value};
            } else if (variant.getAttribute("bitpos")) |bitpos_str| {
                break :blk Value{.Bitpos = std.fmt.parseInt(u5, bitpos_str, 10) catch unreachable};
            } else if (variant.getAttribute("alias")) |alias| {
                break :blk Value{.Alias = alias};
            } else if (variant.getAttribute("offset")) |offset_str| {
                const offset = std.fmt.parseInt(u32, offset_str, 10) catch unreachable;

                const actual_ext_nr = ext_nr orelse blk: {
                    const ext_nr_str = variant.getAttribute("extnumber").?;
                    break :blk std.fmt.parseInt(u32, ext_nr_str, 10) catch unreachable;
                };

                const abs_value = EnumInfo.extensionEnumInfoValue(actual_ext_nr, offset);
                const value = if (variant.getAttribute("dir")) |_| -@intCast(i32, abs_value) else @intCast(i32, abs_value);

                break :blk Value{.Value = value};
            } else {
                unreachable;
            }
        };

        self.addVariant(name, value);
    }

    fn isBackwardsCompatAlias(variant: *xml.Element) bool {
        if (variant.getAttribute("comment")) |comment| {
            return mem.eql(u8, comment, "Backwards-compatible alias containing a typo") or
                mem.eql(u8, comment, "Deprecated name for backwards compatibility");
        }

        return false;
    }

    fn extensionEnumInfoValue(ext_nr: u32, offset: u32) u32 {
        const extension_value_base = 1000000000;
        const extension_block = 1000;
        return extension_value_base + (ext_nr - 1) * extension_block + offset;
    }
};

pub fn generate(backing_allocator: *Allocator, root: *xml.Element) *Registry {
    std.debug.assert(mem.eql(u8, root.tag, "registry"));

    var registry = Registry.init(backing_allocator) catch unreachable;

    processTypes(registry, root);
    processEnumInfos(registry, root);
    processFeatures(registry, root);
    processExtensions(registry, root);

    return registry;
}

fn processTypes(registry: *Registry, root: *xml.Element) void {
    var types = root.findChildByTag("types").?;
    var it = types.findChildrenByTag("type");
    while (it.next()) |ty| {
        const category = ty.getAttribute("category") orelse continue;
        if (mem.eql(u8, category, "bitmask")) {
            processBitmaskInfoType(registry, ty);
        } else if (mem.eql(u8, category, "handle")) {
            processHandleType(registry, ty);
        } else if (mem.eql(u8, category, "struct")) {
            processStructType(registry, ty);
        }
    }
}

fn processBitmaskInfoType(registry: *Registry, ty: *xml.Element) void {
    if (ty.getAttribute("name")) |name| {
        const alias = ty.getAttribute("alias").?;
        if (registry.bitmasks.put(name, .{.Alias = alias}) catch unreachable) |_| unreachable;
    } else {
        const name = ty.getCharData("name").?;
        const bits: BitmaskInfo = if (ty.getAttribute("requires")) |bits_name| .{.Bits = bits_name} else .None;
        if (registry.bitmasks.put(name, bits) catch unreachable) |_| unreachable;
    }
}

fn processHandleType(registry: *Registry, ty: *xml.Element) void {
    if (ty.getAttribute("alias")) |alias| {
        const name = ty.getAttribute("name").?;
        if (registry.handles.put(name, .{.Alias = alias}) catch unreachable) |_| unreachable;
    } else {
        const define_type_str = ty.getCharData("type").?;
        const name = ty.getCharData("name").?;
        const handle: HandleInfo = if (std.mem.eql(u8, define_type_str, "VK_DEFINE_HANDLE")) .Dispatchable else .NonDispatchable;
        if (registry.handles.put(name, handle) catch unreachable) |_| unreachable;
    }
}

fn processStructType(registry: *Registry, ty: *xml.Element) void {
    const name = ty.getAttribute("name").?;
    const extends = ty.getAttribute("structextends");

    var s = StructInfo.init(&registry.arena.allocator, extends);

    std.debug.warn("{}:\n", .{name});

    var members = ty.findChildrenByTag("member");
    while (members.next()) |member| {
        const member_name = member.getCharData("name").?;
        const type_info = TypeInfo.fromXml(&registry.arena.allocator, member);

        std.debug.warn("    {} = ", .{member_name});
        type_info.dump();
        std.debug.warn("\n", .{});
    }

    if (registry.structs.put(name, s) catch unreachable) |_| unreachable;
}

fn processEnumInfos(registry: *Registry, root: *xml.Element) void {
    var it = root.findChildrenByTag("enums");
    while (it.next()) |enums| {
        const name = enums.getAttribute("name").?;
        if (!mem.eql(u8, name, "API Constants")) {
            const e = EnumInfo.fromXml(&registry.arena.allocator, enums);
            if (registry.enums.put(name, e) catch unreachable) |_| unreachable;
        }
    }
}

fn processExtensions(registry: *Registry, root: *xml.Element) void {
    var extensions = root.findChildByTag("extensions").?;
    var ext_it = extensions.findChildrenByTag("extension");
    while (ext_it.next()) |ext| {
        if (ext.getAttribute("supported")) |support| {
            if (mem.eql(u8, support, "disabled")) continue;
        }

        processExtension(registry, ext);
    }
}

fn processExtension(registry: *Registry, ext: *xml.Element) void {
    const ext_nr_str = ext.getAttribute("number").?;
    const ext_nr = std.fmt.parseInt(u32, ext_nr_str, 10) catch unreachable;

    var version: ?u32 = null;

    var req_it = ext.findChildrenByTag("require");
    while (req_it.next()) |req| {
        var it = req.findChildrenByTag("enum");
        while (it.next()) |variant| {
            if (variant.getAttribute("extends")) |enum_name| {
                // Some extensions define variants for other extensions,
                // these are also defined in those extensions, so just skip them
                if (variant.getAttribute("extnumber")) |_| continue;

                const kv = registry.enums.get(enum_name).?;
                kv.value.processVariantFromXml(variant, ext_nr);
            } else if (variant.getAttribute("name")) |name| {
                if (mem.endsWith(u8, name, "_SPEC_VERSION")) {
                    const version_str = variant.getAttribute("value").?;
                    version = std.fmt.parseInt(u32, version_str, 10) catch unreachable;
                }
            }
        }
    }

    var ext_info = ExtensionInfo{
        .name = ext.getAttribute("name").?,
        .number = ext_nr,
        .version = version.?
    };

    registry.extensions.push(ext_info) catch unreachable;
}

fn processFeatures(registry: *Registry, root: *xml.Element) void {
    var feature_it = root.findChildrenByTag("feature");
    while (feature_it.next()) |feature| {
        var req_it = feature.findChildrenByTag("require");
        while (req_it.next()) |req| {
            var enum_it = req.findChildrenByTag("enum");
            while (enum_it.next()) |variant| {
                const enum_name = variant.getAttribute("extends") orelse continue;
                const kv = registry.enums.get(enum_name).?;
                kv.value.processVariantFromXml(variant, null);
            }
        }
    }
}
