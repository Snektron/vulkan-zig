const std = @import("std");
const xml = @import("xml.zig");
const mem = std.mem;
const Allocator = mem.Allocator;
const SegmentedList = std.SegmentedList;
const StringHashMap = std.StringHashMap;

fn count(haystack: []const u8, needle: u8) usize {
    var total: usize = 0;
    for (haystack) |elem| {
        if (elem == needle) {
            total += 1;
        }
    }

    return total;
}

pub const Registry = struct {
    arena: std.heap.ArenaAllocator,

    enums: StringHashMap(EnumInfo),
    bitmasks: StringHashMap(BitmaskInfo),
    handles: StringHashMap(HandleInfo),
    structs: StringHashMap(StructInfo),
    commands: StringHashMap(CommandInfo),
    fn_ptrs: StringHashMap(CommandInfo),
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
                .commands = StringHashMap(CommandInfo).init(allocator),
                .fn_ptrs = StringHashMap(CommandInfo).init(allocator),
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
        self.commands.deinit();
        self.fn_ptrs.deinit();

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
                std.debug.warn("    {} ({} aliases):\n", .{kv.key, kv.value.aliases.count()});

                var member_it = kv.value.members.iterator(0);
                while (member_it.next()) |member| {
                    std.debug.warn("        {} = {}\n", .{member.name, member.type_info});
                }
            }
        }

        {
            std.debug.warn("Commands:\n", .{});
            var it = self.commands.iterator();
            while (it.next()) |kv| {
                std.debug.warn("    fn {}(\n", .{kv.key});

                var param_it = kv.value.parameters.iterator(0);
                while (param_it.next()) |param| {
                    std.debug.warn("        {}: {},\n", .{param.name, param.type_info});
                }

                std.debug.warn("    ) {}\n", .{kv.value.return_type_info});

                if (kv.value.success_codes.len > 0) {
                    std.debug.warn("        Success codes:\n", .{});
                    for (kv.value.success_codes) |code| {
                        std.debug.warn("            {}\n", .{code});
                    }
                }

                if (kv.value.error_codes.len > 0) {
                    std.debug.warn("        Error codes:\n", .{});
                    for (kv.value.error_codes) |code| {
                        std.debug.warn("            {}\n", .{code});
                    }
                }
            }
        }

        {
            std.debug.warn("Function pointers:\n", .{});
            var it = self.fn_ptrs.iterator();
            while (it.next()) |kv| {
                std.debug.warn("    {} = fn(\n", .{kv.key});

                var param_it = kv.value.parameters.iterator(0);
                while (param_it.next()) |param| {
                    std.debug.warn("        {}: {},\n", .{param.name, param.type_info});
                }

                std.debug.warn("    ) {}\n", .{kv.value.return_type_info});
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
        var type_info = TypeInfo{
            .name = elem.getCharData("type").?,
            .pointers = &[_]Pointer{},
            .array_size = elem.getCharData("enum")
        };

        // Find the element which contains the stars of the pointers
        var stars: ?[]const u8 = null;
        var child_it = elem.children.iterator(0);
        while (child_it.next()) |child| {
            if (child.* == .CharData and mem.indexOfScalar(u8, child.CharData, '*') != null) {
                stars = child.CharData;
                break;
            }
        }

        if (stars) |ptr_text| {
            const npointers = count(ptr_text, '*');

            type_info.pointers = allocator.alloc(TypeInfo.Pointer, npointers) catch unreachable;

            // Read the sizes of each pointer
            if (elem.getAttribute("len")) |lens| {
                var len_it = std.mem.separate(lens, ",");
                for (type_info.pointers) |*ptr, i| {
                    ptr.size = if (len_it.next()) |len| lenToPointerSize(len) else .One;
                }
            } else {
                for (type_info.pointers) |*ptr| {
                    ptr.size = .One;
                }
            }

            // Read the constness of each pointer
            // Beware: the const of the inner pointer is given before the type name
            // while the others are in the `ptr_text`.

            const pre = switch (elem.children.at(0).*) {
                .CharData => |char_data| char_data,
                else => ""
            };

            type_info.parseConstness(pre, ptr_text);
        }

        return type_info;
    }

    fn fromFnPtrReturnTypeXml(allocator: *Allocator, elem: *xml.Element) TypeInfo {
        // In function pointers, the return type is not contained within a designated tag.
        // The first chardata of the type has the following structure: 'typedef <return type> (VKAPI_PTR *'
        // In order to parse the <return type>, strip everything from it until the last star and take the last word
        // to be the type. Then parse everything in front of the return type word and after the return type word
        const proto = elem.children.at(0).CharData;
        std.debug.assert(mem.startsWith(u8, proto, "typedef ") and mem.endsWith(u8, proto, " (VKAPI_PTR *"));
        const return_type = proto["typedef ".len .. proto.len - " (VKAPI_PTR *".len];

        var first_star = return_type.len;
        var npointers: usize = 0;
        var i = return_type.len;
        while (i > 0) {
            i -= 1;
            if (return_type[i] == '*') {
                first_star = i;
                npointers += 1;
            }
        }

        const name_start = if (mem.lastIndexOfScalar(u8, return_type[0 .. first_star], ' ')) |index| index + 1 else 0;
        var type_info = TypeInfo{
            .name = return_type[name_start .. first_star],
            .pointers = &[_]Pointer{},
            .array_size = null
        };

        if (npointers > 0) {
            type_info.pointers = allocator.alloc(TypeInfo.Pointer, npointers) catch unreachable;
            for (type_info.pointers) |*ptr| {
                ptr.size = .One;
            }

            type_info.parseConstness(return_type[0 .. name_start], return_type[first_star ..]);
        }

        return type_info;
    }

    fn fromFnPtrParamTypeXml(allocator: *Allocator, pre: []const u8, name: []const u8, post: []const u8) TypeInfo {
        // `pre` and `post` contain information that is shared with other types, seperated by commas. In
        // the case of `pre`, get everything after the comma (if present), and for `post`, everything before
        // and including the last star before the last. If there is no star, the segment contains no
        // useful information anyway. Note that the star should never appear *after* the comma (that wouldn't be
        // a valid C type).
        // Ex: <type>void</type>* pUserData, <type>void</type>* pMemory

        const pre_start = if (mem.indexOfScalar(u8, pre, ',')) |index| index + 1 else pre.len;
        const post_end = if (mem.indexOfScalar(u8, post, '*')) |index| index + 1 else 0;
        const npointers = count(post[0 .. post_end], '*');

        var type_info = TypeInfo{
            .name = name,
            .pointers = &[_]Pointer{},
            .array_size = null
        };

        if (npointers > 0) {
            type_info.pointers = allocator.alloc(TypeInfo.Pointer, npointers) catch unreachable;
            for (type_info.pointers) |*ptr| {
                ptr.size = .One;
            }

            type_info.parseConstness(pre[pre_start ..], post[0 .. post_end]);
        }

        return type_info;
    }

    fn parseConstness(self: *TypeInfo, pre: []const u8, post: []const u8) void {
        // Beware: the const of the inner pointer is given before the type name (in `pre`)
        // while the others are in the `post`.

        // Check the outer pointers
        var const_it = std.mem.separate(post, "*");
        var i: usize = self.pointers.len;
        while (i > 0) {
            i -= 1;
            const is_const = mem.indexOf(u8, const_it.next().?, "const") != null;
            self.pointers[i].is_const = is_const;
        }

        // Check the inner-most pointer
        const first_const = mem.indexOf(u8, pre, "const") != null;
        self.pointers[self.pointers.len - 1].is_const = first_const;
    }

    pub fn format(
        self: TypeInfo,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        context: var,
        comptime Errors: type,
        output: fn (@TypeOf(context), []const u8) Errors!void
    ) Errors!void {
        if (self.array_size) |array_size| {
            try std.fmt.format(context, Errors, output, "[{}]", .{array_size});
        }

        for (self.pointers) |ptr| {
            switch (ptr.size) {
                .One => try output(context, "*"),
                .Many => try output(context, "[*]"),
                .ZeroTerminated => try output(context, "[*:0]")
            }

            if (ptr.is_const) {
                try output(context, "const ");
            }
        }

        try output(context, self.name);
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
        type_info: TypeInfo
    };

    members: SegmentedList(Member, 0),
    aliases: SegmentedList([]const u8, 0),

    fn init(allocator: *Allocator) StructInfo {
        return .{
            .members = SegmentedList(Member, 0).init(allocator),
            .aliases = SegmentedList([]const u8, 0).init(allocator)
        };
    }

    fn fromXml(allocator: *Allocator, elem: *xml.Element) StructInfo {
        var s = StructInfo.init(allocator);

        var members = elem.findChildrenByTag("member");
        while (members.next()) |member| {
            const member_name = member.getCharData("name").?;
            const type_info = TypeInfo.fromXml(allocator, member);

            s.addMember(member_name, type_info);
        }

        return s;
    }

    fn addMember(self: *StructInfo, name: []const u8, type_info: TypeInfo) void {
        self.members.push(.{.name = name, .type_info = type_info}) catch unreachable;
    }

    fn addAlias(self: *StructInfo, alias: []const u8) void {
        self.aliases.push(alias) catch unreachable;
    }
};

const CommandInfo = struct {
    const Parameter = struct {
        name: []const u8,
        type_info: TypeInfo
    };

    parameters: SegmentedList(Parameter, 0),
    return_type_info: TypeInfo,
    success_codes: []const []const u8,
    error_codes: []const []const u8,
    aliases: SegmentedList([]const u8, 0),

    fn init(allocator: *Allocator, return_type_info: TypeInfo) CommandInfo {
        return .{
            .parameters = SegmentedList(Parameter, 0).init(allocator),
            .return_type_info = return_type_info,
            .success_codes = &[_][]u8{},
            .error_codes = &[_][]u8{},
            .aliases = SegmentedList([]const u8, 0).init(allocator)
        };
    }

    fn fromXml(allocator: *Allocator, elem: *xml.Element) CommandInfo {
        const return_type_info = TypeInfo.fromXml(allocator, elem.findChildByTag("proto").?);
        var cmd = CommandInfo.init(allocator, return_type_info);

        if (elem.getAttribute("successcodes")) |codes| {
            cmd.success_codes = CommandInfo.splitResultCodes(allocator, codes);
        }

        if (elem.getAttribute("errorcodes")) |codes| {
            cmd.error_codes = CommandInfo.splitResultCodes(allocator, codes);
        }

        var parameters = elem.findChildrenByTag("param");
        while (parameters.next()) |param| {
            const param_name = param.getCharData("name").?;
            const type_info = TypeInfo.fromXml(allocator, param);

            cmd.addParameter(param_name, type_info);
        }

        return cmd;
    }

    fn fromFnPtrXml(allocator: *Allocator, elem: *xml.Element) CommandInfo {
        const return_type_info = TypeInfo.fromFnPtrReturnTypeXml(allocator, elem);
        var cmd = CommandInfo.init(allocator, return_type_info);

        // The parameters of a function pointer are formulated a bit weird, which is why
        // the chardata surrounding a <type> is also required to parse it completely.
        // This loop assumes there are no other elements in a function pointers declatation.
        var i: usize = 3; // The first parameter's type is at offset 3
        while (i < elem.children.count() - 1) : (i += 2) {
            const pre = elem.children.at(i - 1).CharData;
            const type_name = elem.children.at(i).Element.children.at(0).CharData;
            const post = elem.children.at(i + 1).CharData;
            const type_info = TypeInfo.fromFnPtrParamTypeXml(allocator, pre, type_name, post);

            //  To find the type name, take everything until the first space before the last ) or ,.
            const name_end = mem.lastIndexOfAny(u8, post, "),").?;
            const name_start = mem.lastIndexOfScalar(u8, post[0 .. name_end], ' ').? + 1;
            const name = post[name_start .. name_end];

            cmd.addParameter(name, type_info);
        }

        return cmd;
    }

    fn splitResultCodes(allocator: *Allocator, text: []const u8) []const []const u8 {
        const ncodes = 1 + count(text, ',');
        const codes = allocator.alloc([]const u8, ncodes) catch unreachable;

        var it = mem.separate(text, ",");

        for (codes) |*code, i| {
            code.* = it.next().?;
        }

        return codes;
    }

    fn addParameter(self: *CommandInfo, name: []const u8, type_info: TypeInfo) void {
        self.parameters.push(.{.name = name, .type_info = type_info}) catch unreachable;
    }

    fn addAlias(self: *CommandInfo, alias: []const u8) void {
        self.aliases.push(alias) catch unreachable;
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
    variants: SegmentedList(Variant, 0),

    fn init(allocator: *Allocator, kind: Kind) EnumInfo {
        return .{
            .kind = kind,
            .variants = SegmentedList(Variant, 0).init(allocator)
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
    processEnums(registry, root);
    processCommands(registry, root);
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
        } else if (mem.eql(u8, category, "funcpointer")) {
            processFuncPointerType(registry, ty);
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

    if (ty.getAttribute("alias")) |alias| {
        // Aliases should always be defined after their parent type, so this should be safe
        var s = &registry.structs.get(alias).?.value;
        s.addAlias(name);
        return;
    }

    const s = StructInfo.fromXml(&registry.arena.allocator, ty);
    if (registry.structs.put(name, s) catch unreachable) |_| unreachable;
}

fn processFuncPointerType(registry: *Registry, ty: *xml.Element) void {
    const name = ty.getCharData("name").?;
    const cmd = CommandInfo.fromFnPtrXml(&registry.arena.allocator, ty);
    if (registry.fn_ptrs.put(name, cmd) catch unreachable) |_| unreachable;
}

fn processEnums(registry: *Registry, root: *xml.Element) void {
    var it = root.findChildrenByTag("enums");
    while (it.next()) |enums| {
        const name = enums.getAttribute("name").?;
        if (!mem.eql(u8, name, "API Constants")) {
            const e = EnumInfo.fromXml(&registry.arena.allocator, enums);
            if (registry.enums.put(name, e) catch unreachable) |_| unreachable;
        }
    }
}

fn processCommands(registry: *Registry, root: *xml.Element) void {
    var commands = root.findChildByTag("commands").?;
    var command_it = commands.findChildrenByTag("command");
    while (command_it.next()) |elem| {
        if (elem.getAttribute("alias")) |alias| {
            const name = elem.getAttribute("name").?;
            var cmd = &registry.commands.get(alias).?.value;
            cmd.addAlias(name);
        } else {
            const name = elem.findChildByTag("proto").?.getCharData("name").?;
            const command = CommandInfo.fromXml(&registry.arena.allocator, elem);
            if (registry.commands.put(name, command) catch unreachable) |_| unreachable;
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
