pub const Registry = struct {
    decls: []Declaration,
    api_constants: []ApiConstant,
    tags: []Tag,
};

pub const Declaration = struct {
    name: []const u8,
    decl_type: TypeInfo,
};

pub const ApiConstant = struct {
    pub const Value = union(enum) {
        expr: []const u8,
        alias: []const u8, // Alias of another API constant
    };

    name: []const u8,
    value: Value,
};

pub const Tag = struct {
    name: []const u8,
    author: []const u8,
};

pub const TypeInfo = union(enum) {
    Struct: Container,
    Union: Container,
    Enum: Enum,
    Bitmask: Bitmask,
    Handle: Handle,
    FnPtr: Command,
    Command: Command,
    Alias: []const u8, // Alias of another declaration
    Pointer: Pointer,
    Array: Array,
    Opaque: void,
    Foreign: Foreign
};

pub const Container = struct {
    fields: []Declaration,
};

pub const Enum = struct {
    pub const Value = union(enum) {
        bitpos: u5, // 1 << bitpos
        bit_vector: i32, // Combined flags & some vendor IDs
        int: i32,
        alias: struct {
            alias_name: []const u8,
            is_compat_alias: bool,
        }
    };

    pub const Field = struct {
        name: []const u8,
        value: Value,
    };

    fields: []Field,
    is_bitmask: bool,
};

pub const Bitmask = struct {
    bits_enum: ?[]const u8,
};

pub const Handle = struct {
    parent: ?[]const u8, // VkInstance has no parent
    is_dispatchable: bool,
};

pub const Command = struct {
    const Param = struct {
        name: []const u8,
        param_type: TypeInfo,
    };

    params: []Param,
    return_type: *TypeInfo,
    success_codes: []const []const u8,
    error_codes: []const []const u8,
};

pub const Pointer = struct {
    pub const PointerSize = enum {
        one,
        many, // The length is given by some expression
        zero_terminated
    };

    is_const: bool,
    size: PointerSize,
    child: *TypeInfo,
};

pub const Array = struct {
    pub const ArraySize = union(enum) {
        int: usize,
        alias: []const u8, // Field size is given by an api constant
    };

    size: ArraySize,
    child: *TypeInfo,
};

pub const Foreign = struct {
    dependency: []const u8, // Either a header or vk_platform
};
