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
        alias: []const u8,
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
    Alias: []const u8,
    Pointer: Pointer,
    Array: Array,
    Opaque: void,
};

pub const Container = struct {
    pub const Field = struct {
        name: []const u8,
        field_type: TypeInfo,
    };

    fields: []Field,
};

pub const Enum = struct {
    pub const Value = union(enum) {
        bitpos: u5, // 1 << bitpos
        bitvector: u32, // Combined flags
        int: i32,
        alias: []const u8,
    };

    pub const Field = struct {
        name: []const u8,
        value: Value,
    };

    fields: []Field,
    is_bitmask_bits: bool,
};

pub const Bitmask = struct {
    bits_enum: ?[]const u8,
};

pub const Handle = struct {
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
