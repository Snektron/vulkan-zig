// See https://www.khronos.org/registry/spir-v/specs/unified1/MachineReadableGrammar.html

pub const Registry = union(enum) {
    core: CoreRegistry,
    extension: ExtensionRegistry,
};

pub const CoreRegistry = struct {
    copyright: [][]const u8,
    magic_number: []const u8, // Hexadecimal representation of the magic number
    major_version: u32,
    minor_version: u32,
    revision: u32,
    instructions: []Instruction,
    operand_kinds: []OperandKind,
};

pub const ExtensionRegistry = struct {
    copyright: [][]const u8,
    version: u32,
    minor_version: u32,
    revision: u32,
    instructions: []Instruction,
    operand_kinds: []OperandKind,
};

pub const Instruction = struct {
    opname: []const u8,
    opcode: u32,
    operands: []Operand = &[_]Operand{},
    capabilities: [][]const u8 = &[_][]const u8{},
    extensions: [][]const u8 = &[_][]const u8{},
};

pub const Operand = struct {
    kind: []const u8,

    /// Either
    /// - null: exactly once.
    /// - "?": zero or once.
    /// - "*": zero or more.
    quantifier: ?[]const u8 = null,
    name: []const u8 = "",
};

pub const OperandCategory = enum {
    // Note: non-canonical casing to match Spir-V JSON spec/
    BitEnum,
    ValueEnum,
    Id,
    Literal,
    Composite,
};

pub const OperandKind = struct {
    category: OperandCategory,
    kind: []const u8,
    doc: []const u8 = "",
    enumerants: ?[]Enumerant = null,
    bases: ?[]const []const u8 = null,
};

pub const Enumerant = struct {
    enumerant: []const u8,
    value: union(enum) {
        bitflag: []const u8, // Hexadecimal representation of the value
        int: u31,
    },
    capabilities: [][]const u8 = &[_][]const u8{},
    extensions: [][]const u8 = &[_][]const u8{}, // Valid for .ValueEnum
    parameters: []Operand = &[_]Operand{}, // `quantifier` will always be `null`.
};
