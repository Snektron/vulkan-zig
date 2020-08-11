// See https://www.khronos.org/registry/spir-v/specs/unified1/MachineReadableGrammar.html

pub const Registry = struct {
    copyright: []const u8,
    registry_type: RegistryType,
    instruction: []Instruction,
    operand_kinds: []OperandKind,
};

pub const RegistryType = union(enum) {
    core: struct {
        magic_number: u32,
        major_version: u32,
        minor_version: u32,
        revision: u32,
    },
    extension: struct {
        version: u32,
        revision: u32,
    },
};

pub const Instruction = struct {
    opname: []const u8,
    opcode: u32,
    operands: []Operand,
    capabilities: [][]const u8,
};

pub const Operand = struct {
    kind: []const u8,
    quantifier: Quantifier,
    name: []const u8,
};

pub const Quantifier = enum {
    one,
    zero_or_one,
    zero_or_more,
};

pub const OperandCategory = union(enum) {
    bit_enum: []Enumerant,
    value_enum: []Enumerant,
    id,
    literal,
    composite: Composite,
};

pub const OperandKind = struct {
    category: OperandCategory,
    name: []const u8,
    doc: []const u8,
};

pub const Enumerant = struct {
    name: []const u8,
    value: u32, // A power of 2 for `.bit_enum`.
    capabilities: [][]const u8,
    parameters: []Operand, // `quantifier` will always be `.one`.
};

pub const Composite = struct {
    bases: [][]const u8,
};
