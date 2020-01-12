pub fn Flags(comptime E: type) type {
    comptime {
        var bits: u32 = 0;
        inline for (@typeInfo(E).Enum.fields) |field| {
            const val: u32 = field.value;
            std.debug.assert(@popCount(u32, val) == 1);
            std.debug.assert(bits & val == 0);
            bits |= val;
        }
    }

    return struct {
        const Self = @This();
        bits: u32,

        pub const none = Self.init(.{});
        pub const all = comptime blk: {
            var flags = Self.none;
            inline for (@typeInfo(E).Enum.fields) |field| {
                flags.bits |= field.value;
            }
            break :blk flags;
        };

        pub fn init(flags: var) Self {
            return .{.bits = Self.toBits(flags)};
        }

        pub fn merge(lhs: Self, rhs: Self) Self {
            return .{.bits = lhs.bits | rhs.bits};
        }

        pub fn intersect(lhs: Self, rhs: Self) Self {
            return .{.bits = lhs.bits & rhs.bits};
        }

        pub fn subtract(lhs: Self, rhs: Self) Self {
            return .{.bits = lhs.bits & rhs.complement().bits};
        }

        pub fn complement(self: Self) Self {
            return .{.bits = ~self.bits & Self.all.bits};
        }

        pub fn contains(lhs: Self, rhs: Self) bool {
            return lhs.bits & rhs.bits == rhs.bits;
        }

        pub fn equals(lhs: Self, rhs: Self) bool {
            return lhs.bits == rhs.bits;
        }

        fn toBits(flags: var) u32 {
            const ty = @TypeOf(flags);

            if (ty == Self) {
                return flags.bits;
            }

            switch (@typeInfo(ty)) {
                .EnumLiteral => return @enumToInt(@as(E, flags)),
                .Struct => |struct_info| {
                    var value: u32 = 0;
                    inline for (struct_info.fields) |field| {
                        value |= @enumToInt(@as(E, @field(flags, field.name)));
                    }

                    return value;
                },
                else => @compileError("Value of type '" ++ @typeName(ty)
                    ++ "' cannot be converted into '" ++ @typeName(Self) ++ "'")
            }
        }
    };
}
