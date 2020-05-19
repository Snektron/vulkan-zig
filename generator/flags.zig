const std = @import("std");
const testing = std.testing;

fn FlagTraits(comptime Self: type) type {
    return struct {
        pub fn init(flags: var) Self {
            var self: u32 = 0;

            inline for (std.meta.fields(@TypeOf(flags))) |field| {
                self |= @enumToInt(@as(Self, @field(flags, field.name)));
            }

            return @intToEnum(Self, self);
        }

        pub fn merge(lhs: Self, rhs: Self) Self {
            return @intToEnum(Self, @enumToInt(lhs) | @enumToInt(rhs));
        }

        pub fn intersect(lhs: Self, rhs: Self) Self {
            return @intToEnum(Self, @enumToInt(lhs) & @enumToInt(rhs));
        }

        pub fn subtract(lhs: Self, rhs: Self) Self {
            return @intToEnum(Self, @enumToInt(lhs) & @enumToInt(rhs.complement()));
        }

        pub fn complement(self: Self) Self {
            const all = comptime blk: {
                var flags: u32 = 0;
                for (std.meta.fields(Self)) |field| {
                    flags |= field.value;
                }
                break :blk flags;
            };

            return @intToEnum(Self, ~@enumToInt(self) & all);
        }

        pub fn contains(lhs: Self, rhs: Self) bool {
            return @enumToInt(lhs) & @enumToInt(rhs) == @enumToInt(rhs);
        }

        pub fn format(
            self: Self,
            fmt: []const u8,
            options: std.fmt.FormatOptions,
            out_stream: var
        ) @TypeOf(out_stream).Error!void {
            const bits = @enumToInt(self);
            var first = true;
            inline for (std.meta.fields(Self)) |field| {
                if (@popCount(u32, field.value) != 1) continue;

                if (bits & field.value == field.value) {
                    if (first) {
                        first = false;
                    } else {
                        try std.fmt.formatBuf(" | ", options, out_stream);
                    }

                    try std.fmt.formatBuf(field.name, options, out_stream);
                }
            }
        }
    };
}
