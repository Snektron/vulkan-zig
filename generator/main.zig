const std = @import("std");

pub fn main() void {
    std.debug.warn("Test\n");
}

test "main" {
    _ = @import("xml.zig");
    _ = @import("utf8.zig");
}
