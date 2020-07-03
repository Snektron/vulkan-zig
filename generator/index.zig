pub const generateVk = @import("vulkan/generator.zig").generate;

test "main" {
    _ = @import("xml.zig");
    _ = @import("c-parse.zig");
}
