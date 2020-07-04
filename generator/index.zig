pub const generateVk = @import("vulkan/generator.zig").generate;
pub const VkGenerateStep = @import("vulkan/build-integration.zig").GenerateStep;
pub const ShaderCompileStep = @import("build-integration.zig").ShaderCompileStep;

test "main" {
    _ = @import("xml.zig");
    _ = @import("c-parse.zig");
}
