pub const generateVk = @import("vulkan/generator.zig").generate;
pub const VkGenerateStep = @import("vulkan/build_integration.zig").GenerateStep;
pub const ShaderCompileStep = @import("build_integration.zig").ShaderCompileStep;

test "main" {
    _ = @import("xml.zig");
    _ = @import("vulkan/c_parse.zig");
}
