const std = @import("std");

const vkgen = @import("vulkan_zig");
const ShaderCompileStep = vkgen.ShaderCompileStep;

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const maybe_override_registry = b.option([]const u8, "override-registry", "Override the path to the Vulkan registry used for the examples");

    const registry = b.dependency("vulkan_headers", .{}).path("registry/vk.xml");

    const triangle_exe = b.addExecutable(.{
        .name = "triangle",
        .root_source_file = b.path("triangle.zig"),
        .target = target,
        .link_libc = true,
        .optimize = optimize,
    });
    b.installArtifact(triangle_exe);
    triangle_exe.linkSystemLibrary("glfw");

    const vk_gen = b.dependency("vulkan_zig", .{}).artifact("vulkan-zig-generator");
    const vk_generate_cmd = b.addRunArtifact(vk_gen);

    if (maybe_override_registry) |override_registry| {
        vk_generate_cmd.addFileArg(.{ .cwd_relative = override_registry });
    } else {
        vk_generate_cmd.addFileArg(registry);
    }

    triangle_exe.root_module.addAnonymousImport("vulkan", .{
        .root_source_file = vk_generate_cmd.addOutputFileArg("vk.zig"),
    });

    const shaders = ShaderCompileStep.create(
        b,
        &[_][]const u8{ "glslc", "--target-env=vulkan1.2" },
        "-o",
    );
    shaders.add("triangle_vert", "shaders/triangle.vert", .{});
    shaders.add("triangle_frag", "shaders/triangle.frag", .{});
    triangle_exe.root_module.addImport("shaders", shaders.getModule());

    const triangle_run_cmd = b.addRunArtifact(triangle_exe);
    triangle_run_cmd.step.dependOn(b.getInstallStep());

    const triangle_run_step = b.step("run-triangle", "Run the triangle example");
    triangle_run_step.dependOn(&triangle_run_cmd.step);
}
