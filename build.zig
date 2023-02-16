const std = @import("std");
const vkgen = @import("generator/index.zig");
const Step = std.build.Step;

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const generator_exe = b.addExecutable(.{
        .name = "vulkan-zig-generator",
        .root_source_file = .{ .path = "generator/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    generator_exe.install();

    const triangle_exe = b.addExecutable(.{
        .name = "triangle",
        .root_source_file = .{ .path = "examples/triangle.zig" },
        .target = target,
        .optimize = optimize,
    });
    triangle_exe.install();
    triangle_exe.linkLibC();
    triangle_exe.linkSystemLibrary("glfw");

    const vk_xml_path = b.option([]const u8, "vulkan-registry", "Override the path to the Vulkan registry") orelse "examples/vk.xml";

    const gen = vkgen.VkGenerateStep.create(b, vk_xml_path, "vk.zig");
    triangle_exe.addModule("vulkan", gen.getModule());

    const shaders = vkgen.ShaderCompileStep.create(
        b,
        &[_][]const u8{ "glslc", "--target-env=vulkan1.2" },
        "-o",
    );
    shaders.add("triangle_vert", "examples/shaders/triangle.vert", .{});
    shaders.add("triangle_frag", "examples/shaders/triangle.frag", .{});
    triangle_exe.addModule("shaders", shaders.getModule());

    const triangle_run_cmd = b.addRunArtifact(triangle_exe);
    triangle_run_cmd.condition = .always;
    triangle_run_cmd.step.dependOn(b.getInstallStep());
    const triangle_run_step = b.step("run-triangle", "Run the triangle example");
    triangle_run_step.dependOn(&triangle_run_cmd.step);

    var test_target = b.addTest(.{
        .root_source_file = .{ .path = "generator/index.zig" },
    });

    var test_step = b.step("test", "Run all the tests");
    test_step.dependOn(&test_target.step);

    // This test needs to be an object so that vulkan-zig can import types from the root.
    // It does not need to run anyway.
    const ref_all_decls_test = b.addObject(.{
        .name = "ref-all-decls-test",
        .root_source_file = .{ .path = "test/ref_all_decls.zig" },
        .target = target,
        .optimize = optimize,
    });
    ref_all_decls_test.addModule("vulkan", gen.getModule());
    test_step.dependOn(&ref_all_decls_test.step);
}
