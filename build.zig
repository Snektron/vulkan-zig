const std = @import("std");
const vkgen = @import("generator/index.zig");
const Step = std.build.Step;
const Builder = std.build.Builder;

pub fn build(b: *Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const generator_exe = b.addExecutable("vulkan-zig-generator", "generator/main.zig");
    generator_exe.setTarget(target);
    generator_exe.setBuildMode(mode);
    generator_exe.install();

    const triangle_exe = b.addExecutable("triangle", "examples/triangle.zig");
    triangle_exe.setTarget(target);
    triangle_exe.setBuildMode(mode);
    triangle_exe.install();
    triangle_exe.linkLibC();
    triangle_exe.linkSystemLibrary("glfw");

    const vk_xml_path = b.option([]const u8, "vulkan-registry", "Override the path to the Vulkan registry") orelse "examples/vk.xml";

    const gen = vkgen.VkGenerateStep.create(b, vk_xml_path, "vk.zig");
    triangle_exe.addPackage(gen.getPackage("vulkan"));

    const shaders = vkgen.ShaderCompileStep.create(
        b,
        &[_][]const u8{ "glslc", "--target-env=vulkan1.2" },
    );
    shaders.add("triangle_vert", "examples/shaders/triangle.vert", .{});
    shaders.add("triangle_frag", "examples/shaders/triangle.frag", .{});
    triangle_exe.addPackage(shaders.getPackage("shaders"));

    const triangle_run_cmd = triangle_exe.run();
    triangle_run_cmd.step.dependOn(b.getInstallStep());
    const triangle_run_step = b.step("run-triangle", "Run the triangle example");
    triangle_run_step.dependOn(&triangle_run_cmd.step);

    var test_step = b.step("test", "Run all the tests");
    test_step.dependOn(&b.addTest("generator/index.zig").step);

    // This test needs to be an object so that vulkan-zig can import types from the root.
    // It does not need to run anyway.
    const ref_all_decls_test = b.addObject("ref-all-decls-test", "test/ref_all_decls.zig");
    ref_all_decls_test.addPackage(gen.getPackage("vulkan"));
    test_step.dependOn(&ref_all_decls_test.step);
}
