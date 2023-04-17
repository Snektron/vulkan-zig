const std = @import("std");
const vkgen = @import("generator/index.zig");

pub const ShaderCompileStep = vkgen.ShaderCompileStep;
pub const VkGenerateStep = vkgen.VkGenerateStep;

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const vk_xml_path: ?[]const u8 = b.option([]const u8, "registry", "Override the path to the Vulkan registry");

    // using the package manager, this artifact can be obtained by the user
    // through `b.dependency(<name in build.zig.zon>, .{}).artifact("generator")`.
    // with that, the user need only `.addArg("path/to/vk.xml")`, and then obtain
    // a file source to the generated code with `.addOutputArg("vk.zig")`
    const generator_exe = b.addExecutable(.{
        .name = "generator",
        .root_source_file = .{ .path = "generator/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(generator_exe);

    // or they can skip all that, and just make sure to pass `.registry = "path/to/vk.xml"` to `b.dependency`,
    // and then obtain the module directly via `.module("vulkan-zig")`.
    if (vk_xml_path) |path| {
        const generate_cmd = b.addRunArtifact(generator_exe);

        if (!std.fs.path.isAbsolute(path)) @panic("Make sure to assign an absolute path to the `registry` option (see: std.Build.pathFromRoot).\n");
        generate_cmd.addArg(path);

        _ = b.addModule("vulkan-zig", .{
            .source_file = generate_cmd.addOutputFileArg("vk.zig"),
        });
    }

    // remainder of the script is for local testing

    const triangle_exe = b.addExecutable(.{
        .name = "triangle",
        .root_source_file = .{ .path = "examples/triangle.zig" },
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(triangle_exe);
    triangle_exe.linkLibC();
    triangle_exe.linkSystemLibrary("glfw");

    const example_registry = b.option([]const u8, "example-registry", "Override the path to the Vulkan registry used for the examples") orelse "examples/vk.xml";
    const gen = VkGenerateStep.create(b, example_registry);
    triangle_exe.addModule("vulkan", gen.getModule());

    const shaders = ShaderCompileStep.create(
        b,
        &[_][]const u8{ "glslc", "--target-env=vulkan1.2" },
        "-o",
    );
    shaders.add("triangle_vert", "examples/shaders/triangle.vert", .{});
    shaders.add("triangle_frag", "examples/shaders/triangle.frag", .{});
    triangle_exe.addModule("shaders", shaders.getModule());

    const triangle_run_cmd = b.addRunArtifact(triangle_exe);
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
