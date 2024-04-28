const std = @import("std");
const vkgen = @import("src/index.zig");

pub const ShaderCompileStep = vkgen.ShaderCompileStep;

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const vk_xml_path: ?[]const u8 = b.option([]const u8, "registry", "Override the path to the Vulkan registry");

    // Using the package manager, this artifact can be obtained by the user
    // through `b.dependency(<name in build.zig.zon>, .{}).artifact("vulkan-zig-generator")`.
    // with that, the user need only `.addArg("path/to/vk.xml")`, and then obtain
    // a file source to the generated code with `.addOutputArg("vk.zig")`
    const generator_exe = b.addExecutable(.{
        .name = "vulkan-zig-generator",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(generator_exe);

    // Or they can skip all that, and just make sure to pass `.registry = "path/to/vk.xml"` to `b.dependency`,
    // and then obtain the module directly via `.module("vulkan-zig")`.
    if (vk_xml_path) |path| {
        const generate_cmd = b.addRunArtifact(generator_exe);

        if (!std.fs.path.isAbsolute(path)) @panic("Make sure to assign an absolute path to the `registry` option (see: std.Build.pathFromRoot).\n");
        generate_cmd.addArg(path);

        _ = b.addModule("vulkan-zig", .{
            .root_source_file = generate_cmd.addOutputFileArg("vk.zig"),
        });
    }

    // This section shows a crude example of how to build a program using vulkan-zig by
    // calling the generator executable directly.

    // First, obtain the path to vk.xml. In this example, we will just get it from the command line.
    const example_registry = b.option([]const u8, "example-registry", "Override the path to the Vulkan registry used for the examples") orelse "examples/vk.xml";

    // Obtain a reference to the generator. In a downstream build.zig, you would use
    // `const generator_exe = b.dependency(<name in build.zig.zon>).artifact("vulkan-zig-generator");`.
    // Here we are just going to use our own `generator_exe`.

    // Now create a run artifact, and pass the registry to the generator. The generator is normally
    // invoked as `vulkan-zig-generator <input vk.xml> <output vk.zig>`
    const example_registry_generator_cmd = b.addRunArtifact(generator_exe);
    example_registry_generator_cmd.addArg(example_registry);
    // Obtain a reference to the generated file.
    const example_vk_zig = example_registry_generator_cmd.addOutputFileArg("vk.zig");
    // And turn it into a module.
    const example_vk_zig_module = b.addModule("example-vulkan-zig", .{ .root_source_file = example_vk_zig });

    // Now build a Vulkan program. This is just a simple triangle from the vulkan tutorial.
    const triangle_exe = b.addExecutable(.{
        .name = "triangle",
        .root_source_file = .{ .path = "examples/triangle.zig" },
        .target = target,
        .link_libc = true,
        .optimize = optimize,
    });
    b.installArtifact(triangle_exe);
    triangle_exe.linkSystemLibrary("glfw");

    // Add our module to the list of imports to make vulkan-zig available.
    triangle_exe.root_module.addImport("vulkan", example_vk_zig_module);

    const shaders = ShaderCompileStep.create(
        b,
        &[_][]const u8{ "glslc", "--target-env=vulkan1.2" },
        "-o",
    );
    shaders.add("triangle_vert", "examples/shaders/triangle.vert", .{});
    shaders.add("triangle_frag", "examples/shaders/triangle.frag", .{});
    triangle_exe.root_module.addImport("shaders", shaders.getModule());

    const triangle_run_cmd = b.addRunArtifact(triangle_exe);
    triangle_run_cmd.step.dependOn(b.getInstallStep());

    const triangle_run_step = b.step("run-triangle", "Run the triangle example");
    triangle_run_step.dependOn(&triangle_run_cmd.step);

    // Remainder is for local testing.

    const example_vk_zig_install_step = b.addInstallFile(example_vk_zig, "src/vk.zig");
    b.getInstallStep().dependOn(&example_vk_zig_install_step.step);

    const test_target = b.addTest(.{
        .root_source_file = .{ .path = "src/index.zig" },
    });

    const run_test = b.addRunArtifact(test_target);

    const test_step = b.step("test", "Run all the tests");
    test_step.dependOn(&run_test.step);

    // This test needs to be an object so that vulkan-zig can import types from the root.
    // It does not need to run anyway.
    const ref_all_decls_test = b.addObject(.{
        .name = "ref-all-decls-test",
        .root_source_file = .{ .path = "test/ref_all_decls.zig" },
        .target = target,
        .optimize = optimize,
    });
    ref_all_decls_test.root_module.addImport("vulkan", example_vk_zig_module);
    test_step.dependOn(&ref_all_decls_test.step);
}
