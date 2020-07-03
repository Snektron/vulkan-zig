const std = @import("std");
const vkgen = @import("generator/index.zig");
const Builder = std.build.Builder;
const FmtStep = std.build.FmtStep;

pub fn generateVk(b: *Builder) []const u8 {
    const spec = std.fs.cwd().readFileAlloc(b.allocator, "examples/vk.xml", std.math.maxInt(usize)) catch unreachable;
    const output = std.fs.path.join(
        b.allocator,
        &[_][]const u8{b.cache_root, "vk.zig"},
    ) catch unreachable;

    const output_file = std.fs.cwd().createFile(output, .{}) catch unreachable;
    defer output_file.close();
    vkgen.generateVk(b.allocator, spec, output_file.writer()) catch unreachable;

    return output;
}

pub fn build(b: *Builder) void {
    var test_step = b.step("test", "Run all the tests");
    test_step.dependOn(&b.addTest("generator/index.zig").step);

    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();
    const example_exe = b.addExecutable("example", "examples/main.zig");
    example_exe.setTarget(target);
    example_exe.setBuildMode(mode);
    example_exe.install();
    example_exe.linkSystemLibrary("c");
    example_exe.linkSystemLibrary("glfw");

    const vk_path = generateVk(b);
    const fmt_step = b.addFmt(&[_][]const u8{vk_path});
    example_exe.step.dependOn(&fmt_step.step);
    example_exe.addPackagePath("vulkan", vk_path);

    const example_run_cmd = example_exe.run();
    example_run_cmd.step.dependOn(b.getInstallStep());
    const example_run_step = b.step("run-example", "Run the example");
    example_run_step.dependOn(&example_run_cmd.step);
}
