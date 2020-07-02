const std = @import("std");
const vkgen = @import("generator/generator.zig");
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
    vkgen.generate(b.allocator, spec, output_file.writer()) catch unreachable;

    return output;
}

pub fn build(b: *Builder) void {
    var test_step = b.step("test", "Run all the tests");
    test_step.dependOn(&b.addTest("generator/generator.zig").step);

    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();
    const exe = b.addExecutable("example", "examples/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();
    exe.linkSystemLibrary("c");
    exe.linkSystemLibrary("glfw");

    const vk_path = generateVk(b);
    const fmt_step = b.addFmt(&[_][]const u8{vk_path});
    exe.step.dependOn(&fmt_step.step);
    exe.addPackagePath("vulkan", vk_path);
}
