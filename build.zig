const std = @import("std");
const Builder = std.build.Builder;

pub fn build(b: *Builder) void {
    const generator = b.addExecutable("vulkan-zig-gen", "generator/main.zig");
    generator.setBuildMode(b.standardReleaseOptions());

    var test_step = b.step("test", "Run all the tests");
    test_step.dependOn(&b.addTest("generator/main.zig").step);

    const run_cmd = generator.run();
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "");
    run_step.dependOn(&run_cmd.step);

    b.default_step.dependOn(&generator.step);
    b.installArtifact(generator);
}
