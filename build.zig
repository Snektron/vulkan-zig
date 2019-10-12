const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const exe = b.addExecutable("vulkan-zig-gen", "generator/main.zig");
    exe.setBuildMode(b.standardReleaseOptions());

    const run_cmd = exe.run();

    const run_step = b.step("run", "");
    run_step.dependOn(&run_cmd.step);

    b.default_step.dependOn(&exe.step);
    b.installArtifact(exe);
}
