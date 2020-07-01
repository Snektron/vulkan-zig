const std = @import("std");
const Builder = std.build.Builder;
const FmtStep = std.build.FmtStep;
const vkgen = @import("../generator/generate.zig");

pub fn generateVk(b: *Builder) []const u8 {
    const spec = std.fs.cwd().readFileAlloc(b.allocator, "vk.xml", std.math.maxInt(usize)) catch unreachable;
    const output = std.fs.path.join(
        b.allocator,
        &[_][]const u8{b.cache_root, "vk.zig"},
    ) catch unreachable;

    const output_file = std.fs.cwd().openFile(output, .{}) catch unreachable;
    defer output_file.close();
    vkgen.generate(b.allocator, spec, output_file.writer());

    return output;
}

pub fn build(b: *Builder) void {
    const target = b.standardTargetOptions(.{});

    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("example", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const vk_path = generateVk(b);
    const fmt_step = b.addFmt(&[_][]const u8{vk_path});
    exe.step.dependOn(&fmt_step.step);

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
