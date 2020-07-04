const std = @import("std");
const vkgen = @import("generator/index.zig");
const Builder = std.build.Builder;
const path = std.fs.path;

const Resources = struct {
    builder: *Builder,
    wfs: *std.build.WriteFileStep,
    data: std.ArrayList(u8),

    fn init(builder: *Builder) *Resources {
        const res = builder.allocator.create(Resources) catch unreachable;
        res.* = .{
            .builder = builder,
            .wfs = builder.addWriteFiles(),
            .data = std.ArrayList(u8).init(builder.allocator),
        };
        return res;
    }

    fn addShader(self: *Resources, shader: []const u8) void {
        const spv_name = std.mem.join(
            self.builder.allocator,
            "",
            &[_][]const u8{shader, ".spv"},
        ) catch unreachable;
        const dst = path.join(
            self.builder.allocator,
            &[_][]const u8{self.builder.cache_root, "test.frag.spv"}
        ) catch unreachable;

        const src = path.join(
            self.builder.allocator,
            &[_][]const u8{self.builder.build_root, shader}
        ) catch unreachable;

        const compile_step = self.builder.addSystemCommand(&[_][]const u8{
            "glslc",
            "--target-env=vulkan1.2",
            src,
            "-o",
            dst
        });

        self.wfs.step.dependOn(&compile_step.step);

        const writer = self.data.writer();
        writer.print("const @\"{}\" = @embedFile(\"{}\");", .{src, dst}) catch unreachable;
    }

    fn finalize(self: *Resources) *std.build.WriteFileStep {
        self.wfs.add("resources.zig", self.data.toOwnedSlice());
        return self.wfs;
    }
};

pub fn build(b: *Builder) void {
    var test_step = b.step("test", "Run all the tests");
    test_step.dependOn(&b.addTest("generator/index.zig").step);

    const res = Resources.init(b);
    res.addShader("examples/shaders/test.frag");
    const wfs = res.finalize();

    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();
    const example_exe = b.addExecutable("example", "examples/main.zig");
    example_exe.setTarget(target);
    example_exe.setBuildMode(mode);
    example_exe.install();
    example_exe.linkSystemLibrary("c");
    example_exe.linkSystemLibrary("glfw");
    example_exe.step.dependOn(&wfs.step);

    const gen_step = vkgen.VkGenerateStep.init(b, "examples/vk.xml", "vk.zig");
    example_exe.step.dependOn(&gen_step.step);
    example_exe.addPackagePath("vulkan", gen_step.full_out_path);

    const example_run_cmd = example_exe.run();
    example_run_cmd.step.dependOn(b.getInstallStep());
    const example_run_step = b.step("run-example", "Run the example");
    example_run_step.dependOn(&example_run_cmd.step);
}
