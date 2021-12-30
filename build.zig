const std = @import("std");
const vkgen = @import("generator/index.zig");
const Step = std.build.Step;
const Builder = std.build.Builder;

pub const ResourceGenStep = struct {
    step: Step,
    shader_step: *vkgen.ShaderCompileStep,
    builder: *Builder,
    package: std.build.Pkg,
    output_file: std.build.GeneratedFile,
    resources: std.ArrayList(u8),

    pub fn init(builder: *Builder, out: []const u8) *ResourceGenStep {
        const self = builder.allocator.create(ResourceGenStep) catch unreachable;
        const full_out_path = std.fs.path.join(builder.allocator, &[_][]const u8{
            builder.build_root,
            builder.cache_root,
            out,
        }) catch unreachable;

        self.* = .{
            .step = Step.init(.custom, "resources", builder.allocator, make),
            .shader_step = vkgen.ShaderCompileStep.init(builder, &[_][]const u8{ "glslc", "--target-env=vulkan1.2" }, "shaders"),
            .builder = builder,
            .package = .{
                .name = "resources",
                .path = .{ .generated = &self.output_file },
                .dependencies = null,
            },
            .output_file = .{
                .step = &self.step,
                .path = full_out_path,
            },
            .resources = std.ArrayList(u8).init(builder.allocator),
        };

        self.step.dependOn(&self.shader_step.step);
        return self;
    }

    fn renderPath(path: []const u8, writer: anytype) void {
        const separators = &[_]u8{ std.fs.path.sep_windows, std.fs.path.sep_posix };
        var i: usize = 0;
        while (std.mem.indexOfAnyPos(u8, path, i, separators)) |j| {
            writer.writeAll(path[i..j]) catch unreachable;
            switch (std.fs.path.sep) {
                std.fs.path.sep_windows => writer.writeAll("\\\\") catch unreachable,
                std.fs.path.sep_posix => writer.writeByte(std.fs.path.sep_posix) catch unreachable,
                else => unreachable,
            }

            i = j + 1;
        }
        writer.writeAll(path[i..]) catch unreachable;
    }

    pub fn addShader(self: *ResourceGenStep, name: []const u8, source: []const u8) void {
        const shader_out_path = self.shader_step.add(source);
        var writer = self.resources.writer();

        writer.print("pub const {s} = @embedFile(\"", .{name}) catch unreachable;
        renderPath(shader_out_path, writer);
        writer.writeAll("\");\n") catch unreachable;
    }

    fn make(step: *Step) !void {
        const self = @fieldParentPtr(ResourceGenStep, "step", step);
        const cwd = std.fs.cwd();

        const dir = std.fs.path.dirname(self.output_file.path.?).?;
        try cwd.makePath(dir);
        try cwd.writeFile(self.output_file.path.?, self.resources.items);
    }
};

pub fn build(b: *Builder) void {
    var test_step = b.step("test", "Run all the tests");
    test_step.dependOn(&b.addTest("generator/index.zig").step);

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

    const gen = vkgen.VkGenerateStep.init(b, vk_xml_path, "vk.zig");
    triangle_exe.addPackage(gen.package);

    const res = ResourceGenStep.init(b, "resources.zig");
    res.addShader("triangle_vert", "examples/shaders/triangle.vert");
    res.addShader("triangle_frag", "examples/shaders/triangle.frag");
    triangle_exe.addPackage(res.package);

    const triangle_run_cmd = triangle_exe.run();
    triangle_run_cmd.step.dependOn(b.getInstallStep());
    const triangle_run_step = b.step("run-triangle", "Run the triangle example");
    triangle_run_step.dependOn(&triangle_run_cmd.step);
}
