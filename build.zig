const std = @import("std");
const vkgen = @import("generator/index.zig");
const Step = std.build.Step;
const Builder = std.build.Builder;
const path = std.fs.path;

pub const ResourceGenStep = struct {
    step: Step,
    shader_step: *vkgen.ShaderCompileStep,
    builder: *Builder,
    full_out_path: []const u8,
    resources: std.ArrayList(u8),

    pub fn init(builder: *Builder, out: []const u8) *ResourceGenStep {
        const self = builder.allocator.create(ResourceGenStep) catch unreachable;
        self.* = .{
            .step = Step.init(.Custom, "resources", builder.allocator, make),
            .shader_step = vkgen.ShaderCompileStep.init(builder, &[_][]const u8{"glslc", "--target-env=vulkan1.2"}),
            .builder = builder,
            .full_out_path = path.join(builder.allocator, &[_][]const u8{
                self.builder.build_root,
                builder.cache_root,
                out,
            }) catch unreachable,
            .resources = std.ArrayList(u8).init(builder.allocator),
        };

        self.step.dependOn(&self.shader_step.step);
        return self;
    }

    pub fn addShader(self: *ResourceGenStep, name: []const u8, source: []const u8) void {
        self.resources.writer().print(
            "pub const {} = @embedFile(\"{}\");\n",
            .{name, self.shader_step.add(source)}
        ) catch unreachable;
    }

    fn make(step: *Step) !void {
        const self = @fieldParentPtr(ResourceGenStep, "step", step);
        const cwd = std.fs.cwd();

        const dir = path.dirname(self.full_out_path).?;
        try cwd.makePath(dir);
        try cwd.writeFile(self.full_out_path, self.resources.items);
    }
};

pub fn build(b: *Builder) void {
    var test_step = b.step("test", "Run all the tests");
    test_step.dependOn(&b.addTest("generator/index.zig").step);

    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();
    const triangle_exe = b.addExecutable("triangle", "examples/triangle.zig");
    triangle_exe.setTarget(target);
    triangle_exe.setBuildMode(mode);
    triangle_exe.install();
    triangle_exe.linkSystemLibrary("c");
    triangle_exe.linkSystemLibrary("glfw");

    const gen = vkgen.VkGenerateStep.init(b, "examples/vk.xml", "vk.zig");
    triangle_exe.step.dependOn(&gen.step);
    triangle_exe.addPackagePath("vulkan", gen.full_out_path);

    const res = ResourceGenStep.init(b, "resources.zig");
    res.addShader("triangle_vert", "examples/shaders/triangle.vert");
    res.addShader("triangle_frag", "examples/shaders/triangle.frag");
    triangle_exe.step.dependOn(&res.step);
    triangle_exe.addPackagePath("resources", res.full_out_path);

    const triangle_run_cmd = triangle_exe.run();
    triangle_run_cmd.step.dependOn(b.getInstallStep());
    const triangle_run_step = b.step("run-triangle", "Run the triangle example");
    triangle_run_step.dependOn(&triangle_run_cmd.step);
}
