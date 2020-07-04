const std = @import("std");
const path = std.fs.path;
const Builder = std.build.Builder;
const Step = std.build.Step;

pub const ShaderCompileStep = struct {
    const Shader = struct {
        source_path: []const u8,
        full_out_path: []const u8,
    };

    step: Step,
    builder: *Builder,
    glslc_path: []const u8,
    shaders: std.ArrayList(Shader),

    pub fn init(builder: *Builder, glslc_path: []const u8) *ShaderCompileStep {
        const self = builder.allocator.create(ShaderCompileStep) catch unreachable;
        self.* = .{
            .step = Step.init(.Custom, "shader-compile", builder.allocator, make),
            .builder = builder,
            .glslc_path = glslc_path,
            .shaders = std.ArrayList(Shader).init(builder.allocator),
        };
        return self;
    }

    pub fn add(self: *ShaderCompileStep, src: []const u8) []const u8 {
        const full_out_path = path.join(self.builder.allocator, &[_][]const u8{
            self.builder.build_root,
            self.builder.cache_root,
            "shaders",
            src,
        }) catch unreachable;
        self.shaders.append(.{.source_path = src, .full_out_path = full_out_path}) catch unreachable;
        return full_out_path;
    }

    fn make(step: *Step) !void {
        const self = @fieldParentPtr(ShaderCompileStep, "step", step);
        const cwd = std.fs.cwd();

        for (self.shaders.items) |shader| {
            const dir = path.dirname(shader.full_out_path).?;
            try cwd.makePath(dir);
            try self.builder.spawnChild(&[_][]const u8{
                self.glslc_path,
                shader.source_path,
                "-o",
                shader.full_out_path,
            });
        }
    }
};
