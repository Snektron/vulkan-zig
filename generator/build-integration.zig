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
    glslc_cmd: []const []const u8,
    shaders: std.ArrayList(Shader),

    pub fn init(builder: *Builder, glslc_cmd: []const []const u8) *ShaderCompileStep {
        const self = builder.allocator.create(ShaderCompileStep) catch unreachable;
        self.* = .{
            .step = Step.init(.Custom, "shader-compile", builder.allocator, make),
            .builder = builder,
            .glslc_cmd = glslc_cmd,
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

        const cmd = try self.builder.allocator.alloc([]const u8, self.glslc_cmd.len + 3);
        for (self.glslc_cmd) |part, i| {
            cmd[i] = part;
        }
        cmd[cmd.len - 2] = "-o";

        for (self.shaders.items) |shader| {
            const dir = path.dirname(shader.full_out_path).?;
            try cwd.makePath(dir);
            cmd[cmd.len - 3] = shader.source_path;
            cmd[cmd.len - 1] = shader.full_out_path;
            try self.builder.spawnChild(cmd);
        }
    }
};
