const std = @import("std");
const path = std.fs.path;
const Builder = std.build.Builder;
const Step = std.build.Step;

/// Utility functionality to help with compiling shaders from build.zig.
/// Invokes glslc (or another shader compiler passed to `init`) for each shader
/// added via `addShader`.
pub const ShaderCompileStep = struct {
    /// Structure representing a shader to be compiled.
    const Shader = struct {
        /// The path to the shader, relative to the current build root.
        source_path: []const u8,

        /// The full output path where the compiled shader binary is placed.
        full_out_path: []const u8,
    };

    step: Step,
    builder: *Builder,

    /// The command and optional arguments used to invoke the shader compiler.
    glslc_cmd: []const []const u8,

    /// The directory within `zig-cache/` that the compiled shaders are placed in.
    output_dir: []const u8,

    /// List of shaders that are to be compiled.
    shaders: std.ArrayList(Shader),

    /// Create a ShaderCompilerStep for `builder`. When this step is invoked by the build
    /// system, `<glcl_cmd...> <shader_source> -o <dst_addr>` is invoked for each shader.
    pub fn init(builder: *Builder, glslc_cmd: []const []const u8, output_dir: []const u8) *ShaderCompileStep {
        const self = builder.allocator.create(ShaderCompileStep) catch unreachable;
        self.* = .{
            .step = Step.init(.custom, "shader-compile", builder.allocator, make),
            .builder = builder,
            .output_dir = output_dir,
            .glslc_cmd = builder.dupeStrings(glslc_cmd),
            .shaders = std.ArrayList(Shader).init(builder.allocator),
        };
        return self;
    }

    /// Add a shader to be compiled. `src` is shader source path, relative to the project root.
    /// Returns the full path where the compiled binary will be stored upon successful compilation.
    /// This path can then be used to include the binary into an executable, for example by passing it
    /// to @embedFile via an additional generated file.
    pub fn add(self: *ShaderCompileStep, src: []const u8) []const u8 {
        const output_filename = std.fmt.allocPrint(self.builder.allocator, "{s}.spv", .{ src }) catch unreachable;
        const full_out_path = path.join(self.builder.allocator, &[_][]const u8{
            self.builder.build_root,
            self.builder.cache_root,
            self.output_dir,
            output_filename,
        }) catch unreachable;
        self.shaders.append(.{ .source_path = src, .full_out_path = full_out_path }) catch unreachable;
        return full_out_path;
    }

    /// Internal build function.
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
