const std = @import("std");
const path = std.fs.path;
const Builder = std.build.Builder;
const Step = std.build.Step;

/// Stage the shader should be built for. This is passed to the -fshader-stage
/// argument when invoking glslc.
pub const ShaderStage = enum {
    vertex,
    fragment,
    tesscontrol,
    tesseval,
    geometry,
    compute,
};

/// Utility functionality to help with compiling shaders from build.zig.
/// Invokes glslc (or another shader compiler passed to `init`) for each shader
/// added via `addShader`.
pub const ShaderCompileStep = struct {
    const AddFileParams = struct {
        entry_point: ?[]const u8 = null,
        stage: ?ShaderStage = null,
        output_filename: ?[]const u8 = null,
    };

    /// Structure representing a shader to be compiled.
    const Shader = struct {
        /// The path to the shader, relative to the current build root.
        source_path: []const u8,

        /// The full output path where the compiled shader binary is placed.
        full_out_path: []const u8,

        /// The entry point to use when compiling the shader.
        entry_point: ?[]const u8,

        /// The stage to use when building. If not null, this is passed to
        /// the -fshader-stage argument.
        stage: ?ShaderStage,
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
    /// to @embedFile via an additional generated file. `entry_point` is the entry point to pass to the compiler.
    /// `stage` is an optional shader stage to pass to the compiler with the flag `-fshader-stage` when building the shader.
    pub fn add(self: *ShaderCompileStep, src: []const u8, params: AddFileParams) []const u8 {
        const full_out_path = path.join(self.builder.allocator, &[_][]const u8{
            self.builder.build_root,
            self.builder.cache_root,
            if (params.output_filename) |out| out else std.fmt.allocPrint(self.builder.allocator, "{s}.spv", .{src}) catch unreachable,
        }) catch unreachable;
        self.shaders.append(.{ .source_path = src, .full_out_path = full_out_path, .entry_point = params.entry_point, .stage = params.stage }) catch unreachable;
        return full_out_path;
    }

    /// Internal build function.
    fn make(step: *Step) !void {
        const self = @fieldParentPtr(ShaderCompileStep, "step", step);
        const cwd = std.fs.cwd();

        var cmd = std.ArrayList([]const u8).init(self.builder.allocator);
        try cmd.appendSlice(self.glslc_cmd);
        const base_cmd_len = cmd.items.len;

        for (self.shaders.items) |shader| {
            cmd.items.len = base_cmd_len;

            if (shader.entry_point) |entry_point| {
                try cmd.append(try std.fmt.allocPrint(self.builder.allocator, "-fentry-point={s}", .{entry_point}));
            }

            if (shader.stage) |stage| {
                try cmd.append(try std.fmt.allocPrint(self.builder.allocator, "-fshader-stage={s}", .{@tagName(stage)}));
            }

            const dir = path.dirname(shader.full_out_path).?;
            try cwd.makePath(dir);

            try cmd.appendSlice(&.{ shader.source_path, "-o", shader.full_out_path });
            try self.builder.spawnChild(cmd.items);
        }
    }
};
