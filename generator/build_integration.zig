const std = @import("std");
const path = std.fs.path;
const Builder = std.build.Builder;
const Step = std.build.Step;
const GeneratedFile = std.build.GeneratedFile;

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
    /// The directory within the zig-cache directory that is used to store
    /// shader artifacts.
    pub const cache_dir = "shaders";

    /// This structure contains additional options that can be passed to glslc when shaders are compiled.
    pub const ShaderOptions = struct {
        /// The entry point to use when compiling the shader.
        entry_point: ?[]const u8 = null,

        /// The stage to use when building. If not null, this is passed to
        /// the -fshader-stage argument.
        stage: ?ShaderStage = null,

        /// To ensure that if compilation options change, the shader is recompiled
        /// properly.
        fn hash(self: ShaderOptions, hasher: anytype) void {
            if (self.entry_point) |entry_point| {
                hasher.update(entry_point);
            }
            if (self.stage) |stage| {
                hasher.update(std.mem.asBytes(&@enumToInt(stage)));
            }
        }
    };

    /// Structure representing a shader to be compiled.
    const Shader = struct {
        /// The name of the shader in the generated file.
        /// Must be unique for all shaders added to this ShaderCompileStep.
        name: []const u8,

        /// The path to the shader, relative to the current build root.
        source_path: []const u8,

        /// Miscellaneous options to pass when compiling the shader.
        options: ShaderOptions,
    };

    step: Step,
    b: *Builder,

    /// The command and optional arguments used to invoke the shader compiler.
    glslc_cmd: []const []const u8,

    /// List of shaders that are to be compiled.
    shaders: std.ArrayList(Shader),

    /// The main Zig file that contains all the shaders. Each shader is included as
    /// `pub const ${name} align(@alignOf(u32))= @embedFile("${path").*;`
    generated_file: GeneratedFile,

    /// Create a ShaderCompileStep for `builder`. When this step is invoked by the build
    /// system, `<glcl_cmd...> <shader_source> -o <path>` is invoked for each shader.
    pub fn create(builder: *Builder, glslc_cmd: []const []const u8) *ShaderCompileStep {
        const self = builder.allocator.create(ShaderCompileStep) catch unreachable;
        self.* = .{
            .step = Step.init(.custom, "shaders", builder.allocator, make),
            .b = builder,
            .glslc_cmd = builder.dupeStrings(glslc_cmd),
            .shaders = std.ArrayList(Shader).init(builder.allocator),
            .generated_file = undefined,
        };
        self.generated_file = .{ .step = &self.step };
        return self;
    }

    /// Returns the shaders package with name `package_name`.
    pub fn getPackage(self: *ShaderCompileStep, package_name: []const u8) std.build.Pkg {
        return .{ .name = package_name, .source = self.getSource() };
    }

    /// Returns the file source for the generated shader resource code.
    pub fn getSource(self: *ShaderCompileStep) std.build.FileSource {
        return .{ .generated = &self.generated_file };
    }

    /// Add a shader to be compiled. `src` is shader source path, relative to the project root.
    /// Returns the full path where the compiled binary will be stored upon successful compilation.
    /// This path can then be used to include the binary into an executable, for example by passing it
    /// to @embedFile via an additional generated file. `entry_point` is the entry point to pass to the compiler.
    /// `stage` is an optional shader stage to pass to the compiler with the flag `-fshader-stage` when building the shader.
    pub fn add(self: *ShaderCompileStep, name: []const u8, src: []const u8, options: ShaderOptions) void {
        const full_source_path = std.fs.path.join(self.b.allocator, &.{
            self.b.build_root,
            src,
        }) catch unreachable;
        self.shaders.append(.{
            .name = name,
            .source_path = full_source_path,
            .options = options,
        }) catch unreachable;
    }

    /// Create a hash of a shader's source contents.
    fn hashShaderToFileName(self: *ShaderCompileStep, shader: Shader) ![64]u8 {
        const source = std.fs.cwd().readFileAlloc(
            self.b.allocator,
            shader.source_path,
            std.math.maxInt(usize),
        ) catch |err| switch (err) {
            error.FileNotFound => {
                std.log.err("could not open shader '{s}'", .{shader.source_path});
                return error.FileNotFound;
            },
            else => |e| return e,
        };

        var hasher = std.crypto.hash.blake2.Blake2b384.init(.{});
        // Random bytes to make ShaderCompileStep unique. Refresh with new random
        // bytes when the implementation is changed in a non-backwards-compatible way.
        hasher.update("Pw7Z*9Q8r!fLY8&!");
        // Make sure that there is no cache hit if the shader's source has changed.
        hasher.update(source);
        // Not only the shader source must be the same to ensure uniqueness -
        // the compilation options must be the same as well!
        shader.options.hash(&hasher);
        // And the compile command, too.
        for (self.glslc_cmd) |cmd| {
            hasher.update(cmd);
        }

        return digest(&hasher);
    }

    /// Create a base-64 hash digest from a hasher, which we can use as file name.
    fn digest(hasher: anytype) [64]u8 {
        var hash_digest: [48]u8 = undefined;
        hasher.final(&hash_digest);
        var hash: [64]u8 = undefined;
        _ = std.fs.base64_encoder.encode(&hash, &hash_digest);
        return hash;
    }

    /// Internal build function.
    fn make(step: *Step) !void {
        const self = @fieldParentPtr(ShaderCompileStep, "step", step);
        const cwd = std.fs.cwd();

        var cmd = std.ArrayList([]const u8).init(self.b.allocator);
        try cmd.appendSlice(self.glslc_cmd);
        const base_cmd_len = cmd.items.len;

        var shaders_file_contents = std.ArrayList(u8).init(self.b.allocator);
        const shaders_out = shaders_file_contents.writer();

        const shaders_dir = try std.fs.path.join(
            self.b.allocator,
            &.{ self.b.build_root, self.b.cache_root, cache_dir },
        );
        try cwd.makePath(shaders_dir);

        for (self.shaders.items) |shader| {
            const shader_basename = try self.hashShaderToFileName(shader);
            const shader_out_path = try std.fs.path.join(self.b.allocator, &.{
                shaders_dir,
                &shader_basename,
            });

            // This path must be relative to the shaders zig file - which is in the same directory
            try shaders_out.print("pub const {s} align(@alignOf(u32)) = @embedFile(\"{s}\").*;\n", .{
                shader.name,
                &shader_basename,
            });

            // If we have a cache hit, we can save some compile time by not invoking glslc.
            compile_shader: {
                std.fs.accessAbsolute(shader_out_path, .{}) catch |err| switch (err) {
                    error.FileNotFound => break :compile_shader,
                    else => |e| return e,
                };

                continue;
            }

            cmd.items.len = base_cmd_len;

            if (shader.options.entry_point) |entry_point| {
                try cmd.append(try std.fmt.allocPrint(self.b.allocator, "-fentry-point={s}", .{entry_point}));
            }

            if (shader.options.stage) |stage| {
                try cmd.append(try std.fmt.allocPrint(self.b.allocator, "-fshader-stage={s}", .{@tagName(stage)}));
            }

            try cmd.appendSlice(&.{ shader.source_path, "-o", shader_out_path });
            try self.b.spawnChild(cmd.items);
        }

        // Generate a file name for the shaders zig source based on the contents of shaders_file_contents.
        // In this case we don't need to omit writing the file - Zig does this check already for us.
        var hasher = std.crypto.hash.blake2.Blake2b384.init(.{});
        // Note: don't need to seed the hasher - it transitively contains the seed from
        // hashShaderToFileName. Change that if the implementation changes.
        hasher.update(shaders_file_contents.items);

        const shaders_path = try std.fs.path.join(
            self.b.allocator,
            &.{ shaders_dir, &digest(&hasher) },
        );

        try cwd.writeFile(shaders_path, shaders_file_contents.items);
        self.generated_file.path = shaders_path;
    }
};
