const std = @import("std");
const generate = @import("generator.zig").generate;
const path = std.fs.path;
const Build = std.Build;
const Step = Build.Step;

/// build.zig integration for Vulkan binding generation. This step can be used to generate
/// Vulkan bindings at compiletime from vk.xml, by providing the path to vk.xml and the output
/// path relative to zig-cache. The final package can then be obtained by `package()`, the result
/// of which can be added to the project using `std.Build.addModule`.
pub const GenerateStep = struct {
    step: Step,
    builder: *Build,
    generated_file: std.build.GeneratedFile,
    /// The path to vk.xml
    spec_path: []const u8,

    /// Initialize a Vulkan generation step, for `builder`. `spec_path` is the path to
    /// vk.xml, relative to the project root. The generated bindings will be placed at
    /// `out_path`, which is relative to the zig-cache directory.
    pub fn create(builder: *Build, spec_path: []const u8) *GenerateStep {
        const self = builder.allocator.create(GenerateStep) catch unreachable;
        self.* = .{
            .step = Step.init(.custom, "vulkan-generate", builder.allocator, make),
            .builder = builder,
            .generated_file = .{
                .step = &self.step,
            },
            .spec_path = spec_path,
        };
        return self;
    }

    /// Initialize a Vulkan generation step for `builder`, by extracting vk.xml from the LunarG installation
    /// root. Typically, the location of the LunarG SDK root can be retrieved by querying for the VULKAN_SDK
    /// environment variable, set by activating the environment setup script located in the SDK root.
    /// `builder` and `out_path` are used in the same manner as `init`.
    pub fn createFromSdk(builder: *Build, sdk_path: []const u8, output_name: []const u8) *GenerateStep {
        const spec_path = std.fs.path.join(
            builder.allocator,
            &[_][]const u8{ sdk_path, "share/vulkan/registry/vk.xml" },
        ) catch unreachable;

        return create(builder, spec_path, output_name);
    }

    /// Returns the module with the generated budings, with name `module_name`.
    pub fn getModule(self: *GenerateStep) *std.build.Module {
        return self.builder.createModule(.{
            .source_file = self.getSource(),
        });
    }

    /// Returns the file source for the generated bindings.
    pub fn getSource(self: *GenerateStep) std.build.FileSource {
        return .{ .generated = &self.generated_file };
    }

    /// Internal build function. This reads `vk.xml`, and passes it to `generate`, which then generates
    /// the final bindings. The resulting generated bindings are not formatted, which is why an ArrayList
    /// writer is passed instead of a file writer. This is then formatted into standard formatting
    /// by parsing it and rendering with `std.zig.parse` and `std.zig.render` respectively.
    fn make(step: *Step) !void {
        const self = @fieldParentPtr(GenerateStep, "step", step);
        const cwd = std.fs.cwd();

        var man = self.builder.cache.obtain();
        defer man.deinit();

        const spec = try cwd.readFileAlloc(self.builder.allocator, self.spec_path, std.math.maxInt(usize));
        // TODO: Look into whether this is the right way to be doing
        // this - maybe the file-level caching API has some benefits I
        // don't understand.
        man.hash.addBytes(spec);

        const already_exists = man.hit() catch |err| @panic(switch (err) {
            inline else => |e| "Cache error: " ++ @errorName(e),
        });
        const digest = man.final();
        const output_file_path = try self.builder.cache_root.join(
            self.builder.allocator,
            &.{ "o", &digest, "vk.zig" },
        );
        if (already_exists) {
            self.generated_file.path = output_file_path;
            return;
        }

        var out_buffer = std.ArrayList(u8).init(self.builder.allocator);
        generate(self.builder.allocator, spec, out_buffer.writer()) catch |err| switch (err) {
            error.InvalidXml => {
                std.log.err("invalid vulkan registry - invalid xml", .{});
                std.log.err("please check that the correct vk.xml file is passed", .{});
                return err;
            },
            error.InvalidRegistry => {
                std.log.err("invalid vulkan registry - registry is valid xml but contents are invalid", .{});
                std.log.err("please check that the correct vk.xml file is passed", .{});
                return err;
            },
            error.UnhandledBitfieldStruct => {
                std.log.err("unhandled struct with bit fields detected in vk.xml", .{});
                std.log.err("this is a bug in vulkan-zig", .{});
                std.log.err("please make a bug report at https://github.com/Snektron/vulkan-zig/issues/", .{});
                return err;
            },
            error.OutOfMemory => return error.OutOfMemory,
        };
        try out_buffer.append(0);

        const src = out_buffer.items[0 .. out_buffer.items.len - 1 :0];
        const tree = try std.zig.Ast.parse(self.builder.allocator, src, .zig);
        std.debug.assert(tree.errors.len == 0); // If this triggers, vulkan-zig produced invalid code.

        const formatted = try tree.render(self.builder.allocator);

        const output_dir_path = std.fs.path.dirname(output_file_path).?;
        cwd.makePath(output_dir_path) catch |err| {
            std.debug.print("unable to make path {s}: {s}\n", .{ output_dir_path, @errorName(err) });
            return err;
        };

        try cwd.writeFile(output_file_path, formatted);
        self.generated_file.path = output_file_path;
        try man.writeManifest();
    }
};
