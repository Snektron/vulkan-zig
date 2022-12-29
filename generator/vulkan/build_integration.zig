const std = @import("std");
const generate = @import("generator.zig").generate;
const path = std.fs.path;
const Builder = std.build.Builder;
const Step = std.build.Step;

/// build.zig integration for Vulkan binding generation. This step can be used to generate
/// Vulkan bindings at compiletime from vk.xml, by providing the path to vk.xml and the output
/// path relative to zig-cache. The final package can then be obtained by `package()`, the result
/// of which can be added to the project using `std.build.Builder.addPackage`.
pub const GenerateStep = struct {
    step: Step,
    builder: *Builder,

    /// The path to vk.xml
    spec_path: []const u8,

    generated_file: std.build.GeneratedFile,

    /// Initialize a Vulkan generation step, for `builder`. `spec_path` is the path to
    /// vk.xml, relative to the project root. The generated bindings will be placed at
    /// `out_path`, which is relative to the zig-cache directory.
    pub fn create(builder: *Builder, spec_path: []const u8, out_path: []const u8) *GenerateStep {
        const self = builder.allocator.create(GenerateStep) catch unreachable;
        const full_out_path = path.join(builder.allocator, &[_][]const u8{
            builder.build_root,
            builder.cache_root,
            out_path,
        }) catch unreachable;

        self.* = .{
            .step = Step.init(.custom, "vulkan-generate", builder.allocator, make),
            .builder = builder,
            .spec_path = spec_path,
            .generated_file = .{
                .step = &self.step,
                .path = full_out_path,
            },
        };
        return self;
    }

    /// Initialize a Vulkan generation step for `builder`, by extracting vk.xml from the LunarG installation
    /// root. Typically, the location of the LunarG SDK root can be retrieved by querying for the VULKAN_SDK
    /// environment variable, set by activating the environment setup script located in the SDK root.
    /// `builder` and `out_path` are used in the same manner as `init`.
    pub fn createFromSdk(builder: *Builder, sdk_path: []const u8, out_path: []const u8) *GenerateStep {
        const spec_path = std.fs.path.join(
            builder.allocator,
            &[_][]const u8{ sdk_path, "share/vulkan/registry/vk.xml" },
        ) catch unreachable;

        return create(builder, spec_path, out_path);
    }

    /// Returns the package with the generated budings, with name `package_name`.
    pub fn getPackage(self: *GenerateStep, package_name: []const u8) std.build.Pkg {
        return .{ .name = package_name, .source = self.getSource() };
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

        const spec = try cwd.readFileAlloc(self.builder.allocator, self.spec_path, std.math.maxInt(usize));

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
        const tree = try std.zig.parse(self.builder.allocator, src);
        std.debug.assert(tree.errors.len == 0); // If this triggers, vulkan-zig produced invalid code.

        var formatted = try tree.render(self.builder.allocator);

        const dir = path.dirname(self.generated_file.path.?).?;
        try cwd.makePath(dir);
        try cwd.writeFile(self.generated_file.path.?, formatted);
    }
};
