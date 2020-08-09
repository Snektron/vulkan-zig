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

    /// The package representing the generated bindings. The generated bindings will be placed
    /// in `package.path`. When using this step, this member should be passed to
    /// `std.build.Builder.addPackage`, which causes the bindings to become available under the
    /// name `vulkan`.
    package: std.build.Pkg,

    /// Initialize a Vulkan generation step, for `builder`. `spec_path` is the path to
    /// vk.xml, relative to the project root. The generated bindings will be placed at
    /// `out_path`, which is relative to the zig-cache directory.
    pub fn init(builder: *Builder, spec_path: []const u8, out_path: []const u8) *GenerateStep {
        const self = builder.allocator.create(GenerateStep) catch unreachable;
        const full_out_path = path.join(builder.allocator, &[_][]const u8{
            builder.build_root,
            builder.cache_root,
            out_path,
        }) catch unreachable;

        self.* = .{
            .step = Step.init(.Custom, "vulkan-generate", builder.allocator, make),
            .builder = builder,
            .spec_path = spec_path,
            .package = .{
                .name = "vulkan",
                .path = full_out_path,
                .dependencies = null,
            }
        };
        return self;
    }

    /// Internal build function. This reads `vk.xml`, and passes it to `generate`, which then generates
    /// the final bindings. The resulting generated bindings are not formatted, which is why an ArrayList
    /// writer is passed instead of a file writer. This is then formatted into standard formatting
    /// by parsing it and rendering with `std.zig.parse` and `std.zig.render` respectively.
    fn make(step: *Step) !void {
        const self = @fieldParentPtr(GenerateStep, "step", step);
        const cwd = std.fs.cwd();
        var out_buffer = std.ArrayList(u8).init(self.builder.allocator);
        const spec = try cwd.readFileAlloc(self.builder.allocator, self.spec_path, std.math.maxInt(usize));
        try generate(self.builder.allocator, spec, out_buffer.writer());

        const tree = try std.zig.parse(self.builder.allocator, out_buffer.items);

        const dir = path.dirname(self.package.path).?;
        try cwd.makePath(dir);
        const output_file = cwd.createFile(self.package.path, .{}) catch unreachable;
        defer output_file.close();
        _ = try std.zig.render(self.builder.allocator, output_file.outStream(), tree);
    }
};
