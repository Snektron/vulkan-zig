const std = @import("std");
const generate = @import("generator.zig").generate;
const path = std.fs.path;
const Builder = std.build.Builder;
const Step = std.build.Step;

/// build.zig integration for Vulkan binding generation. This step can be used to generate
/// Vulkan bindings at compiletime from vk.xml, by providing the path to vk.xml and the output
/// path relative to zig-cache. The final file can then be added to the project using
/// `std.build.Builder.addPackagePath`.
pub const GenerateStep = struct {
    step: Step,
    builder: *Builder,

    /// The path to vk.xml
    spec_path: []const u8,

    /// The full path where the final generated binding will be placed. When using this step,
    /// this path should be passed to addPackagePath.
    full_out_path: []const u8,

    /// Initialize a Vulkan generation step, for `builder`. `spec_path` is the path to
    /// vk.xml, relative to the project root. The generated bindings will be placed at
    /// `out_path`, which is relative to the zig-cache directory.
    pub fn init(builder: *Builder, spec_path: []const u8, out_path: []const u8) *GenerateStep {
        const self = builder.allocator.create(GenerateStep) catch unreachable;
        self.* = .{
            .step = Step.init(.Custom, "vulkan-generate", builder.allocator, make),
            .builder = builder,
            .spec_path = spec_path,
            .full_out_path = path.join(builder.allocator, &[_][]const u8{
                self.builder.build_root,
                builder.cache_root,
                out_path,
            }) catch unreachable,
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

        const dir = path.dirname(self.full_out_path).?;
        try cwd.makePath(dir);
        const output_file = cwd.createFile(self.full_out_path, .{}) catch unreachable;
        defer output_file.close();
        _ = try std.zig.render(self.builder.allocator, output_file.outStream(), tree);
    }
};
