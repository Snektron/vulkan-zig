const std = @import("std");
const generate = @import("generator.zig").generate;
const path = std.fs.path;
const Builder = std.build.Builder;
const Step = std.build.Step;

pub const GenerateStep = struct {
    step: Step,
    builder: *Builder,
    spec_path: []const u8,
    full_out_path: []const u8,

    // relative_out_path is relative to builder.cache_root
    pub fn init(builder: *Builder, spec_path: []const u8, relative_out_path: []const u8) *GenerateStep {
        const self = builder.allocator.create(GenerateStep) catch unreachable;
        self.* = .{
            .step = Step.init(.Custom, "vulkan-generate", builder.allocator, make),
            .builder = builder,
            .spec_path = spec_path,
            .full_out_path = path.join(builder.allocator, &[_][]const u8{
                builder.cache_root,
                relative_out_path,
            }) catch unreachable,
        };
        return self;
    }

    fn make(step: *Step) !void {
        const self = @fieldParentPtr(GenerateStep, "step", step);
        const cwd = std.fs.cwd();

        var out_buffer = std.ArrayList(u8).init(self.builder.allocator);
        const spec = try std.fs.cwd().readFileAlloc(self.builder.allocator, self.spec_path, std.math.maxInt(usize));
        try generate(self.builder.allocator, spec, out_buffer.writer());

        const tree = try std.zig.parse(self.builder.allocator, out_buffer.items);

        const dir = path.dirname(self.full_out_path).?;
        try std.fs.cwd().makePath(dir);
        const output_file = std.fs.cwd().createFile(self.full_out_path, .{}) catch unreachable;
        _ = try std.zig.render(self.builder.allocator, output_file.outStream(), tree);
    }
};
