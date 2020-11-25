const std = @import("std");
const generate = @import("vulkan/generator.zig").generate;

const usage = "Usage: {} [-h|--help] <spec xml path> <output zig source>\n";

pub fn main() !void {
    const stderr = std.io.getStdErr();
    const stdout = std.io.getStdOut();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    var args = std.process.args();
    const prog_name = try args.next(allocator) orelse return error.ExecutableNameMissing;

    var maybe_xml_path: ?[]const u8 = null;
    var maybe_out_path: ?[]const u8 = null;

    while (args.next(allocator)) |err_or_arg| {
        const arg = try err_or_arg;

        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            @setEvalBranchQuota(2000);
            try stderr.writer().print(
                \\Utility to generate a Zig binding from the Vulkan XML API registry.
                \\
                \\The most recent Vulkan XML API registry can be obtained from
                \\https://github.com/KhronosGroup/Vulkan-Docs/blob/master/xml/vk.xml,
                \\and the most recent LunarG Vulkan SDK version can be found at
                \\$VULKAN_SDK/x86_64/share/vulkan/registry/vk.xml.
                \\
                \\
                ++ usage,
                .{ prog_name },
            );
            return;
        } else if (maybe_xml_path == null) {
            maybe_xml_path = arg;
        } else if (maybe_out_path == null) {
            maybe_out_path = arg;
        } else {
            try stderr.writer().print("Error: Superficial argument '{}'\n", .{ arg });
        }
    }

    const xml_path = maybe_xml_path orelse {
        try stderr.writer().print("Error: Missing required argument <spec xml path>\n" ++ usage, .{ prog_name });
        return;
    };

    const out_path = maybe_out_path orelse {
        try stderr.writer().print("Error: Missing required argument <output zig source>\n" ++ usage, .{ prog_name });
        return;
    };

    const cwd = std.fs.cwd();
    const xml_src = cwd.readFileAlloc(allocator, xml_path, std.math.maxInt(usize)) catch |err| {
        try stderr.writer().print("Error: Failed to open input file '{}' ({})\n", .{ xml_path, @errorName(err) });
        return;
    };

    const out_file = cwd.createFile(out_path, .{}) catch |err| {
        try stderr.writer().print("Error: Failed to create output file '{}' ({})\n", .{ out_path, @errorName(err) });
        return;
    };
    defer out_file.close();

    var out_buffer = std.ArrayList(u8).init(allocator);
    try generate(allocator, xml_src, out_buffer.writer());
    const tree = try std.zig.parse(allocator, out_buffer.items);

    _ = try std.zig.render(allocator, out_file.writer(), tree);
}
