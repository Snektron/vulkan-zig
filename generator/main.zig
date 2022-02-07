const std = @import("std");
const generate = @import("vulkan/generator.zig").generate;

const usage = "Usage: {s} [-h|--help] <spec xml path> <output zig source>\n";

pub fn main() !void {
    const stderr = std.io.getStdErr();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    const prog_name = args.next() orelse return error.ExecutableNameMissing;

    var maybe_xml_path: ?[]const u8 = null;
    var maybe_out_path: ?[]const u8 = null;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            @setEvalBranchQuota(2000);
            try stderr.writer().print(
                   "Utility to generate a Zig binding from the Vulkan XML API registry.\n"
                ++ "\n"
                ++ "The most recent Vulkan XML API registry can be obtained from\n"
                ++ "https://github.com/KhronosGroup/Vulkan-Docs/blob/master/xml/vk.xml,\n"
                ++ "and the most recent LunarG Vulkan SDK version can be found at\n"
                ++ "$VULKAN_SDK/x86_64/share/vulkan/registry/vk.xml.\n"
                ++ "\n"
                ++ usage,
                .{prog_name},
            );
            return;
        } else if (maybe_xml_path == null) {
            maybe_xml_path = arg;
        } else if (maybe_out_path == null) {
            maybe_out_path = arg;
        } else {
            try stderr.writer().print("Error: Superficial argument '{s}'\n", .{arg});
        }
    }

    const xml_path = maybe_xml_path orelse {
        try stderr.writer().print("Error: Missing required argument <spec xml path>\n" ++ usage, .{prog_name});
        return;
    };

    const out_path = maybe_out_path orelse {
        try stderr.writer().print("Error: Missing required argument <output zig source>\n" ++ usage, .{prog_name});
        return;
    };

    const cwd = std.fs.cwd();
    const xml_src = cwd.readFileAlloc(allocator, xml_path, std.math.maxInt(usize)) catch |err| {
        try stderr.writer().print("Error: Failed to open input file '{s}' ({s})\n", .{ xml_path, @errorName(err) });
        return;
    };

    var out_buffer = std.ArrayList(u8).init(allocator);
    try generate(allocator, xml_src, out_buffer.writer());
    try out_buffer.append(0);

    const src = out_buffer.items[0 .. out_buffer.items.len - 1 :0];
    const tree = try std.zig.parse(allocator, src);
    const formatted = try tree.render(allocator);
    defer allocator.free(formatted);

    if (std.fs.path.dirname(out_path)) |dir| {
        cwd.makePath(dir) catch |err| {
            try stderr.writer().print("Error: Failed to create output directory '{s}' ({s})\n", .{ dir, @errorName(err) });
            return;
        };
    }

    cwd.writeFile(out_path, formatted) catch |err| {
        try stderr.writer().print("Error: Failed to write to output file '{s}' ({s})\n", .{ out_path, @errorName(err) });
        return;
    };
}
