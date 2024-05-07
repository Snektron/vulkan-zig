const std = @import("std");
const generator = @import("vulkan/generator.zig");

pub const generateVk = generator.generate;
pub const VkGenerateStep = @import("vulkan/build_integration.zig").GenerateStep;
pub const ShaderCompileStep = @import("build_integration.zig").ShaderCompileStep;

fn invalidUsage(prog_name: []const u8, comptime fmt: []const u8, args: anytype) noreturn {
    std.log.err(fmt, args);
    std.log.err("see {s} --help for usage", .{prog_name});
    std.process.exit(1);
}

fn reportParseErrors(tree: std.zig.Ast) !void {
    const stderr = std.io.getStdErr().writer();

    for (tree.errors) |err| {
        const loc = tree.tokenLocation(0, err.token);
        try stderr.print("(vulkan-zig error):{}:{}: error: ", .{ loc.line + 1, loc.column + 1 });
        try tree.renderError(err, stderr);
        try stderr.print("\n{s}\n", .{tree.source[loc.line_start..loc.line_end]});
        for (0..loc.column) |_| {
            try stderr.writeAll(" ");
        }
        try stderr.writeAll("^\n");
    }
}

pub fn main() void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var args = std.process.argsWithAllocator(allocator) catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM"),
    };
    const prog_name = args.next() orelse "vulkan-zig-generator";

    var maybe_xml_path: ?[]const u8 = null;
    var maybe_out_path: ?[]const u8 = null;
    var debug: bool = false;
    var api = generator.Api.vulkan;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            @setEvalBranchQuota(2000);
            std.io.getStdOut().writer().print(
                \\Utility to generate a Zig binding from the Vulkan XML API registry.
                \\
                \\The most recent Vulkan XML API registry can be obtained from
                \\https://github.com/KhronosGroup/Vulkan-Docs/blob/master/xml/vk.xml,
                \\and the most recent LunarG Vulkan SDK version can be found at
                \\$VULKAN_SDK/x86_64/share/vulkan/registry/vk.xml.
                \\
                \\Usage: {s} [options] <spec xml path> <output zig source>
                \\Options:
                \\-h --help        show this message and exit.
                \\-a --api <api>   Generate API for 'vulkan' or 'vulkansc'. Defaults to 'vulkan'.
                \\--debug Write out unformatted source if does not parse correctly.
                \\
            ,
                .{prog_name},
            ) catch |err| {
                std.log.err("failed to write to stdout: {s}", .{@errorName(err)});
                std.process.exit(1);
            };
            return;
        } else if (std.mem.eql(u8, arg, "-a") or std.mem.eql(u8, arg, "--api")) {
            const api_str = args.next() orelse {
                invalidUsage(prog_name, "{s} expects argument <api>", .{arg});
            };
            api = std.meta.stringToEnum(generator.Api, api_str) orelse {
                invalidUsage(prog_name, "invalid api '{s}'", .{api_str});
            };
        } else if (maybe_xml_path == null) {
            maybe_xml_path = arg;
        } else if (maybe_out_path == null) {
            maybe_out_path = arg;
        } else if (std.mem.eql(u8, arg, "--debug")) {
            debug = true;
        } else {
            invalidUsage(prog_name, "superficial argument '{s}'", .{arg});
        }
    }

    const xml_path = maybe_xml_path orelse {
        invalidUsage(prog_name, "missing required argument <spec xml path>", .{});
    };

    const out_path = maybe_out_path orelse {
        invalidUsage(prog_name, "missing required argument <output zig source>", .{});
    };

    const cwd = std.fs.cwd();
    const xml_src = cwd.readFileAlloc(allocator, xml_path, std.math.maxInt(usize)) catch |err| {
        std.log.err("failed to open input file '{s}' ({s})", .{ xml_path, @errorName(err) });
        std.process.exit(1);
    };

    var out_buffer = std.ArrayList(u8).init(allocator);
    generator.generate(allocator, api, xml_src, out_buffer.writer()) catch |err| switch (err) {
        error.InvalidXml => {
            std.log.err("invalid vulkan registry - invalid xml", .{});
            std.log.err("please check that the correct vk.xml file is passed", .{});
            std.process.exit(1);
        },
        error.InvalidRegistry => {
            std.log.err("invalid vulkan registry - registry is valid xml but contents are invalid", .{});
            std.log.err("please check that the correct vk.xml file is passed", .{});
            std.process.exit(1);
        },
        error.UnhandledBitfieldStruct => {
            std.log.err("unhandled struct with bit fields detected in vk.xml", .{});
            std.log.err("this is a bug in vulkan-zig", .{});
            std.log.err("please make a bug report at https://github.com/Snektron/vulkan-zig/issues/", .{});
            std.process.exit(1);
        },
        error.OutOfMemory => @panic("oom"),
    };

    out_buffer.append(0) catch @panic("oom");

    const src = out_buffer.items[0 .. out_buffer.items.len - 1 :0];
    const tree = std.zig.Ast.parse(allocator, src, .zig) catch |err| switch (err) {
        error.OutOfMemory => @panic("oom"),
    };

    const formatted = if (tree.errors.len > 0) blk: {
        std.log.err("generated invalid zig code", .{});
        std.log.err("this is a bug in vulkan-zig", .{});
        std.log.err("please make a bug report at https://github.com/Snektron/vulkan-zig/issues/", .{});
        std.log.err("or run with --debug to write out unformatted source", .{});

        reportParseErrors(tree) catch |err| {
            std.log.err("failed to dump ast errors: {s}", .{@errorName(err)});
            std.process.exit(1);
        };

        if (debug) {
            break :blk src;
        }
        std.process.exit(1);
    } else tree.render(allocator) catch |err| switch (err) {
        error.OutOfMemory => @panic("oom"),
    };

    if (std.fs.path.dirname(out_path)) |dir| {
        cwd.makePath(dir) catch |err| {
            std.log.err("failed to create output directory '{s}' ({s})", .{ dir, @errorName(err) });
            std.process.exit(1);
        };
    }

    cwd.writeFile(.{
        .sub_path = out_path,
        .data = formatted,
    }) catch |err| {
        std.log.err("failed to write to output file '{s}' ({s})", .{ out_path, @errorName(err) });
        std.process.exit(1);
    };
}

test "main" {
    _ = @import("xml.zig");
    _ = @import("vulkan/c_parse.zig");
}
