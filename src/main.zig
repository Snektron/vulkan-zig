const std = @import("std");

const generator = @import("vulkan/generator.zig");

fn invalidUsage(prog_name: []const u8, comptime fmt: []const u8, args: anytype) noreturn {
    std.log.err(fmt, args);
    std.log.err("see {s} --help for usage", .{prog_name});
    std.process.exit(1);
}

fn reportParseErrors(tree: std.zig.Ast) !void {
    var buf: [1024]u8 = undefined;
    var stderr = std.fs.File.stderr().writer(&buf);
    const w = &stderr.interface;
    for (tree.errors) |err| {
        const loc = tree.tokenLocation(0, err.token);
        try w.print("(vulkan-zig error):{}:{}: error: ", .{ loc.line + 1, loc.column + 1 });
        try tree.renderError(err, w);
        try w.print("\n{s}\n", .{tree.source[loc.line_start..loc.line_end]});
        for (0..loc.column) |_| {
            try w.writeAll(" ");
        }
        try w.writeAll("^\n");
    }
}

fn oomPanic() noreturn {
    @panic("Out of memory");
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var args = std.process.argsWithAllocator(allocator) catch |err| switch (err) {
        error.OutOfMemory => oomPanic(),
    };
    const prog_name = args.next() orelse "vulkan-zig-generator";

    var maybe_xml_path: ?[]const u8 = null;
    var maybe_out_path: ?[]const u8 = null;
    var maybe_video_xml_path: ?[]const u8 = null;
    var debug: bool = false;
    var api = generator.Api.vulkan;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            @setEvalBranchQuota(2000);
            var buf: [1024]u8 = undefined;
            var w = std.fs.File.stdout().writer(&buf);
            w.interface.print(
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
                \\--debug          Write out unformatted source if does not parse correctly.
                \\--video <path>   Also gnerate Vulkan Video API bindings from video.xml
                \\                 registry at <path>.
                \\
            ,
                .{prog_name},
            ) catch |err| {
                std.process.fatal("failed to write to stdout: {s}", .{@errorName(err)});
            };
            return;
        } else if (std.mem.eql(u8, arg, "-a") or std.mem.eql(u8, arg, "--api")) {
            const api_str = args.next() orelse {
                invalidUsage(prog_name, "{s} expects argument <api>", .{arg});
            };
            api = std.meta.stringToEnum(generator.Api, api_str) orelse {
                invalidUsage(prog_name, "invalid api '{s}'", .{api_str});
            };
        } else if (std.mem.eql(u8, arg, "--debug")) {
            debug = true;
        } else if (std.mem.eql(u8, arg, "--video")) {
            maybe_video_xml_path = args.next() orelse {
                invalidUsage(prog_name, "{s} expects argument <path>", .{arg});
            };
        } else if (maybe_xml_path == null) {
            maybe_xml_path = arg;
        } else if (maybe_out_path == null) {
            maybe_out_path = arg;
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
        std.process.fatal("failed to open input file '{s}' ({s})", .{ xml_path, @errorName(err) });
    };

    const maybe_video_xml_src = if (maybe_video_xml_path) |video_xml_path|
        cwd.readFileAlloc(allocator, video_xml_path, std.math.maxInt(usize)) catch |err| {
            std.process.fatal("failed to open input file '{s}' ({s})", .{ video_xml_path, @errorName(err) });
        }
    else
        null;

    var aw: std.Io.Writer.Allocating = .init(allocator);
    generator.generate(allocator, api, xml_src, maybe_video_xml_src, &aw.writer) catch |err| {
        if (debug) {
            return err;
        }

        switch (err) {
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
            error.OutOfMemory, error.WriteFailed => oomPanic(),
        }
    };

    aw.writer.writeByte(0) catch oomPanic();

    const buffered = aw.writer.buffered();
    const src = buffered[0 .. buffered.len - 1 :0];
    const tree = std.zig.Ast.parse(allocator, src, .zig) catch |err| switch (err) {
        error.OutOfMemory => oomPanic(),
    };

    const formatted = if (tree.errors.len > 0) blk: {
        std.log.err("generated invalid zig code", .{});
        std.log.err("this is a bug in vulkan-zig", .{});
        std.log.err("please make a bug report at https://github.com/Snektron/vulkan-zig/issues/", .{});
        std.log.err("or run with --debug to write out unformatted source", .{});

        reportParseErrors(tree) catch |err| {
            std.process.fatal("failed to dump ast errors: {s}", .{@errorName(err)});
        };

        if (debug) {
            break :blk src;
        }
        std.process.exit(1);
    } else tree.renderAlloc(allocator) catch |err| switch (err) {
        error.OutOfMemory => oomPanic(),
    };

    if (std.fs.path.dirname(out_path)) |dir| {
        cwd.makePath(dir) catch |err| {
            std.process.fatal("failed to create output directory '{s}' ({s})", .{ dir, @errorName(err) });
        };
    }

    cwd.writeFile(.{
        .sub_path = out_path,
        .data = formatted,
    }) catch |err| {
        std.process.fatal("failed to write to output file '{s}' ({s})", .{ out_path, @errorName(err) });
    };
}

test "main" {
    _ = @import("xml.zig");
    _ = @import("vulkan/c_parse.zig");
}
