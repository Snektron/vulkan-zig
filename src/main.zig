const std = @import("std");

const generator = @import("vulkan/generator.zig");

fn invalidUsage(prog_name: []const u8, comptime fmt: []const u8, args: anytype) noreturn {
    std.log.err(fmt, args);
    std.log.err("see {s} --help for usage", .{prog_name});
    std.process.exit(1);
}

fn reportParseErrors(io: std.Io, tree: std.zig.Ast) !void {
    var buf: [1024]u8 = undefined;
    var stderr = std.Io.File.stderr().writer(io, &buf);
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

fn getFileContent(io: std.Io, cwd: std.Io.Dir, path: []const u8, allocator: std.mem.Allocator) []const u8 {
    if (std.mem.startsWith(u8, path, "http://") or std.mem.startsWith(u8, path, "https://")) {
        const uri = std.Uri.parse(path) catch |err|
            std.process.fatal("failed to parse url '{s}' ({s})", .{ path, @errorName(err) });
        var writer: std.Io.Writer.Allocating = .init(allocator);
        var client: std.http.Client = .{ .allocator = allocator, .io = io };
        defer client.deinit();
        const result = client.fetch(.{
            .keep_alive = false,
            .location = .{ .uri = uri },
            .method = .GET,
            .response_writer = &writer.writer,
        }) catch |err|
            std.process.fatal("failed to download input file '{s}' ({s})", .{ path, @errorName(err) });
        if (result.status != .ok) {
            std.process.fatal("failed to download input file '{s}' ({s})", .{ path, @tagName(result.status) });
        }
        return writer.toOwnedSlice() catch oomPanic();
    } else {
        return cwd.readFileAlloc(io, path, allocator, .unlimited) catch |err| {
            std.process.fatal("failed to open input file '{s}' ({s})", .{ path, @errorName(err) });
        };
    }
}

pub fn main(init: std.process.Init) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var threaded: std.Io.Threaded = .init(allocator, .{
        .environ = .empty,
    });
    defer threaded.deinit();
    const io = threaded.io();

    var args = init.minimal.args.iterateAllocator(allocator) catch |err| switch (err) {
        error.OutOfMemory => oomPanic(),
    };
    const prog_name = args.next() orelse "vulkan-zig-generator";
    const canonical_vk_xml = "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/refs/heads/master/xml/vk.xml";
    const canonical_video_xml = "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/refs/heads/main/xml/video.xml";

    var maybe_xml_path: ?[]const u8 = null;
    var maybe_out_path: ?[]const u8 = null;
    var maybe_video_xml_path: ?[]const u8 = null;
    var auto_pull: bool = false;
    var debug: bool = false;
    var api = generator.Api.vulkan;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            var buf: [1024]u8 = undefined;
            var w = std.Io.File.stdout().writer(io, &buf);
            (blk: {
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
                    \\--video <path>   Also generate Vulkan Video API bindings from video.xml
                    \\                 registry at <path>.
                    \\--auto_pull      Automatically pulls xml files from the canonical address.
                    \\
                ,
                    .{prog_name},
                ) catch |err| break :blk err;
                w.flush() catch |err| break :blk err;
            }) catch |err| {
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
        } else if (std.mem.eql(u8, arg, "--auto_pull")) {
            auto_pull = true;
        } else if (maybe_xml_path == null and !auto_pull) {
            maybe_xml_path = arg;
        } else if (maybe_out_path == null) {
            maybe_out_path = arg;
        } else {
            invalidUsage(prog_name, "unexpected argument '{s}'", .{arg});
        }
    }

    const xml_path = maybe_xml_path orelse if (auto_pull)
        canonical_vk_xml
    else
        invalidUsage(prog_name, "missing required argument <spec xml path>", .{});
    maybe_video_xml_path = maybe_video_xml_path orelse if (auto_pull)
        canonical_video_xml
    else
        null;

    const out_path = maybe_out_path orelse {
        invalidUsage(prog_name, "missing required argument <output zig source>", .{});
    };

    const cwd = std.Io.Dir.cwd();
    const xml_src = getFileContent(io, cwd, xml_path, allocator);
    const maybe_video_xml_src = if (maybe_video_xml_path) |video_xml_path|
        getFileContent(io, cwd, video_xml_path, allocator)
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

        reportParseErrors(io, tree) catch |err| {
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
        cwd.createDir(io, dir, .default_dir) catch |err| {
            if (err != error.PathAlreadyExists)
                std.process.fatal("failed to create output directory '{s}' ({s})", .{ dir, @errorName(err) });
        };
    }

    cwd.writeFile(io, .{
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
