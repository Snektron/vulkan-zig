const std = @import("std");
const generator = @import("generator.zig");
const Build = std.Build;

/// build.zig integration for Vulkan binding generation. This step can be used to generate
/// Vulkan bindings at compiletime from vk.xml, by providing the path to vk.xml and the output
/// path relative to zig-cache. The final package can then be obtained by `package()`, the result
/// of which can be added to the project using `std.Build.addModule`.
pub const GenerateStep = struct {
    step: Build.Step,
    generated_file: Build.GeneratedFile,
    /// The path to vk.xml
    spec_path: []const u8,
    /// The API to generate for.
    /// Defaults to Vulkan.
    // Note: VulkanSC is experimental.
    api: generator.Api = .vulkan,

    /// Initialize a Vulkan generation step, for `builder`. `spec_path` is the path to
    /// vk.xml, relative to the project root. The generated bindings will be placed at
    /// `out_path`, which is relative to the zig-cache directory.
    pub fn create(builder: *Build, spec_path: []const u8) *GenerateStep {
        const self = builder.allocator.create(GenerateStep) catch unreachable;
        self.* = .{
            .step = Build.Step.init(.{
                .id = .custom,
                .name = "vulkan-generate",
                .owner = builder,
                .makeFn = make,
            }),
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

    /// Set the API to generate for.
    pub fn setApi(self: *GenerateStep, api_to_generate: generator.Api) void {
        self.api = api_to_generate;
    }

    /// Returns the module with the generated budings, with name `module_name`.
    pub fn getModule(self: *GenerateStep) *Build.Module {
        return self.step.owner.createModule(.{
            .root_source_file = self.getSource(),
        });
    }

    /// Returns the file source for the generated bindings.
    pub fn getSource(self: *GenerateStep) Build.LazyPath {
        return .{ .generated = .{ .file = &self.generated_file } };
    }

    /// Internal build function. This reads `vk.xml`, and passes it to `generate`, which then generates
    /// the final bindings. The resulting generated bindings are not formatted, which is why an ArrayList
    /// writer is passed instead of a file writer. This is then formatted into standard formatting
    /// by parsing it and rendering with `std.zig.parse` and `std.zig.render` respectively.
    fn make(step: *Build.Step, progress: std.Progress.Node) !void {
        _ = progress;

        const b = step.owner;
        const self: *GenerateStep = @fieldParentPtr("step", step);
        const cwd = std.fs.cwd();

        var man = b.graph.cache.obtain();
        defer man.deinit();

        const spec = try cwd.readFileAlloc(b.allocator, self.spec_path, std.math.maxInt(usize));
        // TODO: Look into whether this is the right way to be doing
        // this - maybe the file-level caching API has some benefits I
        // don't understand.
        man.hash.addBytes(spec);

        const already_exists = try step.cacheHit(&man);
        const digest = man.final();
        const output_file_path = try b.cache_root.join(b.allocator, &.{ "o", &digest, "vk.zig" });
        if (already_exists) {
            self.generated_file.path = output_file_path;
            return;
        }

        var out_buffer = std.ArrayList(u8).init(b.allocator);
        generator.generate(b.allocator, self.api, spec, out_buffer.writer()) catch |err| switch (err) {
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
        const tree = try std.zig.Ast.parse(b.allocator, src, .zig);

        if (tree.errors.len > 0) {
            var start: usize = undefined;
            var end: usize = undefined;
            var index: usize = undefined;
            var repeat: usize = undefined;
            var spaces: []const u8 = undefined;
            var carets: []const u8 = undefined;
            var current_token: std.zig.Token = undefined;

            std.debug.print("{s}\n", .{
                src,
            });

            var tokens = try std.ArrayList(std.zig.Ast.Error).initCapacity(b.allocator, tree.errors.len);
            try tokens.appendSlice(tree.errors);

            std.mem.sort(std.zig.Ast.Error, tokens.items, {}, struct {
                pub fn desc(_: void, l_err: std.zig.Ast.Error, r_err: std.zig.Ast.Error) bool {
                    return l_err.token > r_err.token;
                }
            }.desc);

            var iterator = std.zig.Tokenizer.init(src);

            index = 1;
            current_token = iterator.next();

            while (current_token.tag != std.zig.Token.Tag.eof and tokens.items.len > 0) {
                if (tokens.items[tokens.items.len - 1].token == index) {
                    start = std.mem.lastIndexOf(u8, src[0..current_token.loc.start], "\n") orelse 0;
                    end = (std.mem.indexOf(u8, src[current_token.loc.end..], "\n") orelse src.len - current_token.loc.end) + current_token.loc.end;

                    repeat = 1;
                    spaces = "";
                    while (repeat < current_token.loc.start - start) {
                        spaces = try std.fmt.allocPrint(b.allocator, "{s} ", .{
                            spaces,
                        });
                        repeat += 1;
                    }

                    repeat = 1;
                    carets = "";
                    while (repeat < current_token.loc.end + 1 - current_token.loc.start) {
                        carets = try std.fmt.allocPrint(b.allocator, "{s}^", .{
                            carets,
                        });
                        repeat += 1;
                    }

                    std.debug.print("ERROR: {}\nTOKEN: {}\n\n{s}\n{s}{s}\n", .{
                        tokens.items[tokens.items.len - 1],
                        current_token,
                        if (src[start] == '\n') src[start + 1 .. end] else src[start..end],
                        spaces,
                        carets,
                    });

                    _ = tokens.pop();
                }

                current_token = iterator.next();
                index += 1;
            }
        }

        std.debug.assert(tree.errors.len == 0); // If this triggers, vulkan-zig produced invalid code.

        const formatted = try tree.render(b.allocator);

        const output_dir_path = std.fs.path.dirname(output_file_path).?;
        cwd.makePath(output_dir_path) catch |err| {
            std.debug.print("unable to make path {s}: {s}\n", .{ output_dir_path, @errorName(err) });
            return err;
        };

        try cwd.writeFile(.{ .sub_path = output_file_path, .data = formatted });
        self.generated_file.path = output_file_path;
        try step.writeManifest(&man);
    }
};
