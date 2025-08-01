const std = @import("std");

const vkgen = @import("vulkan_zig");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const maybe_override_registry = b.option([]const u8, "override-registry", "Override the path to the Vulkan registry used for the examples");
    const use_zig_shaders = b.option(bool, "zig-shader", "Use Zig shaders instead of GLSL") orelse false;

    const registry = b.dependency("vulkan_headers", .{}).path("registry/vk.xml");

    const triangle_exe = b.addExecutable(.{
        .name = "triangle",
        .root_module = b.createModule(.{
            .root_source_file = b.path("triangle.zig"),
            .target = target,
            .link_libc = true,
            .optimize = optimize,
        }),
        // TODO: Remove this once x86_64 is stable
        .use_llvm = true,
    });
    b.installArtifact(triangle_exe);
    triangle_exe.linkSystemLibrary("glfw");

    const registry_path: std.Build.LazyPath = if (maybe_override_registry) |override_registry|
        .{ .cwd_relative = override_registry }
    else
        registry;

    const vulkan = b.dependency("vulkan_zig", .{
        .registry = registry_path,
    }).module("vulkan-zig");

    triangle_exe.root_module.addImport("vulkan", vulkan);

    if (use_zig_shaders) {
        const spirv_target = b.resolveTargetQuery(.{
            .cpu_arch = .spirv32,
            .os_tag = .vulkan,
            .cpu_model = .{ .explicit = &std.Target.spirv.cpu.vulkan_v1_2 },
            .ofmt = .spirv,
        });

        const vert_spv = b.addObject(.{
            .name = "vertex_shader",
            .root_module = b.createModule(.{
                .root_source_file = b.path("shaders/vertex.zig"),
                .target = spirv_target,
            }),
            .use_llvm = false,
        });
        triangle_exe.root_module.addAnonymousImport(
            "vertex_shader",
            .{ .root_source_file = vert_spv.getEmittedBin() },
        );

        const frag_spv = b.addObject(.{
            .name = "fragment_shader",
            .root_module = b.createModule(.{
                .root_source_file = b.path("shaders/fragment.zig"),
                .target = spirv_target,
            }),
            .use_llvm = false,
        });
        triangle_exe.root_module.addAnonymousImport(
            "fragment_shader",
            .{ .root_source_file = frag_spv.getEmittedBin() },
        );
    } else {
        const vert_cmd = b.addSystemCommand(&.{
            "glslc",
            "--target-env=vulkan1.2",
            "-o",
        });
        const vert_spv = vert_cmd.addOutputFileArg("vert.spv");
        vert_cmd.addFileArg(b.path("shaders/triangle.vert"));
        triangle_exe.root_module.addAnonymousImport("vertex_shader", .{
            .root_source_file = vert_spv,
        });

        const frag_cmd = b.addSystemCommand(&.{
            "glslc",
            "--target-env=vulkan1.2",
            "-o",
        });
        const frag_spv = frag_cmd.addOutputFileArg("frag.spv");
        frag_cmd.addFileArg(b.path("shaders/triangle.frag"));
        triangle_exe.root_module.addAnonymousImport("fragment_shader", .{
            .root_source_file = frag_spv,
        });
    }

    const triangle_run_cmd = b.addRunArtifact(triangle_exe);
    triangle_run_cmd.step.dependOn(b.getInstallStep());

    const triangle_run_step = b.step("run-triangle", "Run the triangle example");
    triangle_run_step.dependOn(&triangle_run_cmd.step);
}
