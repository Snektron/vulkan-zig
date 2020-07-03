const std = @import("std");
const vk = @import("vulkan");
const c = @import("c.zig");
const GraphicsContext = @import("graphics_context.zig").GraphicsContext;
const Swapchain = @import("swapchain.zig").Swapchain;
const Allocator = std.mem.Allocator;

const app_name = "vulkan-zig example";

pub fn main() !void {
    if (c.glfwInit() != c.GLFW_TRUE) return error.GlfwInitFailed;
    defer c.glfwTerminate();

    const extent = vk.Extent2D{.width = 800, .height = 600};

    c.glfwWindowHint(c.GLFW_CLIENT_API, c.GLFW_NO_API);
    const window = c.glfwCreateWindow(
        extent.width,
        extent.height,
        app_name,
        null,
        null
    ) orelse return error.WindowInitFailed;
    defer c.glfwDestroyWindow(window);

    const allocator = std.heap.page_allocator;

    const gc = try GraphicsContext.init(allocator, app_name, window);
    defer gc.deinit();

    std.debug.print("Using device: {}\n", .{gc.deviceName()});

    var swapchain = try Swapchain.init(&gc, allocator, extent);
    defer swapchain.deinit();

    const pool = try gc.vkd.createCommandPool(gc.dev, .{
        .flags = .{},
        .queue_family_index = gc.graphics_queue.family,
    }, null);
    defer gc.vkd.destroyCommandPool(gc.dev, pool, null);

    var cmdbufs = try createCommandBuffers(&gc, pool, allocator, swapchain);
    defer destroyCommandBuffers(&gc, pool, allocator, cmdbufs);

    while (c.glfwWindowShouldClose(window) == c.GLFW_FALSE) {
        const cmdbuf = cmdbufs[swapchain.image_index];

        const state = swapchain.present(cmdbuf) catch |err| switch (err) {
            error.OutOfDateKHR => Swapchain.PresentState.suboptimal,
            else => |narrow| return narrow,
        };

        if (state == .suboptimal) {
            var w: c_int = undefined;
            var h: c_int = undefined;
            c.glfwGetWindowSize(window, &w, &h);

            try swapchain.recreate(.{.width = @intCast(u32, w), .height = @intCast(u32, h)});

            destroyCommandBuffers(&gc, pool, allocator, cmdbufs);
            cmdbufs = try createCommandBuffers(&gc, pool, allocator, swapchain);
        }

        c.glfwSwapBuffers(window);
        c.glfwPollEvents();

        try gc.vkd.queueWaitIdle(gc.graphics_queue.handle);
    }
}

fn createCommandBuffers(
    gc: *const GraphicsContext,
    pool: vk.CommandPool,
    allocator: *Allocator,
    swapchain: Swapchain,
) ![]vk.CommandBuffer {
    const cmdbufs = try allocator.alloc(vk.CommandBuffer, swapchain.swap_images.len);
    errdefer allocator.free(cmdbufs);

    try gc.vkd.allocateCommandBuffers(gc.dev, .{
        .command_pool = pool,
        .level = .primary,
        .command_buffer_count = @truncate(u32, cmdbufs.len),
    }, cmdbufs.ptr);
    errdefer gc.vkd.freeCommandBuffers(gc.dev, pool, @truncate(u32, cmdbufs.len), cmdbufs.ptr);

    const subresource_range = vk.ImageSubresourceRange{
        .aspect_mask = .{.color_bit = true},
        .base_mip_level = 0,
        .level_count = 1,
        .base_array_layer = 0,
        .layer_count = 1,
    };

    const color = vk.ClearColorValue{.float_32 = .{1, 0, 1, 1}};

    for (cmdbufs) |cmdbuf, i| {
        const image = swapchain.swap_images[i].image;

        try gc.vkd.beginCommandBuffer(cmdbuf, .{
            .flags = .{},
            .p_inheritance_info = null,
        });

        imageTransition(
            gc,
            cmdbuf,
            image,
            subresource_range,
            .{.layout = .@"undefined", .stage = .{.top_of_pipe_bit = true}},
            .{.layout = .general, .stage = .{.top_of_pipe_bit = true}},
        );

        gc.vkd.cmdClearColorImage(
            cmdbuf,
            image,
            .general,
            color,
            1,
            @ptrCast([*]const vk.ImageSubresourceRange, &subresource_range),
        );

        imageTransition(
            gc,
            cmdbuf,
            image,
            subresource_range,
            .{.layout = .general, .stage = .{.top_of_pipe_bit = true}},
            .{.layout = .present_src_khr, .stage = .{.bottom_of_pipe_bit = true}},
        );

        try gc.vkd.endCommandBuffer(cmdbuf);
    }

    return cmdbufs;
}

fn destroyCommandBuffers(gc: *const GraphicsContext, pool: vk.CommandPool, allocator: *Allocator, cmdbufs: []vk.CommandBuffer) void {
    gc.vkd.freeCommandBuffers(gc.dev, pool, @truncate(u32, cmdbufs.len), cmdbufs.ptr);
    allocator.free(cmdbufs);
}

const ImageState = struct {
    layout: vk.ImageLayout,
    stage: vk.PipelineStageFlags,
    access_mask: vk.AccessFlags = .{},
};

fn imageTransition(
    gc: *const GraphicsContext,
    cmdbuf: vk.CommandBuffer,
    image: vk.Image,
    subresource_range: vk.ImageSubresourceRange,
    src: ImageState,
    dst: ImageState
) void {
    const barrier = vk.ImageMemoryBarrier{
        .src_access_mask = src.access_mask,
        .dst_access_mask = dst.access_mask,
        .old_layout = src.layout,
        .new_layout = dst.layout,
        .src_queue_family_index = vk.QUEUE_FAMILY_IGNORED,
        .dst_queue_family_index = vk.QUEUE_FAMILY_IGNORED,
        .image = image,
        .subresource_range = subresource_range,
    };

    gc.vkd.cmdPipelineBarrier(
        cmdbuf, src.stage, dst.stage, .{},
        0, undefined,
        0, undefined,
        1, @ptrCast([*]const vk.ImageMemoryBarrier, &barrier)
    );
}
