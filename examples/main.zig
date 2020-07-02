const std = @import("std");
const vk = @import("vulkan");
const c = @import("c.zig");
const GraphicsContext = @import("graphics_context.zig").GraphicsContext;
const Swapchain = @import("swapchain.zig").Swapchain;

const app_name = "vulkan-zig example";
const app_info = vk.ApplicationInfo{
    .p_application_name = app_name,
    .application_version = vk.makeVersion(0, 0, 0),
    .p_engine_name = app_name,
    .engine_version = vk.makeVersion(0, 0, 0),
    .api_version = vk.API_VERSION_1_2,
};

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

    const gc = try GraphicsContext.init(std.heap.page_allocator, &app_info, window);
    defer gc.deinit();

    std.debug.print("Using device: {}\n", .{gc.deviceName()});

    var swapchain = try Swapchain.init(&gc, std.heap.page_allocator, extent);
    defer swapchain.deinit();

    const pool = try gc.vkd.createCommandPool(gc.dev, .{
        .flags = .{},
        .queue_family_index = gc.graphics_queue.family,
    }, null);
    defer gc.vkd.destroyCommandPool(gc.dev, pool, null);

    while (c.glfwWindowShouldClose(window) == c.GLFW_FALSE) {
        var cmdbuf: vk.CommandBuffer = undefined;
        try gc.vkd.allocateCommandBuffers(gc.dev, .{
            .command_pool = pool,
            .level = .primary,
            .command_buffer_count = 1,
        }, @ptrCast([*]vk.CommandBuffer, &cmdbuf));
        defer gc.vkd.freeCommandBuffers(gc.dev, pool, 1, @ptrCast([*]const vk.CommandBuffer, &cmdbuf));

        try gc.vkd.beginCommandBuffer(cmdbuf, .{
            .flags = .{.one_time_submit_bit = true},
            .p_inheritance_info = null,
        });

        const subresource_range = vk.ImageSubresourceRange{
            .aspect_mask = .{.color_bit = true},
            .base_mip_level = 0,
            .level_count = 1,
            .base_array_layer = 0,
            .layer_count = 1,
        };

        const color = vk.ClearColorValue{.float_32 = .{1, 0, 1, 1}};

        imageTransition(
            &gc,
            cmdbuf,
            swapchain.currentImage(),
            subresource_range,
            .{.layout = .@"undefined", .stage = .{.top_of_pipe_bit = true}},
            .{.layout = .general, .stage = .{.top_of_pipe_bit = true}},
        );

        gc.vkd.cmdClearColorImage(
            cmdbuf,
            swapchain.currentImage(),
            .general,
            color,
            1,
            @ptrCast([*]const vk.ImageSubresourceRange, &subresource_range),
        );

        imageTransition(
            &gc,
            cmdbuf,
            swapchain.currentImage(),
            subresource_range,
            .{.layout = .general, .stage = .{.top_of_pipe_bit = true}},
            .{.layout = .present_src_khr, .stage = .{.bottom_of_pipe_bit = true}},
        );

        try gc.vkd.endCommandBuffer(cmdbuf);
        const result = swapchain.present(cmdbuf);
        try gc.vkd.queueWaitIdle(gc.graphics_queue.handle);
        const state = result catch |err| switch (err) {
            error.OutOfDateKHR => Swapchain.PresentState.suboptimal,
            else => |narrow| return narrow,
        };

        if (state == .suboptimal) {
            var w: c_int = undefined;
            var h: c_int = undefined;
            c.glfwGetWindowSize(window, &w, &h);

            try swapchain.recreate(.{.width = @intCast(u32, w), .height = @intCast(u32, h)});
        }

        c.glfwSwapBuffers(window);
        c.glfwPollEvents();
    }
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
