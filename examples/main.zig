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

    const swapchain = try Swapchain.init(&gc, std.heap.page_allocator, extent);
    defer swapchain.deinit();

    while (c.glfwWindowShouldClose(window) == c.GLFW_FALSE) {
        c.glfwSwapBuffers(window);
        c.glfwPollEvents();
    }
}
