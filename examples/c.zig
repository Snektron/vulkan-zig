const c = @cImport({
    @cDefine("GLFW_INCLUDE_NONE", {});
    @cInclude("GLFW/glfw3.h");
});

const vk = @import("vulkan");

// Re-export the GLFW things that we need
pub const GLFW_TRUE = c.GLFW_TRUE;
pub const GLFW_FALSE = c.GLFW_FALSE;
pub const GLFW_CLIENT_API = c.GLFW_CLIENT_API;
pub const GLFW_NO_API = c.GLFW_NO_API;

pub const GLFWwindow = c.GLFWwindow;

pub const glfwInit = c.glfwInit;
pub const glfwTerminate = c.glfwTerminate;
pub const glfwVulkanSupported = c.glfwVulkanSupported;
pub const glfwWindowHint = c.glfwWindowHint;
pub const glfwCreateWindow = c.glfwCreateWindow;
pub const glfwDestroyWindow = c.glfwDestroyWindow;
pub const glfwWindowShouldClose = c.glfwWindowShouldClose;
pub const glfwGetRequiredInstanceExtensions = c.glfwGetRequiredInstanceExtensions;
pub const glfwGetFramebufferSize = c.glfwGetFramebufferSize;
pub const glfwPollEvents = c.glfwPollEvents;

// usually the GLFW vulkan functions are exported if Vulkan is included,
// but since thats not the case here, they are manually imported.

pub extern fn glfwGetInstanceProcAddress(instance: vk.Instance, procname: [*:0]const u8) vk.PfnVoidFunction;
pub extern fn glfwGetPhysicalDevicePresentationSupport(instance: vk.Instance, pdev: vk.PhysicalDevice, queuefamily: u32) c_int;
pub extern fn glfwCreateWindowSurface(instance: vk.Instance, window: *GLFWwindow, allocation_callbacks: ?*const vk.AllocationCallbacks, surface: *vk.SurfaceKHR) vk.Result;
