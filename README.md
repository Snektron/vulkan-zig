# vulkan-zig

A Vulkan binding generator for Zig.

[![Actions Status](https://github.com/Snektron/vulkan-zig/workflows/Build/badge.svg)](https://github.com/Snektron/vulkan-zig/actions)

## Overview

vulkan-zig attempts to provide a better experience to programming Vulkan applications in Zig, by providing features such as integration of vulkan errors with Zig's error system, function pointer loading, renaming fields to standard Zig style, better bitfield handling, turning out parameters into return values and more.

vulkan-zig is automatically tested daily against the latest vk.xml and zig, and supports vk.xml from version 1.x.163.

### Zig versions

vulkan-zig aims to be always compatible with the ever-changing Zig master branch (however, development may lag a few days behind). Sometimes, the Zig master branch breaks a bunch of functionality however, which may make the latest version vulkan-zig incompatible with older releases of Zig. Versions compatible with older versions of zig are marked with the tag `zig-<version>`.

## Features
### CLI-interface
A CLI-interface is provided to generate vk.zig from the [Vulkan XML registry](https://github.com/KhronosGroup/Vulkan-Docs/blob/master/xml), which is built by default when invoking `zig build` in the project root. To generate vk.zig, simply invoke the program as follows:
```
$ zig-cache/bin/vulkan-zig-generator path/to/vk.xml output/path/to/vk.zig
```
This reads the xml file, parses its contents, renders the Vulkan bindings, and formats file, before writing the result to the output path. While the intended usage of vulkan-zig is through direct generation from build.zig (see below), the CLI-interface can be used for one-off generation and vendoring the result.

### Generation from build.zig
Vulkan bindings can be generated from the Vulkan XML registry at compile time with build.zig, by using the provided Vulkan generation step:
```zig
const vkgen = @import("vulkan-zig/generator/index.zig");

pub fn build(b: *Builder) void {
    ...
    const exe = b.addExecutable("my-executable", "src/main.zig");

    // Create a step that generates vk.zig (stored in zig-cache) from the provided vulkan registry.
    const gen = vkgen.VkGenerateStep.init(b, "path/to/vk.xml", "vk.zig");
    exe.step.dependOn(&gen.step);

    // Add the generated file as package to the final executable
    exe.addPackagePath("vulkan", gen.full_out_path);
}
```
This reads vk.xml, parses its contents, and renders the Vulkan bindings to "vk.zig", which is then formatted and placed in `zig-cache`. The resulting file can then be added to an executable by using `addPackagePath`.

### Function & field renaming
Functions and fields are renamed to be more or less in line with [Zig's standard library style](https://ziglang.org/documentation/master/#Style-Guide):
* The vk prefix is removed everywhere
  * Structs like `VkInstanceCreateInfo` are renamed to `InstanceCreateInfo`.
  * Handles like `VkSwapchainKHR` are renamed to `SwapchainKHR` (note that the tag is retained in caps).
  * Functions like `vkCreateInstance` are generated as `createInstance` as wrapper and as `PfnCreateInstance` as function pointer.
  * API constants like `VK_WHOLE_SIZE` retain screaming snake case, and are generates as `WHOLE_SIZE`.
* The type name is stripped from enumeration fields and bitflags, and they are generated in (lower) snake case. For example, `VK_IMAGE_LAYOUT_GENERAL` is generated as just `general`. Note that author tags are also generated to lower case: `VK_SURFACE_TRANSFORM_FLAGS_IDENTITY_BIT_KHR` is translated to `identity_bit_khr`.
* Container fields and function parameter names are generated in (lower) snake case in a similar manner: `ppEnabledLayerNames` becomes `pp_enabled_layer_names`.
* Any name which is either an illegal Zig name or a reserved identifier is rendered using `@"name"` syntax. For example, `VK_IMAGE_TYPE_2D` is translated to `@"2d"`.

### Function pointers & Wrappers
vulkan-zig provides no integration for statically linking libvulkan, and these symbols are not generated at all. Instead, vulkan functions are to be loaded dynamically. For each Vulkan function, a function pointer type is generated using the exact parameters and return types as defined by the Vulkan specification:
```zig
pub const PfnCreateInstance = fn (
    p_create_info: *const InstanceCreateInfo,
    p_allocator: ?*const AllocationCallbacks,
    p_instance: *Instance,
) callconv(vulkan_call_conv) Result;
```

For each function, a wrapper is generated into one of three structs:
* BaseWrapper. This contains wrappers for functions which are loaded by `vkGetInstanceProcAddr` without an instance, such as `vkCreateInstance`, `vkEnumerateInstanceVersion`, etc.
* InstanceWrapper. This contains wrappers for functions which are otherwise loaded by `vkGetInstanceProcAddr`.
* DeviceWrapper. This contains wrappers for functions which are loaded by `vkGetDeviceProcAddr`.

Each wrapper struct is to be used as a mixin on a struct containing **just** function pointers as members:
```zig
const vk = @import("vulkan");
const BaseDispatch = struct {
    vkCreateInstance: vk.PfnCreateInstance,
    usingnamespace vk.BaseWrapper(@This());
};
```
The wrapper struct then provides wrapper functions for each function pointer in the dispatch struct:
```zig
pub const BaseWrapper(comptime Self: type) type {
    return struct {
        pub fn createInstance(
            self: Self,
            create_info: InstanceCreateInfo,
            p_allocator: ?*const AllocationCallbacks,
        ) error{
            OutOfHostMemory,
            OutOfDeviceMemory,
            InitializationFailed,
            LayerNotPresent,
            ExtensionNotPresent,
            IncompatibleDriver,
            Unknown,
        }!Instance {
            var instance: Instance = undefined;
            const result = self.vkCreateInstance(
                &create_info,
                p_allocator,
                &instance,
            );
            switch (result) {
                .success => {},
                .error_out_of_host_memory => return error.OutOfHostMemory,
                .error_out_of_device_memory => return error.OutOfDeviceMemory,
                .error_initialization_failed => return error.InitializationFailed,
                .error_layer_not_present => return error.LayerNotPresent,
                .error_extension_not_present => return error.ExtensionNotPresent,
                .error_incompatible_driver => return error.IncompatibleDriver,
                else => return error.Unknown,
            }
            return instance;
        }

        ...
    }
}
```
Wrappers are generated according to the following rules:
* The return type is determined from the original return type and the parameters.
  * Any non-const, non-optional single-item pointer is interpreted as an out parameter.
  * If a command returns a non-error `VkResult` other than `VK_SUCCESS` it is also returned.
  * If there are multiple return values selected, an additional struct is generated. The original call's return value is called `return_value`, `VkResult` is named `result`, and the out parameters are called the same except `p_` is removed. They are generated in this order.
* Any const non-optional single-item pointer is interpreted as an in-parameter. For these, one level of indirection is removed so that create info structure pointers can now be passed as values, enabling the ability to use struct literals for these parameters.
* Error codes are translated into Zig errors.
* As of yet, there is no specific handling of enumeration style commands or other commands which accept slices.

Furthermore, each wrapper contains a function to load each function pointer member when passed either `PfnGetInstanceProcAddr` or `PfnGetDeviceProcAddr`, which attempts to load each member as function pointer and casts it to the appropriate type. These functions are loaded literally, and any wrongly named member or member with a wrong function pointer type will result in problems.
* For `BaseWrapper`, this function has signature `fn load(loader: PfnGetInstanceProcAddr) !Self`.
* For `InstanceWrapper`, this function has signature `fn load(instance: Instance, loader: PfnGetInstanceProcAddr) !Self`.
* For `DeviceWrapper`, this function has signature `fn load(device: Device, loader: PfnGetDeviceProcAddr) !Self`.

### Bitflags
Packed structs of bools are used for bit flags in vulkan-zig, instead of both a `FlagBits` and `Flags` variant. Places where either of these variants are used are both replaced by this packed struct instead. This means that even in places where just one flag would normally be accepted, the packed struct is accepted. The programmer is responsible for only enabling a single bit.

Each bit is defaulted to `false`, and the first `bool` is aligned to guarantee the overal alignment
of each Flags type to guarantee ABI compatibility when passing bitfields through structs:
```zig
pub const QueueFlags = packed struct {
    graphics_bit: bool align(@alignOf(Flags)) = false,
    compute_bit: bool = false,
    transfer_bit: bool = false,
    sparse_binding_bit: bool = false,
    protected_bit: bool = false,
    _reserved_bit_5: bool = false,
    _reserved_bit_6: bool = false,
    ...
}
```
Note that on function call ABI boundaries, this alignment trick is not sufficient. Instead, the flags
are reinterpreted as an integer which is passed instead. Each flags type is augmented by a mixin which provides `IntType`, an integer which represents the flags on function ABI boundaries. This mixin also provides some common set operation on bitflags:
```zig
pub fn FlagsMixin(comptime FlagsType: type) type {
    return struct {
        pub const IntType = Flags;

        // Return the integer representation of these flags
        pub fn toInt(self: FlagsType) IntType {...}

        // Turn an integer representation back into a flags type
        pub fn fromInt(flags: IntType) FlagsType { ... }

        // Return the set-union of `lhs` and `rhs.
        pub fn merge(lhs: FlagsType, rhs: FlagsType) FlagsType { ... }

        // Return the set-intersection of `lhs` and `rhs`.
        pub fn intersect(lhs: FlagsType, rhs: FlagsType) FlagsType { ... }

        // Return the set-complement of `lhs` and `rhs`. Note: this also inverses reserved bits.
        pub fn complement(self: FlagsType) FlagsType { ... }

        // Return the set-subtraction of `lhs` and `rhs`: All fields set in `rhs` are cleared in `lhs`.
        pub fn subtract(lhs: FlagsType, rhs: FlagsType) FlagsType { ... }

        // Returns whether all bits set in `rhs` are also set in `lhs`.
        pub fn contains(lhs: FlagsType, rhs: FlagsType) bool { ... }
    };
}
```

### Handles
Handles are generated to a non-exhaustive enum, backed by a `u64` for non-dispatchable handles and `usize` for dispatchable ones:
```zig
const Instance = extern enum(usize) { null_handle = 0, _ };
```
This means that handles are type-safe even when compiling for a 32-bit target.

### Struct defaults
Defaults are generated for certain fields of structs:
* sType is defaulted to the appropriate value.
* pNext is defaulted to `null`.
* No other fields have default values.
```zig
pub const InstanceCreateInfo = extern struct {
    s_type: StructureType = .instance_create_info,
    p_next: ?*const c_void = null,
    flags: InstanceCreateFlags,
    ...
};
```

### Pointer types
Pointer types in both commands (wrapped and function pointers) and struct fields are augmented with the following information, where available in the registry:
* Pointer optional-ness.
* Pointer const-ness.
* Pointer size: Either single-item, null-terminated or many-items.

Note that this information is not everywhere as useful in the registry, leading to places where optional-ness is not correct. Most notably, CreateInfo type structures which take a slice often have the item count marked as optional, but the pointer itself not. As of yet, this is not fixed in vulkan-zig. If drivers properly follow the Vulkan specification, these can be initialized to `undefined`, however, [that is not always the case](https://zeux.io/2019/07/17/serializing-pipeline-cache/).

### Platform types
Defaults with the same ABI layout are generated for most platform-defined types. These can either by bitcasted to, or overridden by defining them in the project root:
```zig
pub const xcb_connection_t = if (@hasDecl(root, "xcb_connection_t")) root.xcb_connection_t else @Type(.Opaque);
```
For some times (such as those from Google Games Platform) no default is known. Usage of these without providing a concrete type in the project root generates a compile error.

### Shader compilation
vulkan-zig provides functionality to help compiling shaders using glslc. It can be used from build.zig as follows:

```zig
const vkgen = @import("vulkan-zig/generator/index.zig");

pub fn build(b: *Builder) void {
    ...
    const exe = b.addExecutable("my-executable", "src/main.zig");

    const gen = vkgen.VkGenerateStep(b, "path/to/vk.xml", "vk.zig");
    exe.step.dependOn(&gen.step);
    exe.addPackagePath("vulkan", gen.full_out_path);

    const shader_comp = vkgen.ShaderCompileStep.init(
        builder,
        &[_][]const u8{"glslc", "--target-env=vulkan1.2"}, // Path to glslc and additional parameters
    );
    exe.step.dependOn(&shader_comp.step);
    const spv_path = shader_comp.addShader("path/to/shader.frag");
}
```
Upon compilation, glslc is then invoked to compile each shader, and the result is placed within `zig-cache`. `addShader` returns the full path to the compiled shader code. This file can then be included in the project, as is done in [build.zig for the example](build.zig) by generating an additional file which uses `@embedFile`.

## Limitations
* Currently, the self-hosted version of Zig's cache-hash API is not yet ready for usage, which means that the bindings are regenerated every time an executable is built.

* vulkan-zig has as of yet no functionality for selecting feature levels and extensions when generating bindings. This is because when an extension is promoted to Vulkan core, its fields and commands are renamed to lose the extensions author tag (for example, VkSemaphoreWaitFlagsKHR was renamed to VkSemaphoreWaitFlags when it was promoted from an extension to Vulkan 1.2 core). This leads to inconsistencies when only items from up to a certain feature level is included, as these promoted items then need to re-gain a tag.

## Example
A partial implementation of https://vulkan-tutorial.org is implemented in [examples/triangle.zig](examples/triangle.zig). This example can be ran by executing `zig build run-triangle` in vulkan-zig's root.

## See also
* Implementation of https://vulkan-tutorial.org: https://github.com/andrewrk/zig-vulkan-triangle.
* Alternative binding generator: https://github.com/SpexGuy/Zig-Vulkan-Headers
