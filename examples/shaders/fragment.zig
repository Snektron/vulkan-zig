const std = @import("std");
const gpu = std.gpu;

extern const v_color: @Vector(3, f32) addrspace(.input);
extern var f_color: @Vector(4, f32) addrspace(.output);

export fn main() callconv(.spirv_fragment) void {
    gpu.location(&v_color, 0);
    gpu.location(&f_color, 0);

    f_color = .{ v_color[0], v_color[1], v_color[2], 1.0 };
}
