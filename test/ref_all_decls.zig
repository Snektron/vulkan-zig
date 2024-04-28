const std = @import("std");
const vk = @import("vulkan");

// Provide bogus defaults for unknown platform types
// The actual type does not really matter here...
pub const GgpStreamDescriptor = u32;
pub const GgpFrameToken = u32;
pub const _screen_buffer = u32;
pub const NvSciSyncAttrList = u32;
pub const NvSciSyncObj = u32;
pub const NvSciSyncFence = u32;
pub const NvSciBufAttrList = u32;
pub const NvSciBufObj = u32;
pub const ANativeWindow = u32;
pub const AHardwareBuffer = u32;
pub const CAMetalLayer = u32;
pub const MTLDevice_id = u32;
pub const MTLCommandQueue_id = u32;
pub const MTLBuffer_id = u32;
pub const MTLTexture_id = u32;
pub const MTLSharedEvent_id = u32;
pub const IOSurfaceRef = u32;

// For some reason these types are exported in a different header, and not described in vk.xml.
pub const StdVideoH264ProfileIdc = u32;
pub const StdVideoH264LevelIdc = u32;
pub const StdVideoH264ChromaFormatIdc = u32;
pub const StdVideoH264PocType = u32;
pub const StdVideoH264SpsFlags = u32;
pub const StdVideoH264ScalingLists = u32;
pub const StdVideoH264SequenceParameterSetVui = u32;
pub const StdVideoH264AspectRatioIdc = u32;
pub const StdVideoH264HrdParameters = u32;
pub const StdVideoH264SpsVuiFlags = u32;
pub const StdVideoH264WeightedBipredIdc = u32;
pub const StdVideoH264PpsFlags = u32;
pub const StdVideoH264SliceType = u32;
pub const StdVideoH264CabacInitIdc = u32;
pub const StdVideoH264DisableDeblockingFilterIdc = u32;
pub const StdVideoH264PictureType = u32;
pub const StdVideoH264ModificationOfPicNumsIdc = u32;
pub const StdVideoH264MemMgmtControlOp = u32;
pub const StdVideoDecodeH264PictureInfo = u32;
pub const StdVideoDecodeH264ReferenceInfo = u32;
pub const StdVideoDecodeH264PictureInfoFlags = u32;
pub const StdVideoDecodeH264ReferenceInfoFlags = u32;
pub const StdVideoH264SequenceParameterSet = u32;
pub const StdVideoH264PictureParameterSet = u32;
pub const StdVideoH265ProfileIdc = u32;
pub const StdVideoH265VideoParameterSet = u32;
pub const StdVideoH265SequenceParameterSet = u32;
pub const StdVideoH265PictureParameterSet = u32;
pub const StdVideoH265DecPicBufMgr = u32;
pub const StdVideoH265HrdParameters = u32;
pub const StdVideoH265VpsFlags = u32;
pub const StdVideoH265LevelIdc = u32;
pub const StdVideoH265SpsFlags = u32;
pub const StdVideoH265ScalingLists = u32;
pub const StdVideoH265SequenceParameterSetVui = u32;
pub const StdVideoH265PredictorPaletteEntries = u32;
pub const StdVideoH265PpsFlags = u32;
pub const StdVideoH265SubLayerHrdParameters = u32;
pub const StdVideoH265HrdFlags = u32;
pub const StdVideoH265SpsVuiFlags = u32;
pub const StdVideoH265SliceType = u32;
pub const StdVideoH265PictureType = u32;
pub const StdVideoDecodeH265PictureInfo = u32;
pub const StdVideoDecodeH265ReferenceInfo = u32;
pub const StdVideoDecodeH265PictureInfoFlags = u32;
pub const StdVideoDecodeH265ReferenceInfoFlags = u32;
pub const StdVideoAV1Profile = u32;
pub const StdVideoAV1Level = u32;
pub const StdVideoAV1SequenceHeader = u32;
pub const StdVideoDecodeAV1PictureInfo = u32;
pub const StdVideoDecodeAV1ReferenceInfo = u32;
pub const StdVideoEncodeH264SliceHeader = u32;
pub const StdVideoEncodeH264PictureInfo = u32;
pub const StdVideoEncodeH264ReferenceInfo = u32;
pub const StdVideoEncodeH264SliceHeaderFlags = u32;
pub const StdVideoEncodeH264ReferenceListsInfo = u32;
pub const StdVideoEncodeH264PictureInfoFlags = u32;
pub const StdVideoEncodeH264ReferenceInfoFlags = u32;
pub const StdVideoEncodeH264RefMgmtFlags = u32;
pub const StdVideoEncodeH264RefListModEntry = u32;
pub const StdVideoEncodeH264RefPicMarkingEntry = u32;
pub const StdVideoEncodeH265PictureInfoFlags = u32;
pub const StdVideoEncodeH265PictureInfo = u32;
pub const StdVideoEncodeH265SliceSegmentHeader = u32;
pub const StdVideoEncodeH265ReferenceInfo = u32;
pub const StdVideoEncodeH265ReferenceListsInfo = u32;
pub const StdVideoEncodeH265SliceSegmentHeaderFlags = u32;
pub const StdVideoEncodeH265ReferenceInfoFlags = u32;
pub const StdVideoEncodeH265ReferenceModificationFlags = u32;

comptime {
    @setEvalBranchQuota(100000);
    reallyRefAllDecls(vk);
}

fn reallyRefAllDecls(comptime T: type) void {
    switch (@typeInfo(T)) {
        .Struct, .Union => {
            reallyRefAllContainerDecls(T);
            inline for (std.meta.fields(T)) |field| {
                reallyRefAllDecls(field.type);
            }
        },
        .Enum, .Opaque => {
            reallyRefAllContainerDecls(T);
        },
        else => {},
    }
}

fn reallyRefAllContainerDecls(comptime T: type) void {
    inline for (comptime std.meta.declarations(T)) |decl| {
        if (@TypeOf(@field(T, decl.name)) == type) {
            reallyRefAllDecls(@field(T, decl.name));
        }
    }
}
