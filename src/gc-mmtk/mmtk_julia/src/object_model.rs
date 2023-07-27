use crate::api::{mmtk_get_obj_size, mmtk_object_is_managed_by_mmtk};
use crate::julia_scanning::{
    jl_array_typename, jl_method_type, jl_module_type, jl_simplevector_type, jl_string_type,
    jl_task_type, mmtk_jl_array_len, mmtk_jl_array_ndimwords, mmtk_jl_tparam0, mmtk_jl_typeof,
    mmtk_jl_typetagof,
};
use crate::julia_types::*;
use crate::{JuliaVM, JULIA_BUFF_TAG, JULIA_HEADER_SIZE};
use mmtk::util::copy::*;
use mmtk::util::{Address, ObjectReference};
use mmtk::vm::ObjectModel;
use mmtk::vm::*;

pub struct VMObjectModel {}

/// Global logging bit metadata spec
/// 1 bit per object
pub(crate) const LOGGING_SIDE_METADATA_SPEC: VMGlobalLogBitSpec = VMGlobalLogBitSpec::side_first();

pub(crate) const MARKING_METADATA_SPEC: VMLocalMarkBitSpec =
    VMLocalMarkBitSpec::side_after(LOS_METADATA_SPEC.as_spec());

// pub(crate) const LOCAL_FORWARDING_POINTER_METADATA_SPEC: VMLocalForwardingPointerSpec =
//     VMLocalForwardingPointerSpec::side_after(MARKING_METADATA_SPEC.as_spec());

// pub(crate) const LOCAL_FORWARDING_METADATA_BITS_SPEC: VMLocalForwardingBitsSpec =
//     VMLocalForwardingBitsSpec::side_after(LOCAL_FORWARDING_POINTER_METADATA_SPEC.as_spec());

/// PolicySpecific mark-and-nursery bits metadata spec
/// 2-bits per object
pub(crate) const LOS_METADATA_SPEC: VMLocalLOSMarkNurserySpec =
    VMLocalLOSMarkNurserySpec::side_first();

impl ObjectModel<JuliaVM> for VMObjectModel {
    const GLOBAL_LOG_BIT_SPEC: VMGlobalLogBitSpec = LOGGING_SIDE_METADATA_SPEC;
    const LOCAL_FORWARDING_POINTER_SPEC: VMLocalForwardingPointerSpec =
        VMLocalForwardingPointerSpec::in_header(0);
    const LOCAL_FORWARDING_BITS_SPEC: VMLocalForwardingBitsSpec =
        VMLocalForwardingBitsSpec::in_header(0);
    const LOCAL_MARK_BIT_SPEC: VMLocalMarkBitSpec = MARKING_METADATA_SPEC;
    const LOCAL_LOS_MARK_NURSERY_SPEC: VMLocalLOSMarkNurserySpec = LOS_METADATA_SPEC;
    const UNIFIED_OBJECT_REFERENCE_ADDRESS: bool = false;
    const OBJECT_REF_OFFSET_LOWER_BOUND: isize = 0;

    fn copy(
        _from: ObjectReference,
        _semantics: CopySemantics,
        _copy_context: &mut GCWorkerCopyContext<JuliaVM>,
    ) -> ObjectReference {
        unimplemented!()
    }

    fn copy_to(_from: ObjectReference, _to: ObjectReference, _region: Address) -> Address {
        unimplemented!()
    }

    fn get_current_size(object: ObjectReference) -> usize {
        // not being called by objects in LOS
        debug_assert!(!is_object_in_los(&object));
        unsafe { get_so_object_size(object) }
    }

    fn get_size_when_copied(_object: ObjectReference) -> usize {
        unimplemented!()
    }

    fn get_align_when_copied(_object: ObjectReference) -> usize {
        unimplemented!()
    }

    fn get_align_offset_when_copied(_object: ObjectReference) -> usize {
        unimplemented!()
    }

    fn get_reference_when_copied_to(_from: ObjectReference, _to: Address) -> ObjectReference {
        unimplemented!()
    }

    fn get_type_descriptor(_reference: ObjectReference) -> &'static [i8] {
        unimplemented!()
    }

    #[inline(always)]
    fn ref_to_object_start(object: ObjectReference) -> Address {
        let res = if is_object_in_los(&object) {
            object.to_raw_address() - 48
        } else {
            unsafe { get_object_start_ref(object) }
        };
        res
    }

    #[inline(always)]
    fn ref_to_address(object: ObjectReference) -> Address {
        object.to_raw_address()
    }

    #[inline(always)]
    fn address_to_ref(address: Address) -> ObjectReference {
        ObjectReference::from_raw_address(address)
    }

    #[inline(always)]
    fn ref_to_header(_object: ObjectReference) -> Address {
        unreachable!()
        // object.to_raw_address() - 8
    }

    fn dump_object(_object: ObjectReference) {
        unimplemented!()
    }
}

#[inline(always)]
pub fn is_object_in_los(object: &ObjectReference) -> bool {
    // FIXME: get the range from MMTk. Or at least assert at boot time to make sure those constants are correct.
    (*object).to_raw_address().as_usize() >= 0x600_0000_0000
        && (*object).to_raw_address().as_usize() < 0x800_0000_0000
}

const JL_GC_SIZECLASSES: [::std::os::raw::c_int; 49] = [
    8,
    // 16 pools at 8-byte spacing
    // the 8-byte aligned pools are only used for Strings
    16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 96, 104, 112, 120, 128, 136,
    // 8 pools at 16-byte spacing
    144, 160, 176, 192, 208, 224, 240, 256,
    // the following tables are computed for maximum packing efficiency via the formula:
    // pg = 2^14
    // sz = (div.(pg-8, rng).÷16)*16; hcat(sz, (pg-8).÷sz, pg .- (pg-8).÷sz.*sz)'

    // rng = 60:-4:32 (8 pools)
    272, 288, 304, 336, 368, 400, 448, 496,
    //   60,  56,  53,  48,  44,  40,  36,  33, /pool
    //   64, 256, 272, 256, 192, 384, 256,  16, bytes lost

    // rng = 30:-2:16 (8 pools)
    544, 576, 624, 672, 736, 816, 896, 1008,
    //   30,  28,  26,  24,  22,  20,  18,  16, /pool
    //   64, 256, 160, 256, 192,  64, 256, 256, bytes lost

    // rng = 15:-1:8 (8 pools)
    1088, 1168, 1248, 1360, 1488, 1632, 1808,
    2032, //    15,   14,   13,   12,   11,   10,    9,    8, /pool
          //    64,   32,  160,   64,   16,   64,  112,  128, bytes lost
];

const SZCLASS_TABLE: [u8; 128] = [
    0, 1, 3, 5, 7, 9, 11, 13, 15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 28, 29, 29, 30,
    30, 31, 31, 31, 32, 32, 32, 33, 33, 33, 34, 34, 35, 35, 35, 36, 36, 36, 37, 37, 37, 37, 38, 38,
    38, 38, 38, 39, 39, 39, 39, 39, 40, 40, 40, 40, 40, 40, 40, 41, 41, 41, 41, 41, 42, 42, 42, 42,
    42, 43, 43, 43, 43, 43, 44, 44, 44, 44, 44, 44, 44, 45, 45, 45, 45, 45, 45, 45, 45, 46, 46, 46,
    46, 46, 46, 46, 46, 46, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 48, 48, 48, 48, 48, 48, 48,
    48, 48, 48, 48, 48, 48, 48,
];

#[inline(always)]
pub unsafe fn get_so_object_size(object: ObjectReference) -> usize {
    let obj_address = object.to_raw_address();
    let obj_type = mmtk_jl_typeof(obj_address);

    if obj_type as usize == JULIA_BUFF_TAG {
        mmtk_get_obj_size(object)
    } else if (*obj_type).name == jl_array_typename {
        let a = obj_address.to_ptr::<mmtk_jl_array_t>();
        let osize = match (*a).flags.how_custom() {
            0 => {
                let a_ndims_words = mmtk_jl_array_ndimwords(mmtk_jl_array_ndims(a));
                let mut dtsz = std::mem::size_of::<mmtk_jl_array_t>()
                    + a_ndims_words * std::mem::size_of::<usize>();
                let data = (*a).data;
                let data_addr = Address::from_mut_ptr(data);

                if mmtk_object_is_managed_by_mmtk(data_addr.as_usize()) {
                    let pre_data_bytes = (data_addr.as_usize()
                        - (*a).offset as usize * (*a).elsize as usize)
                        - a as usize;

                    if pre_data_bytes > 0 {
                        // a->data is allocated after a
                        dtsz = pre_data_bytes;
                        dtsz += mmtk_jl_array_nbytes(a);
                    }
                    if dtsz + JULIA_HEADER_SIZE > 2032 {
                        // if it's too large to be inlined (a->data and a are disjoint objects)
                        dtsz = std::mem::size_of::<mmtk_jl_array_t>()
                            + a_ndims_words * std::mem::size_of::<usize>();
                    }
                }
                debug_assert!(
                    dtsz + JULIA_HEADER_SIZE <= 2032,
                    "size {} greater than minimum!",
                    dtsz + JULIA_HEADER_SIZE
                );

                let pool_id = mmtk_jl_gc_szclass(dtsz + JULIA_HEADER_SIZE);
                JL_GC_SIZECLASSES[pool_id]
            }
            1 | 2 => {
                let a_ndims_words = mmtk_jl_array_ndimwords(mmtk_jl_array_ndims(a));
                let dtsz = std::mem::size_of::<mmtk_jl_array_t>()
                    + a_ndims_words * std::mem::size_of::<usize>();

                debug_assert!(
                    dtsz + JULIA_HEADER_SIZE <= 2032,
                    "size {} greater than minimum!",
                    dtsz + JULIA_HEADER_SIZE
                );

                let pool_id = mmtk_jl_gc_szclass(dtsz + JULIA_HEADER_SIZE);
                JL_GC_SIZECLASSES[pool_id]
            }
            3 => {
                let a_ndims_words = mmtk_jl_array_ndimwords(mmtk_jl_array_ndims(a));
                let dtsz = std::mem::size_of::<mmtk_jl_array_t>()
                    + a_ndims_words * std::mem::size_of::<usize>()
                    + std::mem::size_of::<Address>();
                debug_assert!(
                    dtsz + JULIA_HEADER_SIZE <= 2032,
                    "size {} greater than minimum!",
                    dtsz + JULIA_HEADER_SIZE
                );

                let pool_id = mmtk_jl_gc_szclass(dtsz + JULIA_HEADER_SIZE);
                JL_GC_SIZECLASSES[pool_id]
            }
            _ => unreachable!(),
        };

        osize as usize
    } else if obj_type == jl_simplevector_type {
        let length = (*obj_address.to_ptr::<mmtk_jl_svec_t>()).length as usize;
        let dtsz = length * std::mem::size_of::<Address>() + std::mem::size_of::<mmtk_jl_svec_t>();

        debug_assert!(
            dtsz + JULIA_HEADER_SIZE <= 2032,
            "size {} greater than minimum!",
            dtsz + JULIA_HEADER_SIZE
        );

        let sz = dtsz + JULIA_HEADER_SIZE;

        let pool_id = mmtk_jl_gc_szclass(sz);
        let osize = JL_GC_SIZECLASSES[pool_id];

        osize as usize
    } else if obj_type == jl_module_type {
        let dtsz = std::mem::size_of::<mmtk_jl_module_t>();
        debug_assert!(
            dtsz + JULIA_HEADER_SIZE <= 2032,
            "size {} greater than minimum!",
            dtsz + JULIA_HEADER_SIZE
        );

        let pool_id = mmtk_jl_gc_szclass(dtsz + JULIA_HEADER_SIZE);
        let osize = JL_GC_SIZECLASSES[pool_id];

        osize as usize
    } else if obj_type == jl_task_type {
        let dtsz = std::mem::size_of::<mmtk_jl_task_t>();
        debug_assert!(
            dtsz + JULIA_HEADER_SIZE <= 2032,
            "size {} greater than minimum!",
            dtsz + JULIA_HEADER_SIZE
        );

        let pool_id = mmtk_jl_gc_szclass(dtsz + JULIA_HEADER_SIZE);
        let osize = JL_GC_SIZECLASSES[pool_id];

        osize as usize
    } else if obj_type == jl_string_type {
        let length = object.to_raw_address().load::<usize>();
        let dtsz = length + std::mem::size_of::<usize>() + 1;

        debug_assert!(
            dtsz + JULIA_HEADER_SIZE <= 2032,
            "size {} greater than minimum!",
            dtsz + JULIA_HEADER_SIZE
        );

        let pool_id = mmtk_jl_gc_szclass_align8(dtsz + JULIA_HEADER_SIZE);
        let osize = JL_GC_SIZECLASSES[pool_id];

        osize as usize
    } else if obj_type == jl_method_type {
        let dtsz = std::mem::size_of::<mmtk_jl_method_t>();
        debug_assert!(
            dtsz + JULIA_HEADER_SIZE <= 2032,
            "size {} greater than minimum!",
            dtsz + JULIA_HEADER_SIZE
        );

        let pool_id = mmtk_jl_gc_szclass(dtsz + JULIA_HEADER_SIZE);
        let osize = JL_GC_SIZECLASSES[pool_id];

        osize as usize
    } else {
        let layout = (*obj_type).layout;
        let dtsz = (*layout).size as usize;
        debug_assert!(
            dtsz + JULIA_HEADER_SIZE <= 2032,
            "size {} greater than minimum!",
            dtsz + JULIA_HEADER_SIZE
        );

        let pool_id = mmtk_jl_gc_szclass(dtsz + JULIA_HEADER_SIZE);
        let osize = JL_GC_SIZECLASSES[pool_id];

        osize as usize
    }
}

#[inline(always)]
pub unsafe fn get_object_start_ref(object: ObjectReference) -> Address {
    let obj_address = object.to_raw_address();
    let obj_type = mmtk_jl_typeof(obj_address);

    if obj_type as usize == JULIA_BUFF_TAG {
        obj_address - 2 * JULIA_HEADER_SIZE
    } else {
        obj_address - JULIA_HEADER_SIZE
    }
}

#[inline(always)]
pub unsafe fn mmtk_jl_gc_szclass(sz: usize) -> usize {
    if sz <= 8 {
        return 0;
    }

    let klass = SZCLASS_TABLE[(sz + 15) / 16];
    return klass as usize;
}

#[inline(always)]
pub unsafe fn mmtk_jl_gc_szclass_align8(sz: usize) -> usize {
    if sz >= 16 && sz <= 152 {
        return (sz + 7) / 8 - 1;
    }

    return mmtk_jl_gc_szclass(sz);
}

#[inline(always)]
pub unsafe fn mmtk_jl_array_nbytes(a: *const mmtk_jl_array_t) -> usize {
    let mut sz;

    let isbitsunion = mmtk_jl_array_isbitunion(a);

    if mmtk_jl_array_ndims(a) == 1 {
        let elsize_is_one = if (*a).elsize == 1 && !isbitsunion {
            1
        } else {
            0
        };
        sz = (*a).elsize as usize * (*a).__bindgen_anon_1.maxsize + elsize_is_one;
    } else {
        sz = (*a).elsize as usize * (*a).length;
    }

    if isbitsunion {
        sz += mmtk_jl_array_len(a);
    }

    sz
}

#[inline(always)]
pub unsafe fn mmtk_jl_array_ndims(a: *const mmtk_jl_array_t) -> u32 {
    (*a).flags.ndims_custom() as u32
}

#[inline(always)]
pub unsafe fn mmtk_jl_array_isbitunion(a: *const mmtk_jl_array_t) -> bool {
    ((*a).flags.ptrarray_custom()) == 0
        && mmtk_jl_is_uniontype(mmtk_jl_tparam0(mmtk_jl_typeof(Address::from_ptr(a))))
}

#[inline(always)]
pub unsafe fn mmtk_jl_is_uniontype(t: *const mmtk_jl_datatype_t) -> bool {
    mmtk_jl_typetagof(Address::from_ptr(t)).as_usize()
        == (mmtk_jlsmall_typeof_tags_mmtk_jl_uniontype_tag << 4) as usize
}
