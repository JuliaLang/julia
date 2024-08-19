use crate::api::mmtk_get_obj_size;
use crate::julia_scanning::{
    ijl_small_typeof, jl_genericmemory_typename, mmtk_jl_genericmemory_how, mmtk_jl_typeof,
    mmtk_jl_typetagof,
};
use crate::{julia_types::*, UPCALLS};
use crate::{JuliaVM, JULIA_BUFF_TAG, JULIA_HEADER_SIZE};
use log::trace;
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

#[cfg(feature = "object_pinning")]
pub(crate) const LOCAL_PINNING_METADATA_BITS_SPEC: VMLocalPinningBitSpec =
    VMLocalPinningBitSpec::side_after(MARKING_METADATA_SPEC.as_spec());

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
        VMLocalForwardingPointerSpec::in_header(-64);

    #[cfg(feature = "object_pinning")]
    const LOCAL_FORWARDING_BITS_SPEC: VMLocalForwardingBitsSpec =
        VMLocalForwardingBitsSpec::side_after(LOCAL_PINNING_METADATA_BITS_SPEC.as_spec());
    #[cfg(not(feature = "object_pinning"))]
    const LOCAL_FORWARDING_BITS_SPEC: VMLocalForwardingBitsSpec =
        VMLocalForwardingBitsSpec::side_after(MARKING_METADATA_SPEC.as_spec());

    const LOCAL_MARK_BIT_SPEC: VMLocalMarkBitSpec = MARKING_METADATA_SPEC;
    const LOCAL_LOS_MARK_NURSERY_SPEC: VMLocalLOSMarkNurserySpec = LOS_METADATA_SPEC;
    const UNIFIED_OBJECT_REFERENCE_ADDRESS: bool = false;
    const OBJECT_REF_OFFSET_LOWER_BOUND: isize = 0;

    #[cfg(feature = "object_pinning")]
    const LOCAL_PINNING_BIT_SPEC: VMLocalPinningBitSpec = LOCAL_PINNING_METADATA_BITS_SPEC;

    fn copy(
        from: ObjectReference,
        semantics: CopySemantics,
        copy_context: &mut GCWorkerCopyContext<JuliaVM>,
    ) -> ObjectReference {
        trace!("Attempting to copy object {}", from);

        let bytes = Self::get_current_size(from);
        let from_addr = from.to_raw_address();
        let from_start = Self::ref_to_object_start(from);
        let header_offset = from_addr - from_start;

        let dst = if header_offset == 8 {
            // regular object
            // Note: The `from` reference is not used by any allocator currently in MMTk core.
            copy_context.alloc_copy(from, bytes, 16, 8, semantics)
        } else if header_offset == 16 {
            // buffer should not be copied
            unimplemented!();
        } else {
            unimplemented!()
        };
        // `alloc_copy` should never return zero.
        debug_assert!(!dst.is_zero());

        let src = from_start;
        unsafe {
            std::ptr::copy_nonoverlapping::<u8>(src.to_ptr(), dst.to_mut_ptr(), bytes);
        }
        let to_obj = unsafe { ObjectReference::from_raw_address_unchecked(dst + header_offset) };

        copy_context.post_copy(to_obj, bytes, semantics);

        trace!("Copied object {} into {}", from, to_obj);

        unsafe {
            let vt = mmtk_jl_typeof(from.to_raw_address());

            if (*vt).name == jl_genericmemory_typename {
                ((*UPCALLS).update_inlined_array)(from.to_raw_address(), to_obj.to_raw_address())
            }
        }

        // zero from_obj (for debugging purposes)
        #[cfg(debug_assertions)]
        {
            use atomic::Ordering;
            unsafe {
                libc::memset(from_start.to_mut_ptr(), 0, bytes);
            }

            Self::LOCAL_FORWARDING_BITS_SPEC.store_atomic::<JuliaVM, u8>(
                from,
                0b10 as u8, // BEING_FORWARDED
                None,
                Ordering::SeqCst,
            );
        }

        to_obj
    }

    fn copy_to(_from: ObjectReference, _to: ObjectReference, _region: Address) -> Address {
        unimplemented!()
    }

    fn get_current_size(object: ObjectReference) -> usize {
        // not being called by objects in LOS
        debug_assert!(!is_object_in_los(&object));

        let obj_size = unsafe { get_so_object_size(object) };
        obj_size
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
    fn ref_to_header(object: ObjectReference) -> Address {
        object.to_raw_address()
    }

    const IN_OBJECT_ADDRESS_OFFSET: isize = 0;

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

#[inline(always)]
pub unsafe fn get_so_object_size(object: ObjectReference) -> usize {
    let obj_address = object.to_raw_address();
    let mut vtag = mmtk_jl_typetagof(obj_address);
    let mut vtag_usize = vtag.as_usize();

    if vtag_usize == JULIA_BUFF_TAG {
        return mmtk_get_obj_size(object);
    }

    if vtag_usize == ((mmtk_jl_small_typeof_tags_mmtk_jl_datatype_tag as usize) << 4)
        || vtag_usize == ((mmtk_jl_small_typeof_tags_mmtk_jl_unionall_tag as usize) << 4)
        || vtag_usize == ((mmtk_jl_small_typeof_tags_mmtk_jl_uniontype_tag as usize) << 4)
        || vtag_usize == ((mmtk_jl_small_typeof_tags_mmtk_jl_tvar_tag as usize) << 4)
        || vtag_usize == ((mmtk_jl_small_typeof_tags_mmtk_jl_vararg_tag as usize) << 4)
    {
        // these objects have pointers in them, but no other special handling
        // so we want these to fall through to the end
        vtag_usize = ijl_small_typeof[vtag.as_usize() / std::mem::size_of::<Address>()] as usize;
        vtag = Address::from_usize(vtag_usize);
    } else if vtag_usize < ((mmtk_jl_small_typeof_tags_mmtk_jl_max_tags as usize) << 4) {
        if vtag_usize == ((mmtk_jl_small_typeof_tags_mmtk_jl_simplevector_tag as usize) << 4) {
            let length = (*obj_address.to_ptr::<mmtk_jl_svec_t>()).length as usize;
            let dtsz =
                length * std::mem::size_of::<Address>() + std::mem::size_of::<mmtk_jl_svec_t>();

            debug_assert!(
                dtsz + JULIA_HEADER_SIZE <= 2032,
                "size {} greater than minimum!",
                dtsz + JULIA_HEADER_SIZE
            );

            return llt_align(dtsz + JULIA_HEADER_SIZE, 16);
        } else if vtag_usize
            == ((mmtk_jl_small_typeof_tags_mmtk_jl_module_tag as usize) << 4) as usize
        {
            let dtsz = std::mem::size_of::<mmtk_jl_module_t>();
            debug_assert!(
                dtsz + JULIA_HEADER_SIZE <= 2032,
                "size {} greater than minimum!",
                dtsz + JULIA_HEADER_SIZE
            );

            return llt_align(dtsz + JULIA_HEADER_SIZE, 16);
        } else if vtag_usize == ((mmtk_jl_small_typeof_tags_mmtk_jl_task_tag as usize) << 4) {
            let dtsz = std::mem::size_of::<mmtk_jl_task_t>();
            debug_assert!(
                dtsz + JULIA_HEADER_SIZE <= 2032,
                "size {} greater than minimum!",
                dtsz + JULIA_HEADER_SIZE
            );

            return llt_align(dtsz + JULIA_HEADER_SIZE, 16);
        } else if vtag_usize == ((mmtk_jl_small_typeof_tags_mmtk_jl_string_tag as usize) << 4) {
            let length = object.to_raw_address().load::<usize>();
            let dtsz = length + std::mem::size_of::<usize>() + 1;

            debug_assert!(
                dtsz + JULIA_HEADER_SIZE <= 2032,
                "size {} greater than minimum!",
                dtsz + JULIA_HEADER_SIZE
            );

            // NB: Strings are aligned to 8 and not to 16
            return llt_align(dtsz + JULIA_HEADER_SIZE, 8);
        } else {
            let vt = ijl_small_typeof[vtag_usize / std::mem::size_of::<Address>()];
            let layout = (*vt).layout;
            let dtsz = (*layout).size as usize;
            debug_assert!(
                dtsz + JULIA_HEADER_SIZE <= 2032,
                "size {} greater than minimum!",
                dtsz + JULIA_HEADER_SIZE
            );

            return llt_align(dtsz + JULIA_HEADER_SIZE, 16);
        }
    } else {
        let vt = vtag.to_ptr::<mmtk_jl_datatype_t>();
        let type_tag = mmtk_jl_typetagof(vtag);

        if type_tag.as_usize() != ((mmtk_jl_small_typeof_tags_mmtk_jl_datatype_tag as usize) << 4)
            || (*vt).smalltag() != 0
        {
            panic!(
                "GC error (probable corruption) - !jl_is_datatype(vt) = {}; vt->smalltag = {}, vt = {:?}",
                type_tag.as_usize() != ((mmtk_jl_small_typeof_tags_mmtk_jl_datatype_tag as usize) << 4),
                (*(vtag.to_ptr::<mmtk_jl_datatype_t>())).smalltag() != 0,
                vt
            );
        }
    }

    let obj_type = mmtk_jl_typeof(obj_address);
    let vt = vtag.to_ptr::<mmtk_jl_datatype_t>();

    assert_eq!(obj_type, vt);
    if (*vt).name == jl_genericmemory_typename {
        let m = obj_address.to_ptr::<mmtk_jl_genericmemory_t>();
        let how = mmtk_jl_genericmemory_how(m);
        let res = if how == 0 {
            let layout = (*(mmtk_jl_typetagof(obj_address).to_ptr::<mmtk_jl_datatype_t>())).layout;
            let mut sz = (*layout).size as usize * (*m).length as usize;
            if (*layout).flags.arrayelem_isunion() != 0 {
                sz += (*m).length as usize;
            }

            let dtsz = llt_align(std::mem::size_of::<mmtk_jl_genericmemory_t>(), 16);
            llt_align(sz + dtsz + JULIA_HEADER_SIZE, 16)
        } else {
            let dtsz =
                std::mem::size_of::<mmtk_jl_genericmemory_t>() + std::mem::size_of::<Address>();
            llt_align(dtsz + JULIA_HEADER_SIZE, 16)
        };

        debug_assert!(res <= 2032, "size {} greater than minimum!", res);

        return res;
    }

    let layout = (*vt).layout;
    let dtsz = (*layout).size as usize;
    debug_assert!(
        dtsz + JULIA_HEADER_SIZE <= 2032,
        "size {} greater than minimum!",
        dtsz + JULIA_HEADER_SIZE
    );

    return llt_align(dtsz + JULIA_HEADER_SIZE, 16);
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
pub unsafe fn llt_align(size: usize, align: usize) -> usize {
    ((size) + (align) - 1) & !((align) - 1)
}

#[inline(always)]
pub unsafe fn mmtk_jl_is_uniontype(t: *const mmtk_jl_datatype_t) -> bool {
    mmtk_jl_typetagof(Address::from_ptr(t)).as_usize()
        == (mmtk_jl_small_typeof_tags_mmtk_jl_uniontype_tag << 4) as usize
}
