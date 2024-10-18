use crate::api::mmtk_object_is_managed_by_mmtk;
use crate::julia_types::*;
use crate::slots::JuliaVMSlot;
use crate::slots::OffsetSlot;
use crate::JULIA_BUFF_TAG;
use memoffset::offset_of;
use mmtk::util::{Address, ObjectReference};
use mmtk::vm::slot::SimpleSlot;
use mmtk::vm::SlotVisitor;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use crate::jl_gc_genericmemory_how;
use crate::jl_gc_get_owner_address_to_mmtk;
use crate::jl_gc_get_stackbase;
use crate::jl_gc_scan_julia_exc_obj;

const JL_MAX_TAGS: usize = 64; // from vm/julia/src/jl_exports.h
const OFFSET_OF_INLINED_SPACE_IN_MODULE: usize =
    offset_of!(jl_module_t, usings) + offset_of!(arraylist_t, _space);

#[allow(improper_ctypes)]
extern "C" {
    pub static jl_simplevector_type: *const jl_datatype_t;
    pub static jl_genericmemory_typename: *mut jl_typename_t;
    pub static jl_genericmemoryref_typename: *mut jl_typename_t;
    pub static jl_array_typename: *mut jl_typename_t;
    pub static jl_module_type: *const jl_datatype_t;
    pub static jl_task_type: *const jl_datatype_t;
    pub static jl_string_type: *const jl_datatype_t;
    pub static jl_weakref_type: *const jl_datatype_t;
    pub static jl_symbol_type: *const jl_datatype_t;
    pub static jl_method_type: *const jl_datatype_t;
    pub static jl_binding_partition_type: *const jl_datatype_t;
    pub static mut jl_small_typeof: [*mut jl_datatype_t; 128usize];
}

#[inline(always)]
pub unsafe fn mmtk_jl_typetagof(addr: Address) -> Address {
    let as_tagged_value =
        addr.as_usize() - std::mem::size_of::<crate::julia_scanning::jl_taggedvalue_t>();
    let t_header = Address::from_usize(as_tagged_value).load::<Address>();
    let t = t_header.as_usize() & !0xf;

    Address::from_usize(t)
}

#[inline(always)]
pub unsafe fn mmtk_jl_typeof(addr: Address) -> *const jl_datatype_t {
    mmtk_jl_to_typeof(mmtk_jl_typetagof(addr))
}

#[inline(always)]
pub unsafe fn mmtk_jl_to_typeof(t: Address) -> *const jl_datatype_t {
    let t_raw = t.as_usize();
    if t_raw < (JL_MAX_TAGS << 4) {
        let ty = jl_small_typeof[t_raw / std::mem::size_of::<Address>()];
        return ty;
    }
    return t.to_ptr::<jl_datatype_t>();
}

const PRINT_OBJ_TYPE: bool = false;

// This function is a rewrite of `gc_mark_outrefs()` in `gc.c`
// INFO: *_custom() functions are acessors to bitfields that do not use bindgen generated code.
#[inline(always)]
pub unsafe fn scan_julia_object<SV: SlotVisitor<JuliaVMSlot>>(obj: Address, closure: &mut SV) {
    // get Julia object type
    let mut vtag = mmtk_jl_typetagof(obj);
    let mut vtag_usize = vtag.as_usize();

    if PRINT_OBJ_TYPE {
        println!(
            "scan_julia_obj {}, obj_type = {:?}",
            obj,
            mmtk_jl_to_typeof(obj)
        );
    }

    // symbols are always marked
    // buffers are marked by their parent object
    if vtag.to_ptr::<jl_datatype_t>() == jl_symbol_type || vtag_usize == JULIA_BUFF_TAG {
        return;
    }

    if vtag_usize == ((jl_small_typeof_tags_jl_datatype_tag as usize) << 4)
        || vtag_usize == ((jl_small_typeof_tags_jl_unionall_tag as usize) << 4)
        || vtag_usize == ((jl_small_typeof_tags_jl_uniontype_tag as usize) << 4)
        || vtag_usize == ((jl_small_typeof_tags_jl_tvar_tag as usize) << 4)
        || vtag_usize == ((jl_small_typeof_tags_jl_vararg_tag as usize) << 4)
    {
        // these objects have pointers in them, but no other special handling
        // so we want these to fall through to the end
        vtag_usize = jl_small_typeof[vtag.as_usize() / std::mem::size_of::<Address>()] as usize;
        vtag = Address::from_usize(vtag_usize);
    } else if vtag_usize < ((jl_small_typeof_tags_jl_max_tags as usize) << 4) {
        // these objects either have specialing handling
        if vtag_usize == ((jl_small_typeof_tags_jl_simplevector_tag as usize) << 4) {
            if PRINT_OBJ_TYPE {
                println!("scan_julia_obj {}: simple vector\n", obj);
            }
            let length = mmtk_jl_svec_len(obj);
            let mut objary_begin = mmtk_jl_svec_data(obj);
            let objary_end = objary_begin.shift::<Address>(length as isize);

            while objary_begin < objary_end {
                process_slot(closure, objary_begin);
                objary_begin = objary_begin.shift::<Address>(1);
            }
        } else if vtag_usize == ((jl_small_typeof_tags_jl_module_tag as usize) << 4) {
            if PRINT_OBJ_TYPE {
                println!("scan_julia_obj {}: module\n", obj);
            }

            let m = obj.to_ptr::<jl_module_t>();
            let bindings_slot = ::std::ptr::addr_of!((*m).bindings);
            if PRINT_OBJ_TYPE {
                println!(" - scan bindings: {:?}\n", bindings_slot);
            }
            process_slot(closure, Address::from_ptr(bindings_slot));

            let bindingkeyset_slot = ::std::ptr::addr_of!((*m).bindingkeyset);
            if PRINT_OBJ_TYPE {
                println!(" - scan bindingkeyset: {:?}\n", bindingkeyset_slot);
            }
            process_slot(closure, Address::from_ptr(bindingkeyset_slot));

            let parent_slot = ::std::ptr::addr_of!((*m).parent);
            if PRINT_OBJ_TYPE {
                println!(" - scan parent: {:?}\n", parent_slot);
            }
            process_slot(closure, Address::from_ptr(parent_slot));

            // m.usings.items may be inlined in the module when the array list size <= AL_N_INLINE (cf. arraylist_new)
            // In that case it may be an mmtk object and not a malloced address.
            // If it is an mmtk object, (*m).usings.items will then be an internal pointer to the module
            // which means we will need to trace and update it if the module moves
            if mmtk_object_is_managed_by_mmtk((*m).usings.items as usize) {
                let offset = OFFSET_OF_INLINED_SPACE_IN_MODULE;
                let slot = Address::from_ptr(::std::ptr::addr_of!((*m).usings.items));
                process_offset_slot(closure, slot, offset);
            }

            let nusings = (*m).usings.len;
            if nusings > 0 {
                let mut objary_begin = Address::from_mut_ptr((*m).usings.items);
                let objary_end = objary_begin.shift::<Address>(nusings as isize);

                while objary_begin < objary_end {
                    if PRINT_OBJ_TYPE {
                        println!(" - scan usings: {:?}\n", objary_begin);
                    }
                    process_slot(closure, objary_begin);
                    objary_begin = objary_begin.shift::<Address>(1);
                }
            }
        } else if vtag_usize == ((jl_small_typeof_tags_jl_task_tag as usize) << 4) {
            if PRINT_OBJ_TYPE {
                println!("scan_julia_obj {}: task\n", obj);
            }

            let ta = obj.to_ptr::<jl_task_t>();

            mmtk_scan_gcstack(ta, closure);

            let layout = (*jl_task_type).layout;
            debug_assert!((*layout).fielddesc_type_custom() == 0);
            debug_assert!((*layout).nfields > 0);
            let npointers = (*layout).npointers;
            let mut obj8_begin = mmtk_jl_dt_layout_ptrs(layout);
            let obj8_end = obj8_begin.shift::<u8>(npointers as isize);

            while obj8_begin < obj8_end {
                let obj8_begin_loaded = obj8_begin.load::<u8>();
                let slot = obj.shift::<Address>(obj8_begin_loaded as isize);
                process_slot(closure, slot);
                obj8_begin = obj8_begin.shift::<u8>(1);
            }
        } else if vtag_usize == ((jl_small_typeof_tags_jl_string_tag as usize) << 4) {
            if PRINT_OBJ_TYPE {
                println!("scan_julia_obj {}: string\n", obj);
            }
        }
        return;
    } else {
        let vt = vtag.to_ptr::<jl_datatype_t>();
        let type_tag = mmtk_jl_typetagof(vtag);

        if type_tag.as_usize() != ((jl_small_typeof_tags_jl_datatype_tag as usize) << 4)
            || (*vt).smalltag() != 0
        {
            panic!(
                "GC error (probable corruption) - !jl_is_datatype(vt) = {}; vt->smalltag = {}, vt = {:?}",
                vt as usize != ((jl_small_typeof_tags_jl_datatype_tag as usize) << 4),
                (*(vtag.to_ptr::<jl_datatype_t>())).smalltag() != 0,
                vt
            );
        }
    }
    let vt = vtag.to_ptr::<jl_datatype_t>();
    if (*vt).name == jl_array_typename {
        let a = obj.to_ptr::<jl_array_t>();
        let memref = (*a).ref_;

        let ptr_or_offset = memref.ptr_or_offset;
        // if the object moves its pointer inside the array object (void* ptr_or_offset) needs to be updated as well
        if mmtk_object_is_managed_by_mmtk(ptr_or_offset as usize) {
            let ptr_or_ref_slot = Address::from_ptr(::std::ptr::addr_of!((*a).ref_.ptr_or_offset));
            let mem_addr_as_usize = memref.mem as usize;
            let ptr_or_offset_as_usize = ptr_or_offset as usize;
            if ptr_or_offset_as_usize > mem_addr_as_usize {
                let offset = ptr_or_offset_as_usize - mem_addr_as_usize;

                // Only update the offset pointer if the offset is valid (> 0)
                if offset > 0 {
                    process_offset_slot(closure, ptr_or_ref_slot, offset);
                }
            }
        }
    }
    if (*vt).name == jl_genericmemory_typename {
        if PRINT_OBJ_TYPE {
            println!("scan_julia_obj {}: genericmemory\n", obj);
        }
        let m = obj.to_ptr::<jl_genericmemory_t>();
        let how = jl_gc_genericmemory_how(obj);

        if PRINT_OBJ_TYPE {
            println!("scan_julia_obj {}: genericmemory how = {}\n", obj, how);
        }

        if how == 3 {
            let owner_addr = mmtk_jl_genericmemory_data_owner_field_address(m);
            process_slot(closure, owner_addr);

            return;
        }

        if (*m).length == 0 {
            return;
        }

        let layout = (*vt).layout;
        if (*layout).flags.arrayelem_isboxed() != 0 {
            let length = (*m).length;
            let mut objary_begin = Address::from_ptr((*m).ptr);
            let objary_end = objary_begin.shift::<Address>(length as isize);
            while objary_begin < objary_end {
                process_slot(closure, objary_begin);
                objary_begin = objary_begin.shift::<Address>(1);
            }
        } else if (*layout).first_ptr >= 0 {
            let npointers = (*layout).npointers;
            let elsize = (*layout).size as usize / std::mem::size_of::<Address>();
            let length = (*m).length;
            let mut objary_begin = Address::from_ptr((*m).ptr);
            let objary_end = objary_begin.shift::<Address>((length * elsize) as isize);
            if npointers == 1 {
                objary_begin = objary_begin.shift::<Address>((*layout).first_ptr as isize);
                while objary_begin < objary_end {
                    process_slot(closure, objary_begin);
                    objary_begin = objary_begin.shift::<Address>(elsize as isize);
                }
            } else if (*layout).fielddesc_type_custom() == 0 {
                let obj8_begin = mmtk_jl_dt_layout_ptrs(layout);
                let obj8_end = obj8_begin.shift::<u8>(npointers as isize);
                let mut elem_begin = obj8_begin;
                let elem_end = obj8_end;

                while objary_begin < objary_end {
                    while elem_begin < elem_end {
                        let elem_begin_loaded = elem_begin.load::<u8>();
                        let slot = objary_begin.shift::<Address>(elem_begin_loaded as isize);
                        process_slot(closure, slot);
                        elem_begin = elem_begin.shift::<u8>(1);
                    }
                    elem_begin = obj8_begin;
                    objary_begin = objary_begin.shift::<Address>(elsize as isize);
                }
            } else if (*layout).fielddesc_type_custom() == 1 {
                let mut obj16_begin = mmtk_jl_dt_layout_ptrs(layout);
                let obj16_end = obj16_begin.shift::<u16>(npointers as isize);

                while objary_begin < objary_end {
                    while obj16_begin < obj16_end {
                        let elem_begin_loaded = obj16_begin.load::<u16>();
                        let slot = objary_begin.shift::<Address>(elem_begin_loaded as isize);
                        process_slot(closure, slot);
                        obj16_begin = obj16_begin.shift::<u16>(1);
                    }
                    obj16_begin = mmtk_jl_dt_layout_ptrs(layout);
                    objary_begin = objary_begin.shift::<Address>(elsize as isize);
                }
            } else {
                unimplemented!();
            }
        }

        return;
    }

    if PRINT_OBJ_TYPE {
        println!("scan_julia_obj {}: datatype\n", obj);
    }

    if vt == jl_weakref_type {
        return;
    }

    let layout = (*vt).layout;
    let npointers = (*layout).npointers;
    if npointers == 0 {
        return;
    } else {
        if vt == jl_binding_partition_type {
            let bpart_ptr = obj.to_mut_ptr::<jl_binding_partition_t>();
            let restriction = (*bpart_ptr).restriction._M_i;
            let offset = mmtk_decode_restriction_value(restriction);
            let slot = Address::from_ptr(::std::ptr::addr_of!((*bpart_ptr).restriction));
            if restriction - offset != 0 {
                process_offset_slot(closure, slot, offset);
            }
        }
        debug_assert!(
            (*layout).nfields > 0 && (*layout).fielddesc_type_custom() != 3,
            "opaque types should have been handled specially"
        );
        if (*layout).fielddesc_type_custom() == 0 {
            let mut obj8_begin = mmtk_jl_dt_layout_ptrs(layout);
            let obj8_end = obj8_begin.shift::<u8>(npointers as isize);

            while obj8_begin < obj8_end {
                let obj8_begin_loaded = obj8_begin.load::<u8>();
                let slot = obj.shift::<Address>(obj8_begin_loaded as isize);
                process_slot(closure, slot);
                obj8_begin = obj8_begin.shift::<u8>(1);
            }
        } else if (*layout).fielddesc_type_custom() == 1 {
            let mut obj16_begin = mmtk_jl_dt_layout_ptrs(layout);
            let obj16_end = obj16_begin.shift::<u16>(npointers as isize);

            while obj16_begin < obj16_end {
                let obj16_begin_loaded = obj16_begin.load::<u16>();
                let slot = obj.shift::<Address>(obj16_begin_loaded as isize);
                process_slot(closure, slot);
                obj16_begin = obj16_begin.shift::<u16>(1);
            }
        } else if (*layout).fielddesc_type_custom() == 2 {
            let mut obj32_begin = mmtk_jl_dt_layout_ptrs(layout);
            let obj32_end = obj32_begin.shift::<u32>(npointers as isize);

            while obj32_begin < obj32_end {
                let obj32_begin_loaded = obj32_begin.load::<u32>();
                let slot = obj.shift::<Address>(obj32_begin_loaded as isize);
                process_slot(closure, slot);
                obj32_begin = obj32_begin.shift::<u32>(1);
            }
        } else {
            debug_assert!((*layout).fielddesc_type_custom() == 3);
            unimplemented!();
        }
    }
}

#[inline(always)]
unsafe fn mmtk_jl_genericmemory_data_owner_field_address(m: *const jl_genericmemory_t) -> Address {
    unsafe { jl_gc_get_owner_address_to_mmtk(Address::from_ptr(m)) }
}

// #[inline(always)]
// unsafe fn mmtk_jl_genericmemory_data_owner_field(
//     m: *const mmtk_jl_genericmemory_t,
// ) -> *const mmtk_jl_value_t {
//     mmtk_jl_genericmemory_data_owner_field_address(m).load::<*const mmtk_jl_value_t>()
// }

pub unsafe fn mmtk_scan_gcstack<EV: SlotVisitor<JuliaVMSlot>>(
    ta: *const jl_task_t,
    closure: &mut EV,
) {
    let stkbuf = (*ta).ctx.stkbuf;
    let copy_stack = (*ta).ctx.copy_stack_custom();

    #[cfg(feature = "julia_copy_stack")]
    if stkbuf != std::ptr::null_mut() && copy_stack != 0 {
        let stkbuf_slot = Address::from_ptr(::std::ptr::addr_of!((*ta).ctx.stkbuf));
        process_slot(closure, stkbuf_slot);
    }

    let mut s = (*ta).gcstack;
    let (mut offset, mut lb, mut ub) = (0 as isize, 0 as u64, u64::MAX);

    #[cfg(feature = "julia_copy_stack")]
    if stkbuf != std::ptr::null_mut() && copy_stack != 0 && (*ta).ptls == std::ptr::null_mut() {
        if ((*ta).tid._M_i) < 0 {
            panic!("tid must be positive.")
        }
        let stackbase = jl_gc_get_stackbase((*ta).tid._M_i);
        ub = stackbase as u64;
        lb = ub - ((*ta).ctx.copy_stack() as u64);
        offset = (*ta).ctx.stkbuf as isize - lb as isize;
    }

    if s != std::ptr::null_mut() {
        let s_nroots_addr = ::std::ptr::addr_of!((*s).nroots);
        let mut nroots = read_stack(Address::from_ptr(s_nroots_addr), offset, lb, ub);
        debug_assert!(nroots.as_usize() as u32 <= std::u32::MAX);
        let mut nr = nroots >> 2;

        loop {
            let rts = Address::from_mut_ptr(s).shift::<Address>(2);
            let mut i = 0;
            while i < nr {
                if (nroots.as_usize() & 1) != 0 {
                    let slot = read_stack(rts.shift::<Address>(i as isize), offset, lb, ub);
                    let real_addr = get_stack_addr(slot, offset, lb, ub);
                    process_slot(closure, real_addr);
                } else {
                    let real_addr =
                        get_stack_addr(rts.shift::<Address>(i as isize), offset, lb, ub);

                    let slot = read_stack(rts.shift::<Address>(i as isize), offset, lb, ub);
                    use crate::julia_finalizer::gc_ptr_tag;
                    // malloced pointer tagged in jl_gc_add_quiescent
                    // skip both the next element (native function), and the object
                    if slot & 3usize == 3 {
                        i += 2;
                        continue;
                    }

                    // pointer is not malloced but function is native, so skip it
                    if gc_ptr_tag(slot, 1) {
                        process_offset_slot(closure, real_addr, 1);
                        i += 2;
                        continue;
                    }

                    process_slot(closure, real_addr);
                }

                i += 1;
            }

            let s_prev_address = ::std::ptr::addr_of!((*s).prev);
            let sprev = read_stack(Address::from_ptr(s_prev_address), offset, lb, ub);
            if sprev.is_zero() {
                break;
            }

            s = sprev.to_mut_ptr::<jl_gcframe_t>();
            let s_nroots_addr = ::std::ptr::addr_of!((*s).nroots);
            let new_nroots = read_stack(Address::from_ptr(s_nroots_addr), offset, lb, ub);
            nroots = new_nroots;
            nr = nroots >> 2;
            continue;
        }
    }

    // just call into C, since the code is cold
    if (*ta).excstack != std::ptr::null_mut() {
        jl_gc_scan_julia_exc_obj(
            Address::from_ptr(ta),
            Address::from_mut_ptr(closure),
            process_slot::<EV> as _,
        );
    }
}

#[inline(always)]
unsafe fn read_stack(addr: Address, offset: isize, lb: u64, ub: u64) -> Address {
    let real_addr = get_stack_addr(addr, offset, lb, ub);

    real_addr.load::<Address>()
}

#[inline(always)]
fn get_stack_addr(addr: Address, offset: isize, lb: u64, ub: u64) -> Address {
    if addr.as_usize() >= lb as usize && addr.as_usize() < ub as usize {
        return addr + offset;
    } else {
        return addr;
    }
}

#[inline(always)]
pub fn process_slot<EV: SlotVisitor<JuliaVMSlot>>(closure: &mut EV, slot: Address) {
    let simple_slot = SimpleSlot::from_address(slot);

    #[cfg(debug_assertions)]
    {
        use mmtk::vm::slot::Slot;

        if PRINT_OBJ_TYPE {
            println!(
                "\tprocess slot = {:?} - {:?}\n",
                simple_slot,
                simple_slot.load()
            );
        }

        if let Some(objref) = simple_slot.load() {
            debug_assert!(
                mmtk::memory_manager::is_in_mmtk_spaces(objref),
                "Object {:?} in slot {:?} is not mapped address",
                objref,
                simple_slot
            );

            let raw_addr_usize = objref.to_raw_address().as_usize();

            // captures wrong slots before creating the work
            debug_assert!(
                raw_addr_usize % 16 == 0 || raw_addr_usize % 8 == 0,
                "Object {:?} in slot {:?} is not aligned to 8 or 16",
                objref,
                simple_slot
            );
        }
    }

    closure.visit_slot(JuliaVMSlot::Simple(simple_slot));
}

// This is based on the function decode_restriction_value from julia_internal.h.
// However, since MMTk uses the slot to load the object, we get the offset from pku using
// that offset to pass to process_offset_edge and get the right address.
#[inline(always)]
pub fn mmtk_decode_restriction_value(pku: usize) -> usize {
    #[cfg(target_pointer_width = "64")]
    {
        // need to apply (pku & ~0x7) to get the object to be traced
        // so we use (pku & 0x7) to get the offset from the object
        // and pass it to process_offset_slot
        return pku & 0x7;
    }

    #[cfg(not(target_pointer_width = "64"))]
    {
        // when not #ifdef _P64 we simply return pku.val
        // i.e., the offset is 0, since val is the first field
        return 0;
    }
}

#[inline(always)]
pub fn process_offset_slot<EV: SlotVisitor<JuliaVMSlot>>(
    closure: &mut EV,
    slot: Address,
    offset: usize,
) {
    let offset_slot = OffsetSlot::new_with_offset(slot, offset);
    #[cfg(debug_assertions)]
    {
        use mmtk::vm::slot::Slot;

        if let Some(objref) = offset_slot.load() {
            debug_assert!(
                mmtk::memory_manager::is_in_mmtk_spaces(objref),
                "Object {:?} in slot {:?} is not mapped address",
                objref,
                offset_slot
            );
        }
    }

    closure.visit_slot(JuliaVMSlot::Offset(offset_slot));
}

#[inline(always)]
pub fn mmtk_jl_array_ndimwords(ndims: u32) -> usize {
    if ndims < 3 {
        return 0;
    }

    return (ndims - 2) as usize;
}

#[inline(always)]
pub unsafe fn mmtk_jl_svec_len(obj: Address) -> usize {
    (*obj.to_ptr::<jl_svec_t>()).length
}

#[inline(always)]
pub unsafe fn mmtk_jl_svec_data(obj: Address) -> Address {
    obj + std::mem::size_of::<crate::julia_scanning::jl_svec_t>()
}

#[inline(always)]
pub unsafe fn mmtk_jl_tparam0(vt: *const jl_datatype_t) -> *const jl_datatype_t {
    mmtk_jl_svecref((*vt).parameters, 0)
}

#[inline(always)]
pub unsafe fn mmtk_jl_svecref(vt: *mut jl_svec_t, i: usize) -> *const jl_datatype_t {
    debug_assert!(
        mmtk_jl_typetagof(Address::from_mut_ptr(vt)).as_usize()
            == (jl_small_typeof_tags_jl_simplevector_tag << 4) as usize
    );
    debug_assert!(i < mmtk_jl_svec_len(Address::from_mut_ptr(vt)));

    let svec_data = mmtk_jl_svec_data(Address::from_mut_ptr(vt));
    let result_ptr = svec_data + i;
    let result = result_ptr.atomic_load::<AtomicUsize>(Ordering::Relaxed);
    ::std::mem::transmute::<usize, *const jl_datatype_t>(result)
}

#[inline(always)]
pub unsafe fn mmtk_jl_dt_layout_ptrs(l: *const jl_datatype_layout_t) -> Address {
    mmtk_jl_dt_layout_fields(l)
        + (mmtk_jl_fielddesc_size((*l).fielddesc_type_custom()) * (*l).nfields) as usize
}

#[inline(always)]
pub unsafe fn mmtk_jl_dt_layout_fields(l: *const jl_datatype_layout_t) -> Address {
    Address::from_ptr(l) + std::mem::size_of::<jl_datatype_layout_t>()
}

#[inline(always)]
pub unsafe fn mmtk_jl_fielddesc_size(fielddesc_type: u16) -> u32 {
    debug_assert!(fielddesc_type <= 2);
    2 << fielddesc_type
}

const JL_BT_NON_PTR_ENTRY: usize = usize::MAX;

pub fn mmtk_jl_bt_is_native(bt_entry: *mut jl_bt_element_t) -> bool {
    let entry = unsafe { (*bt_entry).__bindgen_anon_1.uintptr };
    entry != JL_BT_NON_PTR_ENTRY
}

pub fn mmtk_jl_bt_entry_size(bt_entry: *mut jl_bt_element_t) -> usize {
    if mmtk_jl_bt_is_native(bt_entry) {
        1
    } else {
        2 + mmtk_jl_bt_num_jlvals(bt_entry) + mmtk_jl_bt_num_uintvals(bt_entry)
    }
}

pub fn mmtk_jl_bt_num_jlvals(bt_entry: *mut jl_bt_element_t) -> usize {
    debug_assert!(!mmtk_jl_bt_is_native(bt_entry));
    let entry = unsafe { (*bt_entry.add(1)).__bindgen_anon_1.uintptr };
    entry & 0x7
}

pub fn mmtk_jl_bt_num_uintvals(bt_entry: *mut jl_bt_element_t) -> usize {
    debug_assert!(!mmtk_jl_bt_is_native(bt_entry));
    let entry = unsafe { (*bt_entry.add(1)).__bindgen_anon_1.uintptr };
    (entry >> 3) & 0x7
}

pub fn mmtk_jl_bt_entry_jlvalue(bt_entry: *mut jl_bt_element_t, i: usize) -> ObjectReference {
    let entry = unsafe { (*bt_entry.add(2 + i)).__bindgen_anon_1.jlvalue };
    debug_assert!(!entry.is_null());
    unsafe { ObjectReference::from_raw_address_unchecked(Address::from_mut_ptr(entry)) }
}
