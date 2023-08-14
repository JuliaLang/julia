use crate::edges::JuliaVMEdge;
use crate::edges::OffsetEdge;
use crate::julia_types::*;
use crate::object_model::mmtk_jl_array_ndims;
use crate::JULIA_BUFF_TAG;
use crate::UPCALLS;
use mmtk::util::{Address, ObjectReference};
use mmtk::vm::edge_shape::SimpleEdge;
use mmtk::vm::EdgeVisitor;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

const JL_MAX_TAGS: usize = 64; // from vm/julia/src/jl_exports.h

extern "C" {
    pub static jl_simplevector_type: *const mmtk_jl_datatype_t;
    pub static jl_array_typename: *mut mmtk_jl_typename_t;
    pub static jl_module_type: *const mmtk_jl_datatype_t;
    pub static jl_task_type: *const mmtk_jl_datatype_t;
    pub static jl_string_type: *const mmtk_jl_datatype_t;
    pub static jl_weakref_type: *const mmtk_jl_datatype_t;
    pub static jl_symbol_type: *const mmtk_jl_datatype_t;
    pub static jl_method_type: *const mmtk_jl_datatype_t;
}

extern "C" {
    pub static mut small_typeof: [*mut mmtk_jl_datatype_t; 128usize];
}

#[inline(always)]
pub unsafe fn mmtk_jl_typetagof(addr: Address) -> Address {
    let as_tagged_value =
        addr.as_usize() - std::mem::size_of::<crate::julia_scanning::mmtk_jl_taggedvalue_t>();
    let t_header = Address::from_usize(as_tagged_value).load::<Address>();
    let t = t_header.as_usize() & !15;

    Address::from_usize(t)
}

#[inline(always)]
pub unsafe fn mmtk_jl_typeof(addr: Address) -> *const mmtk_jl_datatype_t {
    mmtk_jl_to_typeof(mmtk_jl_typetagof(addr))
}

#[inline(always)]
pub unsafe fn mmtk_jl_to_typeof(t: Address) -> *const mmtk_jl_datatype_t {
    let t_raw = t.as_usize();
    if t_raw < (JL_MAX_TAGS << 4) {
        let ty = small_typeof[t_raw / std::mem::size_of::<Address>()];
        return ty;
    }
    return t.to_ptr::<mmtk_jl_datatype_t>();
}

const PRINT_OBJ_TYPE: bool = false;

// This function is a rewrite of `gc_mark_outrefs()` in `gc.c`
// INFO: *_custom() functions are acessors to bitfields that do not use bindgen generated code.
#[inline(always)]
pub unsafe fn scan_julia_object<EV: EdgeVisitor<JuliaVMEdge>>(obj: Address, closure: &mut EV) {
    // get Julia object type
    let vt = mmtk_jl_typeof(obj);

    if vt == jl_symbol_type || vt as usize == JULIA_BUFF_TAG {
        return;
    }

    if vt == jl_simplevector_type {
        if PRINT_OBJ_TYPE {
            println!("scan_julia_obj {}: simple vector\n", obj);
        }
        let length = mmtk_jl_svec_len(obj);
        let mut objary_begin = mmtk_jl_svec_data(obj);
        let objary_end = objary_begin.shift::<Address>(length as isize);

        while objary_begin < objary_end {
            process_edge(closure, objary_begin);
            objary_begin = objary_begin.shift::<Address>(1);
        }
    } else if (*vt).name == jl_array_typename {
        if PRINT_OBJ_TYPE {
            println!("scan_julia_obj {}: array\n", obj);
        }

        let array = obj.to_ptr::<mmtk_jl_array_t>();
        let flags = (*array).flags;

        if flags.how_custom() == 1 {
            // julia-allocated buffer that needs to be marked
            let offset = (*array).offset as usize * (*array).elsize as usize;
            let data_addr = ::std::ptr::addr_of!((*array).data);
            process_offset_edge(closure, Address::from_ptr(data_addr), offset);
        } else if flags.how_custom() == 2 {
            // malloc-allocated pointer this array object manages
            // should be processed below if it contains pointers
        } else if flags.how_custom() == 3 {
            // has a pointer to the object that owns the data
            let owner_addr = mmtk_jl_array_data_owner_addr(array);
            process_edge(closure, owner_addr);
            return;
        }

        if (*array).data == std::ptr::null_mut() || mmtk_jl_array_len(array) == 0 {
            return;
        }

        if flags.ptrarray_custom() != 0 {
            if mmtk_jl_tparam0(vt) == jl_symbol_type {
                return;
            }

            let length = mmtk_jl_array_len(array);

            let mut objary_begin = Address::from_ptr((*array).data);
            let objary_end = objary_begin.shift::<Address>(length as isize);

            while objary_begin < objary_end {
                process_edge(closure, objary_begin);
                objary_begin = objary_begin.shift::<Address>(1);
            }
        } else if flags.hasptr_custom() != 0 {
            let et = mmtk_jl_tparam0(vt);
            let layout = (*et).layout;
            let npointers = (*layout).npointers;
            let elsize = (*array).elsize as usize / std::mem::size_of::<Address>();
            let length = mmtk_jl_array_len(array);
            let mut objary_begin = Address::from_ptr((*array).data);
            let objary_end = objary_begin.shift::<Address>((length * elsize) as isize);

            if npointers == 1 {
                objary_begin = objary_begin.shift::<Address>((*layout).first_ptr as isize);
                while objary_begin < objary_end {
                    process_edge(closure, objary_begin);
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
                        process_edge(closure, slot);
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
                        process_edge(closure, slot);
                        obj16_begin = obj16_begin.shift::<u16>(1);
                    }
                    obj16_begin = mmtk_jl_dt_layout_ptrs(layout);
                    objary_begin = objary_begin.shift::<Address>(elsize as isize);
                }
            } else {
                unimplemented!();
            }
        } else {
            return;
        }
    } else if vt == jl_module_type {
        if PRINT_OBJ_TYPE {
            println!("scan_julia_obj {}: module\n", obj);
        }

        let m = obj.to_ptr::<mmtk_jl_module_t>();
        let bindings = (*m).bindings;
        let table = mmtk_jl_svec_data(Address::from_mut_ptr(bindings));
        let bsize = mmtk_jl_svec_len(Address::from_mut_ptr(bindings));
        let mut begin = table.shift::<Address>(1);
        let end = table.shift::<Address>(bsize as isize);

        while begin < end {
            let b: *mut mmtk_jl_binding_t = begin.load::<*mut mmtk_jl_binding_t>();
            if b == crate::reference_glue::jl_nothing as *mut mmtk_jl_binding_t {
                begin = begin.shift::<Address>(1);
                continue;
            }
            if PRINT_OBJ_TYPE {
                println!(" - scan table: {}\n", obj);
            }

            process_edge(closure, begin);
            begin = begin.shift::<Address>(1);
        }

        let parent_edge = ::std::ptr::addr_of!((*m).parent);
        if PRINT_OBJ_TYPE {
            println!(" - scan parent: {:?}\n", parent_edge);
        }
        process_edge(closure, Address::from_ptr(parent_edge));

        let bindingkeyset_edge = ::std::ptr::addr_of!((*m).bindingkeyset);
        if PRINT_OBJ_TYPE {
            println!(" - scan bindingkeyset: {:?}\n", bindingkeyset_edge);
        }
        process_edge(closure, Address::from_ptr(bindingkeyset_edge));

        let bindings_edge = ::std::ptr::addr_of!((*m).bindings);
        if PRINT_OBJ_TYPE {
            println!(" - scan bindings: {:?}\n", bindings_edge);
        }
        process_edge(closure, Address::from_ptr(bindings_edge));

        let nusings = (*m).usings.len;
        if nusings != 0 {
            let mut objary_begin = Address::from_mut_ptr((*m).usings.items);
            let objary_end = objary_begin.shift::<Address>(nusings as isize);

            while objary_begin < objary_end {
                if PRINT_OBJ_TYPE {
                    println!(" - scan usings: {:?}\n", objary_begin);
                }
                process_edge(closure, objary_begin);
                objary_begin = objary_begin.shift::<Address>(1);
            }
        }
    } else if vt == jl_task_type {
        if PRINT_OBJ_TYPE {
            println!("scan_julia_obj {}: task\n", obj);
        }

        let ta = obj.to_ptr::<mmtk_jl_task_t>();

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
            process_edge(closure, slot);
            obj8_begin = obj8_begin.shift::<u8>(1);
        }
    } else if vt == jl_string_type {
        if PRINT_OBJ_TYPE {
            println!("scan_julia_obj {}: string\n", obj);
        }
        return;
    } else {
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
                    process_edge(closure, slot);
                    obj8_begin = obj8_begin.shift::<u8>(1);
                }
            } else if (*layout).fielddesc_type_custom() == 1 {
                let mut obj16_begin = mmtk_jl_dt_layout_ptrs(layout);
                let obj16_end = obj16_begin.shift::<u16>(npointers as isize);

                while obj16_begin < obj16_end {
                    let obj16_begin_loaded = obj16_begin.load::<u16>();
                    let slot = obj.shift::<Address>(obj16_begin_loaded as isize);
                    process_edge(closure, slot);
                    obj16_begin = obj16_begin.shift::<u16>(1);
                }
            } else if (*layout).fielddesc_type_custom() == 2 {
                let mut obj32_begin = mmtk_jl_dt_layout_ptrs(layout);
                let obj32_end = obj32_begin.shift::<u32>(npointers as isize);

                while obj32_begin < obj32_end {
                    let obj32_begin_loaded = obj32_begin.load::<u32>();
                    let slot = obj.shift::<Address>(obj32_begin_loaded as isize);
                    process_edge(closure, slot);
                    obj32_begin = obj32_begin.shift::<u32>(1);
                }
            } else {
                debug_assert!((*layout).fielddesc_type_custom() == 3);
                unimplemented!();
            }
        }
    }
}

pub unsafe fn mmtk_scan_gcstack<EV: EdgeVisitor<JuliaVMEdge>>(
    ta: *const mmtk_jl_task_t,
    closure: &mut EV,
) {
    let stkbuf = (*ta).stkbuf;
    let copy_stack = (*ta).copy_stack_custom();

    #[cfg(feature = "julia_copy_stack")]
    if stkbuf != std::ptr::null_mut() && copy_stack != 0 {
        let stkbuf_edge = Address::from_ptr(::std::ptr::addr_of!((*ta).stkbuf));
        process_edge(closure, stkbuf_edge);
    }

    let mut s = (*ta).gcstack;
    let (mut offset, mut lb, mut ub) = (0 as isize, 0 as u64, u64::MAX);

    #[cfg(feature = "julia_copy_stack")]
    if stkbuf != std::ptr::null_mut() && copy_stack != 0 && (*ta).ptls == std::ptr::null_mut() {
        if ((*ta).tid as i16) < 0 {
            panic!("tid must be positive.")
        }
        let stackbase = ((*UPCALLS).get_stackbase)((*ta).tid);
        ub = stackbase as u64;
        lb = ub - ((*ta).copy_stack() as u64);
        offset = (*ta).stkbuf as isize - lb as isize;
    }

    if s != std::ptr::null_mut() {
        let s_nroots_addr = ::std::ptr::addr_of!((*s).nroots);
        let mut nroots = read_stack(Address::from_ptr(s_nroots_addr), offset, lb, ub);
        debug_assert!(nroots.as_usize() as u32 <= UINT32_MAX);
        let mut nr = nroots >> 2;

        loop {
            let rts = Address::from_mut_ptr(s).shift::<Address>(2);
            let mut i = 0;
            while i < nr {
                if (nroots.as_usize() & 1) != 0 {
                    let slot = read_stack(rts.shift::<Address>(i as isize), offset, lb, ub);
                    let real_addr = get_stack_addr(slot, offset, lb, ub);
                    process_edge(closure, real_addr);
                } else {
                    let real_addr =
                        get_stack_addr(rts.shift::<Address>(i as isize), offset, lb, ub);
                    process_edge(closure, real_addr);
                }

                i += 1;
            }

            let s_prev_address = ::std::ptr::addr_of!((*s).prev);
            let sprev = read_stack(Address::from_ptr(s_prev_address), offset, lb, ub);
            if sprev.is_zero() {
                break;
            }

            s = sprev.to_mut_ptr::<mmtk_jl_gcframe_t>();
            let s_nroots_addr = ::std::ptr::addr_of!((*s).nroots);
            let new_nroots = read_stack(Address::from_ptr(s_nroots_addr), offset, lb, ub);
            nroots = new_nroots;
            nr = nroots >> 2;
            continue;
        }
    }

    // just call into C, since the code is cold
    if (*ta).excstack != std::ptr::null_mut() {
        ((*UPCALLS).scan_julia_exc_obj)(
            Address::from_ptr(ta),
            Address::from_mut_ptr(closure),
            process_edge::<EV> as _,
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

use mmtk::vm::edge_shape::Edge;

#[inline(always)]
pub fn process_edge<EV: EdgeVisitor<JuliaVMEdge>>(closure: &mut EV, slot: Address) {
    let simple_edge = SimpleEdge::from_address(slot);
    debug_assert!(
        simple_edge.load().is_null()
            || mmtk::memory_manager::is_mapped_address(simple_edge.load().to_raw_address()),
        "Object {:?} in slot {:?} is not mapped address",
        simple_edge.load(),
        simple_edge
    );
    closure.visit_edge(JuliaVMEdge::Simple(simple_edge));
}

// #[inline(always)]
// pub unsafe fn boot_image_object_has_been_scanned(obj: Address) -> u8 {
//     let obj_type_addr = mmtk_jl_typeof(obj);
//     let obj_type = obj_type_addr.to_ptr::<mmtk_jl_datatype_t>();

//     if obj_type == jl_symbol_type {
//         return 1;
//     }

//     if BI_METADATA_START_ALIGNED_DOWN == 0 {
//         return 0;
//     }

//     if obj.as_usize() < BI_METADATA_START_ALIGNED_DOWN
//         || obj.as_usize() >= BI_METADATA_END_ALIGNED_UP
//     {
//         return 0;
//     }

//     return check_metadata_scanned(obj);
// }

// #[inline(always)]
// pub unsafe fn boot_image_mark_object_as_scanned(obj: Address) {
//     if BI_METADATA_START_ALIGNED_DOWN == 0 {
//         return;
//     }

//     if obj.as_usize() < BI_METADATA_START_ALIGNED_DOWN
//         || obj.as_usize() >= BI_METADATA_END_ALIGNED_UP
//     {
//         return;
//     }

//     mark_metadata_scanned(obj);
// }

#[inline(always)]
pub fn process_offset_edge<EV: EdgeVisitor<JuliaVMEdge>>(
    closure: &mut EV,
    slot: Address,
    offset: usize,
) {
    let offset_edge = OffsetEdge::new_with_offset(slot, offset);
    debug_assert!(
        offset_edge.load().is_null()
            || mmtk::memory_manager::is_mapped_address(offset_edge.load().to_raw_address()),
        "Object {:?} in slot {:?} is not mapped address",
        offset_edge.load(),
        offset_edge
    );

    closure.visit_edge(JuliaVMEdge::Offset(offset_edge));
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
    (*obj.to_ptr::<mmtk_jl_svec_t>()).length
}

#[inline(always)]
pub unsafe fn mmtk_jl_svec_data(obj: Address) -> Address {
    obj + std::mem::size_of::<crate::julia_scanning::mmtk_jl_svec_t>()
}

#[inline(always)]
pub unsafe fn mmtk_jl_array_len(a: *const mmtk_jl_array_t) -> usize {
    (*a).length
}

#[inline(always)]
pub unsafe fn mmtk_jl_array_data_owner_addr(array: *const mmtk_jl_array_t) -> Address {
    Address::from_ptr(array) + mmtk_jl_array_data_owner_offset(mmtk_jl_array_ndims(array))
}

#[inline(always)]
pub unsafe fn mmtk_jl_array_data_owner_offset(ndims: u32) -> usize {
    // (offsetof(jl_array_t,ncols)
    #[allow(deref_nullptr)]
    let offset_ncols =
        &(*(::std::ptr::null::<mmtk_jl_array_t>())).__bindgen_anon_1 as *const _ as usize;

    // (offsetof(jl_array_t,ncols) + sizeof(size_t)*(1+jl_array_ndimwords(ndims))) in bytes
    let res = offset_ncols
        + std::mem::size_of::<::std::os::raw::c_ulong>() * (1 + mmtk_jl_array_ndimwords(ndims));
    res
}

#[inline(always)]
pub unsafe fn mmtk_jl_tparam0(vt: *const mmtk_jl_datatype_t) -> *const mmtk_jl_datatype_t {
    mmtk_jl_svecref((*vt).parameters, 0)
}

#[inline(always)]
pub unsafe fn mmtk_jl_svecref(vt: *mut mmtk_jl_svec_t, i: usize) -> *const mmtk_jl_datatype_t {
    debug_assert!(
        mmtk_jl_typetagof(Address::from_mut_ptr(vt)).as_usize()
            == (mmtk_jlsmall_typeof_tags_mmtk_jl_simplevector_tag << 4) as usize
    );
    debug_assert!(i < mmtk_jl_svec_len(Address::from_mut_ptr(vt)));

    let svec_data = mmtk_jl_svec_data(Address::from_mut_ptr(vt));
    let result_ptr = svec_data + i;
    let result = result_ptr.atomic_load::<AtomicUsize>(Ordering::Relaxed);
    ::std::mem::transmute::<usize, *const mmtk_jl_datatype_t>(result)
}

#[inline(always)]
pub unsafe fn mmtk_jl_dt_layout_ptrs(l: *const mmtk_jl_datatype_layout_t) -> Address {
    mmtk_jl_dt_layout_fields(l)
        + (mmtk_jl_fielddesc_size((*l).fielddesc_type_custom()) * (*l).nfields) as usize
}

#[inline(always)]
pub unsafe fn mmtk_jl_dt_layout_fields(l: *const mmtk_jl_datatype_layout_t) -> Address {
    Address::from_ptr(l) + std::mem::size_of::<mmtk_jl_datatype_layout_t>()
}

#[inline(always)]
pub unsafe fn mmtk_jl_fielddesc_size(fielddesc_type: u16) -> u32 {
    debug_assert!(fielddesc_type <= 2);
    2 << fielddesc_type
}

const JL_BT_NON_PTR_ENTRY: usize = usize::MAX;

pub fn mmtk_jl_bt_is_native(bt_entry: *mut mmtk_jl_bt_element_t) -> bool {
    let entry = unsafe { (*bt_entry).__bindgen_anon_1.uintptr };
    entry == JL_BT_NON_PTR_ENTRY
}

pub fn mmtk_jl_bt_entry_size(bt_entry: *mut mmtk_jl_bt_element_t) -> usize {
    if mmtk_jl_bt_is_native(bt_entry) {
        1
    } else {
        2 + mmtk_jl_bt_num_jlvals(bt_entry) + mmtk_jl_bt_num_jlvals(bt_entry)
    }
}

pub fn mmtk_jl_bt_num_jlvals(bt_entry: *mut mmtk_jl_bt_element_t) -> usize {
    debug_assert!(!mmtk_jl_bt_is_native(bt_entry));
    let entry = unsafe { (*bt_entry.add(1)).__bindgen_anon_1.uintptr };
    entry & 0x7
}

pub fn mmtk_jl_bt_num_uintvals(bt_entry: *mut mmtk_jl_bt_element_t) -> usize {
    debug_assert!(!mmtk_jl_bt_is_native(bt_entry));
    let entry = unsafe { (*bt_entry.add(1)).__bindgen_anon_1.uintptr };
    (entry >> 3) & 0x7
}

pub fn mmtk_jl_bt_entry_jlvalue(bt_entry: *mut mmtk_jl_bt_element_t, i: usize) -> ObjectReference {
    let entry = unsafe { (*bt_entry.add(2 + i)).__bindgen_anon_1.jlvalue };
    ObjectReference::from_raw_address(Address::from_mut_ptr(entry))
}
