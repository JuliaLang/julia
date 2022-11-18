use mmtk::util:: {Address, ObjectReference};
use mmtk::vm::EdgeVisitor;
use crate::edges::JuliaVMEdge;
use crate::UPCALLS;
use crate::scanning::*;
use crate::julia_types::*;
use std::sync::atomic::Ordering;
use std::sync::atomic::AtomicUsize;
use mmtk::vm::edge_shape::SimpleEdge;
use crate::edges::OffsetEdge;

const HT_NOTFOUND: usize = 1;
const JL_BUFF_TAG: usize = 0x4eadc000; // from vm/julia/julia_internal

extern "C" {
    static jl_simplevector_type: *const mmtk_jl_datatype_t;
    static jl_array_typename: *mut mmtk_jl_typename_t;
    static jl_module_type: *const mmtk_jl_datatype_t;
    static jl_task_type: *const mmtk_jl_datatype_t;
    static jl_string_type: *const mmtk_jl_datatype_t;
    static jl_weakref_type: *const mmtk_jl_datatype_t;
}

pub unsafe fn mmtk_jl_typeof(addr: Address) -> Address {
    let as_tagged_value = addr.as_usize() - std::mem::size_of::<crate::julia_scanning::mmtk_jl_taggedvalue_t>();
    let t_header = Address::from_usize(as_tagged_value).load::<Address>();
    let t = t_header.as_usize() & !15; 

    Address::from_usize(t)
}

pub unsafe fn scan_julia_object(addr: Address, closure : &mut dyn EdgeVisitor<JuliaVMEdge>) {
    // get Julia object type
    let obj_type_addr = mmtk_jl_typeof(addr);
    let obj_type = obj_type_addr.to_ptr::<mmtk_jl_datatype_t>();

    if obj_type_addr.as_usize() == JL_BUFF_TAG {
        return;
    }

    if obj_type == jl_simplevector_type {
        let length = (*addr.to_ptr::<mmtk_jl_svec_t>()).length as usize;

        let start_addr = addr + std::mem::size_of::<crate::julia_scanning::mmtk_jl_svec_t>();
        let end_addr = start_addr + (length * std::mem::size_of::<Address>());
        for addr_usize in (start_addr.as_usize()..end_addr.as_usize()).step_by(std::mem::size_of::<Address>()) {
            let addr = Address::from_usize(addr_usize);
            process_edge(closure, addr);
        }
    } else if (*obj_type).name == jl_array_typename {
        let flags = &(*addr.to_ptr::<mmtk_jl_array_t>()).flags;
        let data = (*addr.to_ptr::<mmtk_jl_array_t>()).data;
        let length = (*addr.to_ptr::<mmtk_jl_array_t>()).length;

        if flags.how() == 1 { // julia-allocated buffer that needs to be marked
            let offset = ((*addr.to_ptr::<mmtk_jl_array_t>()).offset as u16 * (*addr.to_ptr::<mmtk_jl_array_t>()).elsize) as usize;
            process_offset_edge(closure, addr, offset);
        } else if flags.how() == 3 { // has a pointer to the object that owns the data
            #[allow(deref_nullptr)]
            let offset_ncols = &(*(::std::ptr::null::<mmtk_jl_array_t>())).__bindgen_anon_1 as *const _ as usize;
            let a_ndims = flags.ndims();
            let owner_offset = offset_ncols + std::mem::size_of::<::std::os::raw::c_ulong>() * (1 + mmtk_jl_array_ndimwords(a_ndims));
            let owner_address = addr + owner_offset;

            process_edge(closure, owner_address);
            return;
        } 

        if data == std::ptr::null_mut() || length == 0 {
            return;
        }

        if flags.ptrarray() != 0 {
            let start_addr = Address::from_mut_ptr(data);
            let end_addr = start_addr + (length as usize * std::mem::size_of::<Address>());
            for addr_usize in (start_addr.as_usize()..end_addr.as_usize()).step_by(std::mem::size_of::<Address>()) {
                let addr = Address::from_usize(addr_usize);
                process_edge(closure, addr);
            }
        } else if flags.hasptr() != 0 {
            let elem_type_ptr = Address::from_mut_ptr((*obj_type).parameters) + std::mem::size_of::<crate::julia_scanning::mmtk_jl_svec_t>();
            let elem_type_addr = Address::from_usize(elem_type_ptr.atomic_load::<AtomicUsize>(Ordering::Relaxed));
            let elem_type = elem_type_addr.to_mut_ptr::<mmtk_jl_datatype_t>();
            let layout = (*elem_type).layout;
            let npointers = (*layout).npointers;
            let elsize = (*addr.to_ptr::<mmtk_jl_array_t>()).elsize as usize / std::mem::size_of::<Address>();

            let start_addr = Address::from_mut_ptr(data);
            let end_addr = start_addr + length as usize * elsize * std::mem::size_of::<Address>();

            if npointers == 1 {
                let start_addr = start_addr + (*layout).first_ptr as usize * std::mem::size_of::<Address>();

                for addr_usize in (start_addr.as_usize()..end_addr.as_usize()).step_by(elsize * std::mem::size_of::<Address>()) {
                    let addr = Address::from_usize(addr_usize);
                    process_edge(closure, addr);
                }
            } else if (*layout).fielddesc_type() == 0 {
                let layout_fields = Address::from_ptr(layout) + std::mem::size_of::<mmtk_jl_datatype_layout_t>();
                let fielddesc_size = 2 << (*layout).fielddesc_type();
                let obj8_start_addr = layout_fields + fielddesc_size * (*layout).nfields as usize;
                let obj8_end_addr = obj8_start_addr + npointers as usize;

                for addr_usize in (start_addr.as_usize()..end_addr.as_usize()).step_by(elsize * std::mem::size_of::<Address>()) {
                    for elem_usize in obj8_start_addr.as_usize()..obj8_end_addr.as_usize() {
                        let elem = Address::from_usize(addr_usize).to_mut_ptr::<*mut libc::c_uchar>();
                        let index = Address::from_usize(elem_usize).to_mut_ptr::<libc::c_uchar>();
                        let slot: *mut libc::c_uchar = &mut *elem.offset(*index as isize) as *mut *mut libc::c_uchar as *mut libc::c_uchar;
                        process_edge(closure, Address::from_mut_ptr(slot));
                    }
                }
            } else {
                unimplemented!();
            }
        }
    } else if obj_type == jl_module_type {
        let m = addr.to_ptr::<mmtk_jl_module_t>();
        let bsize = (*m).bindings.size;
        let begin = Address::from_mut_ptr((*m).bindings.table) + std::mem::size_of::<Address>() as usize;
        let end = Address::from_mut_ptr((*m).bindings.table) + bsize as usize * std::mem::size_of::<Address>();

        for addr_usize in (begin.as_usize()..end.as_usize()).step_by(2 * std::mem::size_of::<Address>()) {
            let b = Address::from_usize(Address::from_usize(addr_usize).load::<usize>()).to_mut_ptr::<mmtk_jl_binding_t>();
            let b_addr = Address::from_mut_ptr(b);

            if b_addr.as_usize() == HT_NOTFOUND {
                continue;
            }
            
            if !b_addr.is_zero() && object_is_managed_by_mmtk(b_addr.as_usize()) {
                process_edge(closure, Address::from_usize(addr_usize));
            }

            let value = ::std::ptr::addr_of!((*b).value);
            let globalref = ::std::ptr::addr_of!((*b).globalref);

            process_edge(closure, Address::from_usize(value as usize));
            process_edge(closure, Address::from_usize(globalref as usize));
        }

        let parent_edge = ::std::ptr::addr_of!((*m).parent);
        process_edge(closure, Address::from_usize(parent_edge as usize));

        let nusings = (*m).usings.len;
        if nusings != 0 {
            let begin = Address::from_mut_ptr((*m).usings.items);
            let end = begin + (nusings as usize * std::mem::size_of::<Address>());

            for addr_usize in (begin.as_usize()..end.as_usize()).step_by(std::mem::size_of::<Address>()) {
                let addr = Address::from_usize(addr_usize);
                process_edge(closure, addr);
            }
        }
    } else if obj_type == jl_task_type {
        let ta = addr.to_ptr::<mmtk_jl_task_t>();
        let stkbuf = (*ta).stkbuf;
        let stkbuf_addr = Address::from_mut_ptr(stkbuf);
        let copy_stack = (*ta).copy_stack();
        // FIXME: the code below is executed COPY_STACKS has been defined in the C Julia implementation - it is on by default
        if !stkbuf_addr.is_zero() && copy_stack != 0 && object_is_managed_by_mmtk(stkbuf_addr.as_usize()) {
            let stkbuf_edge = Address::from_ptr(::std::ptr::addr_of!((*ta).stkbuf));
            process_edge(closure, stkbuf_edge);
        }
        let mut s = (*ta).gcstack;
        
        let (mut offset, mut lb, mut ub) = (0, 0, usize::MAX);
        // FIXME: the code below is executed COPY_STACKS has been defined in the C Julia implementation - it is on by default
        if !stkbuf_addr.is_zero() && copy_stack != 0 && (*ta).ptls != std::ptr::null_mut() {
            if ((*ta).tid as i16) < 0 {
                panic!("tid must be positive.")
            }
            let stackbase = ((*UPCALLS).get_stackbase)((*ta).tid);
            ub = stackbase as usize;
            lb = ub - (*ta).copy_stack() as usize;
            offset = Address::from_mut_ptr(stkbuf).as_usize() - lb as usize;
        }
        if s != std::ptr::null_mut() {
            let s_nroots_addr = ::std::ptr::addr_of!((*s).nroots);
            let nroots_addr = read_stack(Address::from_ptr(s_nroots_addr), offset, lb, ub);
            let mut nroots = nroots_addr.load::<usize>();
            let mut nr = nroots >> 2;

            loop {
                let rts = Address::from_mut_ptr(s) + 2 * std::mem::size_of::<Address>() as usize;                
                for i in 0..nr {
                    if (nroots & 1) != 0 {
                        let slot = read_stack(rts + (i * std::mem::size_of::<Address>()), offset, lb, ub);
                        let real_addr = read_stack(slot.load::<Address>(), offset, lb, ub);
                        process_edge(closure, real_addr);
                    } else {
                        let slot = read_stack(rts + (i * std::mem::size_of::<Address>()), offset, lb, ub);
                        process_edge(closure, slot);
                    }
                }

                let new_s = read_stack(Address::from_mut_ptr((*s).prev), offset, lb, ub);
                s = new_s.to_mut_ptr::<mmtk_jl_gcframe_t>();
                if s != std::ptr::null_mut() {
                    let s_nroots_addr = ::std::ptr::addr_of!((*s).nroots);
                    nroots = read_stack(Address::from_ptr(s_nroots_addr), offset, lb, ub).load();
                    nr = nroots >> 2;
                    continue;
                }
                break;
            }
        }
        // just call into C, since the code is cold
        if (*ta).excstack != std::ptr::null_mut() {
            ((*UPCALLS).scan_julia_exc_obj)(addr, closure, process_edge as _);       
        }

        let layout = (*jl_task_type).layout;
        let npointers = (*layout).npointers;
        let layout_fields = Address::from_ptr(layout) + std::mem::size_of::<mmtk_jl_datatype_layout_t>();
        let fielddesc_size = 2 << (*layout).fielddesc_type();
        let obj8_start_addr = layout_fields + fielddesc_size * (*layout).nfields as usize;
        let obj8_end_addr = obj8_start_addr + npointers as usize;
        for elem_usize in obj8_start_addr.as_usize()..obj8_end_addr.as_usize() {
            let elem = addr.to_mut_ptr::<*mut libc::c_uchar>();
            let index = Address::from_usize(elem_usize).to_mut_ptr::<libc::c_uchar>();
            let slot: *mut libc::c_uchar = &mut *elem.offset(*index as isize) as *mut *mut libc::c_uchar as *mut libc::c_uchar;
            process_edge(closure, Address::from_mut_ptr(slot));
        }
    } else if obj_type == jl_string_type || obj_type == jl_weakref_type {
        return;
    } else {
        let layout = (*obj_type).layout;
        let npointers = (*layout).npointers;
        if npointers == 0 {
            return;
        } else {
            assert!((*layout).nfields > 0 && (*layout).fielddesc_type() != 3);
            if (*layout).fielddesc_type() == 0 {
                let layout_fields = Address::from_ptr(layout) + std::mem::size_of::<mmtk_jl_datatype_layout_t>();
                let fielddesc_size = 2 << (*layout).fielddesc_type();
                let obj8_start_addr = layout_fields + fielddesc_size * (*layout).nfields as usize;
                let obj8_end_addr = obj8_start_addr + npointers as usize;
                for elem_usize in obj8_start_addr.as_usize()..obj8_end_addr.as_usize() {
                    let elem = addr.to_mut_ptr::<*mut libc::c_uchar>();
                    let index = Address::from_usize(elem_usize).to_mut_ptr::<libc::c_uchar>();
                    let slot: *mut libc::c_uchar = &mut *elem.offset(*index as isize) as *mut *mut libc::c_uchar as *mut libc::c_uchar;
                    process_edge(closure, Address::from_mut_ptr(slot));
                }
            } else if (*layout).fielddesc_type() == 1 {
                let layout_fields = Address::from_ptr(layout) + std::mem::size_of::<mmtk_jl_datatype_layout_t>();
                let fielddesc_size = 2 << (*layout).fielddesc_type();
                let obj16_start_addr = layout_fields + fielddesc_size * (*layout).nfields as usize;
                let obj16_end_addr = obj16_start_addr + (npointers as usize * 2);
                for elem_usize in (obj16_start_addr.as_usize()..obj16_end_addr.as_usize()).step_by(2) {
                    let elem = addr.to_mut_ptr::<*mut libc::c_uchar>();
                    let index = Address::from_usize(elem_usize).load::<u16>();
                    let slot: *mut libc::c_uchar = &mut *elem.offset(index as isize) as *mut *mut libc::c_uchar as *mut libc::c_uchar;
                    process_edge(closure, Address::from_mut_ptr(slot));
                }
            } else if (*layout).fielddesc_type() == 2 {
                unimplemented!();
            } else {
                assert_eq!((*layout).fielddesc_type(), 3);
            }
        }
    }
}

#[inline(always)]
fn read_stack(addr : Address, offset : usize, lb : usize, ub: usize) -> Address {
    if addr.as_usize() >= lb && addr.as_usize() < ub {
        return addr + offset;
    } else {
        return addr;
    }
}

#[inline(always)]
pub fn process_edge(closure : &mut dyn EdgeVisitor<JuliaVMEdge>, slot: Address) {
    let internal_obj: ObjectReference = unsafe { slot.load() };
    let internal_obj_addr = internal_obj.to_address();
    if internal_obj_addr.is_zero() {
        return;
    }

    let simple_edge = SimpleEdge::from_address(slot);

    if object_is_managed_by_mmtk(internal_obj_addr.as_usize()) {
        closure.visit_edge(JuliaVMEdge::Simple(simple_edge));
    } else {
        unsafe {
            let has_been_scanned = ((*UPCALLS).julia_object_has_been_scanned)(internal_obj_addr);
            if has_been_scanned == 0 {
                ((*UPCALLS).mark_julia_object_as_scanned)(internal_obj_addr);
                closure.visit_edge(JuliaVMEdge::Simple(simple_edge));
            }
        }            
    }             
}

#[inline(always)]
pub fn process_offset_edge(closure : &mut dyn EdgeVisitor<JuliaVMEdge>, slot: Address, offset: usize) {
    let internal_obj: ObjectReference = unsafe { slot.load() };
    let internal_obj_addr = internal_obj.to_address();
    if internal_obj_addr.is_zero() {
        return;
    }

    let offset_edge = OffsetEdge::new_with_offset(slot, offset);

    if object_is_managed_by_mmtk(internal_obj_addr.as_usize()) {
        closure.visit_edge(JuliaVMEdge::Offset(offset_edge));
    } else {
        unsafe {
            let has_been_scanned = ((*UPCALLS).julia_object_has_been_scanned)(internal_obj_addr);
            if has_been_scanned == 0 {
                ((*UPCALLS).mark_julia_object_as_scanned)(internal_obj_addr);
                closure.visit_edge(JuliaVMEdge::Offset(offset_edge));
            }
        }            
    }             
}



#[inline(always)]
fn mmtk_jl_array_ndimwords(ndims : u16) -> usize {
    if ndims < 3 {
        return 0;
    }
    
    return (ndims - 2) as usize;
}
