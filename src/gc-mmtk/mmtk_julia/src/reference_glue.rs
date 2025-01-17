use crate::julia_types::*;
use crate::JuliaVM;
use mmtk::scheduler::ProcessEdgesWork;
use mmtk::util::opaque_pointer::*;
use mmtk::util::{Address, ObjectReference};
use mmtk::vm::Finalizable;
use mmtk::vm::ReferenceGlue;

extern "C" {
    pub static jl_nothing: *mut jl_value_t;
}

#[derive(Copy, Clone, Eq, Hash, PartialOrd, PartialEq, Debug)]
pub struct JuliaFinalizableObject(pub ObjectReference, pub Address, pub bool);

impl Finalizable for JuliaFinalizableObject {
    #[inline(always)]
    fn get_reference(&self) -> ObjectReference {
        self.0
    }
    #[inline(always)]
    fn set_reference(&mut self, object: ObjectReference) {
        self.0 = object;
    }
    fn keep_alive<E: ProcessEdgesWork>(&mut self, trace: &mut E) {
        self.set_reference(trace.trace_object(self.get_reference()));
        if !self.2 {
            // not a void pointer
            debug_assert!(!self.1.is_zero());
            let objref = unsafe { ObjectReference::from_raw_address_unchecked(self.1) };
            trace.trace_object(objref);
        }
    }
}

pub struct VMReferenceGlue {}

impl VMReferenceGlue {
    fn load_referent_raw(reference: ObjectReference) -> *mut jl_value_t {
        let reff = reference.to_raw_address().to_ptr::<jl_weakref_t>();
        unsafe { (*reff).value }
    }

    fn set_referent_raw(reference: ObjectReference, referent_raw: *mut jl_value_t) {
        let reff = reference.to_raw_address().to_mut_ptr::<jl_weakref_t>();
        unsafe {
            (*reff).value = referent_raw;
        }
    }
}

impl ReferenceGlue<JuliaVM> for VMReferenceGlue {
    type FinalizableType = JuliaFinalizableObject;

    fn set_referent(reference: ObjectReference, referent: ObjectReference) {
        Self::set_referent_raw(reference, referent.to_raw_address().to_mut_ptr());
    }

    fn clear_referent(new_reference: ObjectReference) {
        Self::set_referent_raw(new_reference, unsafe { jl_nothing });
    }

    fn get_referent(object: ObjectReference) -> Option<ObjectReference> {
        let value = Self::load_referent_raw(object);
        if value == unsafe { jl_nothing } {
            None
        } else {
            debug_assert!(
                !value.is_null(),
                "A weak reference {} contains null referent pointer",
                object
            );
            let objref =
                unsafe { ObjectReference::from_raw_address_unchecked(Address::from_ptr(value)) };
            Some(objref)
        }
    }

    fn enqueue_references(_references: &[ObjectReference], _tls: VMWorkerThread) {}
}
