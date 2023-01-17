use crate::julia_types::*;
use crate::JuliaVM;
use mmtk::scheduler::ProcessEdgesWork;
use mmtk::util::opaque_pointer::*;
use mmtk::util::{Address, ObjectReference};
use mmtk::vm::Finalizable;
use mmtk::vm::ReferenceGlue;

extern "C" {
    static jl_nothing: *mut mmtk_jl_value_t;
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
            trace.trace_object(ObjectReference::from_raw_address(self.1));
        }
    }
}

pub struct VMReferenceGlue {}

impl ReferenceGlue<JuliaVM> for VMReferenceGlue {
    type FinalizableType = JuliaFinalizableObject;
    fn set_referent(reference: ObjectReference, referent: ObjectReference) {
        unsafe {
            let mut reff = reference.to_raw_address().to_mut_ptr::<mmtk_jl_weakref_t>();
            let referent_raw = referent.to_raw_address().to_mut_ptr::<mmtk_jl_value_t>();
            (*reff).value = referent_raw;
        }
    }

    fn clear_referent(new_reference: ObjectReference) {
        Self::set_referent(new_reference, unsafe {
            ObjectReference::from_raw_address(Address::from_mut_ptr(jl_nothing))
        });
    }

    fn get_referent(object: ObjectReference) -> ObjectReference {
        let referent;
        unsafe {
            let reff = object.to_raw_address().to_mut_ptr::<mmtk_jl_weakref_t>();
            referent = ObjectReference::from_raw_address(Address::from_mut_ptr((*reff).value));
        }
        referent
    }

    fn is_referent_cleared(referent: ObjectReference) -> bool {
        unsafe { referent.to_raw_address().to_mut_ptr() == jl_nothing }
    }

    fn enqueue_references(_references: &[ObjectReference], _tls: VMWorkerThread) {}
}
