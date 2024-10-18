use crate::JuliaVM;
use crate::MUTATORS;
use mmtk::util::opaque_pointer::*;
use mmtk::util::Address;
use mmtk::vm::ActivePlan;
use mmtk::Mutator;
use mmtk::{plan::ObjectQueue, scheduler::GCWorker, util::ObjectReference};

use std::collections::HashMap;
use std::sync::RwLockReadGuard;

pub struct JuliaMutatorIterator<'a> {
    // We do not use this field, but this lock guard makes sure that no concurrent access to MUTATORS.
    _guard: RwLockReadGuard<'a, HashMap<Address, Address>>,
    vec: Vec<Address>,
    cursor: usize,
}

impl<'a> JuliaMutatorIterator<'a> {
    fn new(guard: RwLockReadGuard<'a, HashMap<Address, Address>>) -> Self {
        let vec = guard.keys().map(|addr| *addr).collect();
        Self {
            _guard: guard,
            vec,
            cursor: 0,
        }
    }
}

impl<'a> Iterator for JuliaMutatorIterator<'a> {
    type Item = &'a mut Mutator<JuliaVM>;

    fn next(&mut self) -> Option<Self::Item> {
        let mutator_idx = self.cursor;
        self.cursor += 1;

        self.vec
            .get(mutator_idx)
            .map(|addr| unsafe { &mut *(addr.to_mut_ptr::<Mutator<JuliaVM>>()) })
    }
}

pub struct VMActivePlan {}

impl ActivePlan<JuliaVM> for VMActivePlan {
    fn number_of_mutators() -> usize {
        Self::mutators().count()
    }

    fn is_mutator(tls: VMThread) -> bool {
        // FIXME have a tls field to check whether it is a mutator tls
        MUTATORS.read().unwrap().keys().find(|mutator_addr| unsafe { &*mutator_addr.to_mut_ptr::<Mutator<JuliaVM>>() }.mutator_tls.0 == tls).is_some()
    }

    fn mutator(_tls: VMMutatorThread) -> &'static mut Mutator<JuliaVM> {
        unimplemented!()
    }

    fn mutators<'a>() -> Box<dyn Iterator<Item = &'a mut Mutator<JuliaVM>> + 'a> {
        let guard = MUTATORS.read().unwrap();
        Box::new(JuliaMutatorIterator::new(guard))
    }

    fn vm_trace_object<Q: ObjectQueue>(
        queue: &mut Q,
        object: ObjectReference,
        _worker: &mut GCWorker<JuliaVM>,
    ) -> ObjectReference {
        queue.enqueue(object);
        return object;
    }
}

// Expose the mutator iterator so they can be used in C.

#[no_mangle]
pub extern "C" fn mmtk_new_mutator_iterator() -> *mut JuliaMutatorIterator<'static> {
    let guard = MUTATORS.read().unwrap();
    Box::into_raw(Box::new(JuliaMutatorIterator::new(guard)))
}

#[no_mangle]
pub extern "C" fn mmtk_get_next_mutator_tls(
    iter: *mut JuliaMutatorIterator<'static>,
) -> OpaquePointer {
    match unsafe { iter.as_mut() }.unwrap().next() {
        Some(m) => m.mutator_tls.0 .0,
        None => OpaquePointer::from_address(Address::ZERO),
    }
}

#[no_mangle]
pub extern "C" fn mmtk_close_mutator_iterator(iter: *mut JuliaMutatorIterator<'static>) {
    // The boxed pointer will get dropped
    let _to_drop = unsafe { Box::from_raw(iter) };
}
