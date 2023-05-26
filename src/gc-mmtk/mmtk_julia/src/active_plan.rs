use crate::JuliaVM;
use crate::{get_mutator_from_ref, MUTATORS, MUTATOR_TLS, SINGLETON};
use mmtk::util::opaque_pointer::*;
use mmtk::util::Address;
use mmtk::vm::ActivePlan;
use mmtk::Mutator;
use mmtk::Plan;
use mmtk::{plan::ObjectQueue, scheduler::GCWorker, util::ObjectReference};

use std::sync::RwLockReadGuard;

pub struct JuliaMutatorIterator<'a> {
    guard: RwLockReadGuard<'a, Vec<ObjectReference>>,
    cursor: usize,
}

impl<'a> JuliaMutatorIterator<'a> {
    fn new(guard: RwLockReadGuard<'a, Vec<ObjectReference>>) -> Self {
        Self {
            guard: guard,
            cursor: 0,
        }
    }
}

impl<'a> Iterator for JuliaMutatorIterator<'a> {
    type Item = &'a mut Mutator<JuliaVM>;

    fn next(&mut self) -> Option<Self::Item> {
        let ref mutators = self.guard;

        let mutator_idx = self.cursor;
        self.cursor += 1;

        let mutator = mutators.get(mutator_idx);

        match mutator {
            Some(m) => {
                let mutator = unsafe { get_mutator_from_ref(*m) };
                Some(unsafe { &mut *mutator })
            }
            None => None,
        }
    }
}

pub struct VMActivePlan {}

impl ActivePlan<JuliaVM> for VMActivePlan {
    fn global() -> &'static dyn Plan<VM = JuliaVM> {
        SINGLETON.get_plan()
    }

    fn number_of_mutators() -> usize {
        Self::mutators().count()
    }

    fn is_mutator(tls: VMThread) -> bool {
        // FIXME have a tls field to check whether it is a mutator tls
        let tls_str = format!("{:?}", tls);
        let is_mutator = MUTATOR_TLS.read().unwrap().contains(&tls_str);
        if !is_mutator {
            println!("Is the tls {:?} a mutator? {}", tls_str, is_mutator);
        }
        is_mutator
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
pub extern "C" fn new_mutator_iterator() -> *mut JuliaMutatorIterator<'static> {
    let guard = MUTATORS.read().unwrap();
    Box::into_raw(Box::new(JuliaMutatorIterator::new(guard)))
}

#[no_mangle]
pub extern "C" fn get_next_mutator_tls(iter: *mut JuliaMutatorIterator<'static>) -> OpaquePointer {
    match unsafe { iter.as_mut() }.unwrap().next() {
        Some(m) => m.mutator_tls.0 .0,
        None => OpaquePointer::from_address(Address::ZERO),
    }
}

#[no_mangle]
pub extern "C" fn close_mutator_iterator(iter: *mut JuliaMutatorIterator<'static>) {
    // The boxed pointer will get dropped
    let _to_drop = unsafe { Box::from_raw(iter) };
}
