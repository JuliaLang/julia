use crate::JuliaVM;
use crate::{
    get_mutator_from_ref, get_next_julia_mutator, reset_mutator_count, MUTATORS, MUTATOR_TLS,
    SINGLETON,
};
use mmtk::util::opaque_pointer::*;
use mmtk::util::Address;
use mmtk::vm::ActivePlan;
use mmtk::Mutator;
use mmtk::Plan;
use mmtk::{plan::ObjectQueue, scheduler::GCWorker, util::ObjectReference};

pub struct VMActivePlan {}

impl ActivePlan<JuliaVM> for VMActivePlan {
    fn global() -> &'static dyn Plan<VM = JuliaVM> {
        SINGLETON.get_plan()
    }

    fn number_of_mutators() -> usize {
        unimplemented!()
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

    fn reset_mutator_iterator() {
        unsafe { reset_mutator_count() }
    }

    fn get_next_mutator() -> Option<&'static mut Mutator<JuliaVM>> {
        let mutators = MUTATORS.read().unwrap();
        // println!("All mutators: {:?}", mutators.iter());

        let mutator_idx = unsafe { get_next_julia_mutator() };

        // println!("Next mutator is: {:?}", mutator_idx);

        let mutator = mutators.get(mutator_idx);

        let res = match mutator {
            Some(m) => {
                let mutator = unsafe { get_mutator_from_ref(*m) };
                //    println!("Next mutator is: {:?}", mutator);
                Some(unsafe { &mut *mutator })
            }
            None => None,
        };

        res
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

#[no_mangle]
pub extern "C" fn get_next_mutator_tls() -> OpaquePointer {
    let mutators = MUTATORS.read().unwrap();

    let mutator_idx = unsafe { get_next_julia_mutator() };
    let mutator = mutators.get(mutator_idx);

    let res = match mutator {
        Some(m) => {
            let mutator = unsafe { get_mutator_from_ref(*m) };

            unsafe { (*mutator).mutator_tls.0 .0 }
        }
        None => {
            unsafe { reset_mutator_count() }
            OpaquePointer::from_address(unsafe { Address::zero() })
        }
    };

    res
}

#[no_mangle]
pub extern "C" fn reset_count_tls() {
    unsafe { reset_mutator_count() }
}
