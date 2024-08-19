use crate::{JuliaVM, USER_TRIGGERED_GC};
use crate::{SINGLETON, UPCALLS};
use log::{info, trace};
use mmtk::util::alloc::AllocationError;
use mmtk::util::opaque_pointer::*;
use mmtk::vm::{Collection, GCThreadContext};
use mmtk::Mutator;
use std::sync::atomic::{AtomicBool, AtomicIsize, AtomicU32, AtomicU64, Ordering};

use crate::{BLOCK_FOR_GC, STW_COND, WORLD_HAS_STOPPED};

static GC_START: AtomicU64 = AtomicU64::new(0);

extern "C" {
    pub static jl_gc_disable_counter: AtomicU32;
}

pub struct VMCollection {}

impl Collection<JuliaVM> for VMCollection {
    fn stop_all_mutators<F>(_tls: VMWorkerThread, mut mutator_visitor: F)
    where
        F: FnMut(&'static mut Mutator<JuliaVM>),
    {
        // Wait for all mutators to stop and all finalizers to run
        while !AtomicBool::load(&WORLD_HAS_STOPPED, Ordering::SeqCst) {
            // Stay here while the world has not stopped
            // FIXME add wait var
        }

        trace!("Stopped the world!");

        // Tell MMTk the stacks are ready.
        {
            use mmtk::vm::ActivePlan;
            for mutator in crate::active_plan::VMActivePlan::mutators() {
                info!("stop_all_mutators: visiting {:?}", mutator.mutator_tls);
                mutator_visitor(mutator);
            }
        }

        // Record the start time of the GC
        let now = unsafe { ((*UPCALLS).jl_hrtime)() };
        trace!("gc_start = {}", now);
        GC_START.store(now, Ordering::Relaxed);
    }

    fn resume_mutators(_tls: VMWorkerThread) {
        // Get the end time of the GC
        let end = unsafe { ((*UPCALLS).jl_hrtime)() };
        trace!("gc_end = {}", end);
        let gc_time = end - GC_START.load(Ordering::Relaxed);
        unsafe {
            ((*UPCALLS).update_gc_stats)(
                gc_time,
                crate::api::mmtk_used_bytes(),
                is_current_gc_nursery(),
            )
        }

        AtomicBool::store(&BLOCK_FOR_GC, false, Ordering::SeqCst);
        AtomicBool::store(&WORLD_HAS_STOPPED, false, Ordering::SeqCst);

        let &(_, ref cvar) = &*STW_COND.clone();
        cvar.notify_all();

        info!(
            "Live bytes = {}, total bytes = {}",
            crate::api::mmtk_used_bytes(),
            crate::api::mmtk_total_bytes()
        );

        trace!("Resuming mutators.");
    }

    fn block_for_gc(_tls: VMMutatorThread) {
        info!("Triggered GC!");

        unsafe { ((*UPCALLS).prepare_to_collect)() };

        info!("Finished blocking mutator for GC!");
    }

    fn spawn_gc_thread(_tls: VMThread, ctx: GCThreadContext<JuliaVM>) {
        // Just drop the join handle. The thread will run until the process quits.
        let _ = std::thread::spawn(move || {
            use mmtk::util::opaque_pointer::*;
            use mmtk::util::Address;
            let worker_tls = VMWorkerThread(VMThread(OpaquePointer::from_address(unsafe {
                Address::from_usize(thread_id::get())
            })));
            match ctx {
                GCThreadContext::Worker(w) => {
                    mmtk::memory_manager::start_worker(&SINGLETON, worker_tls, w)
                }
            }
        });
    }

    fn schedule_finalization(_tls: VMWorkerThread) {}

    fn out_of_memory(_tls: VMThread, _err_kind: AllocationError) {
        println!("Out of Memory!");
        unsafe { ((*UPCALLS).jl_throw_out_of_memory_error)() };
    }

    fn vm_live_bytes() -> usize {
        crate::api::JULIA_MALLOC_BYTES.load(Ordering::SeqCst)
    }

    fn is_collection_enabled() -> bool {
        unsafe { AtomicU32::load(&jl_gc_disable_counter, Ordering::SeqCst) <= 0 }
    }
}

pub fn is_current_gc_nursery() -> bool {
    match crate::SINGLETON.get_plan().generational() {
        Some(gen) => gen.is_current_gc_nursery(),
        None => false,
    }
}

#[no_mangle]
pub extern "C" fn mmtk_block_thread_for_gc() {
    AtomicBool::store(&BLOCK_FOR_GC, true, Ordering::SeqCst);

    let &(ref lock, ref cvar) = &*STW_COND.clone();
    let mut count = lock.lock().unwrap();

    info!("Blocking for GC!");

    AtomicBool::store(&WORLD_HAS_STOPPED, true, Ordering::SeqCst);

    while AtomicBool::load(&BLOCK_FOR_GC, Ordering::SeqCst) {
        count = cvar.wait(count).unwrap();
    }

    AtomicIsize::store(&USER_TRIGGERED_GC, 0, Ordering::SeqCst);
}
