use crate::SINGLETON;
use crate::{
    jl_gc_mmtk_block_for_gc_enter, jl_gc_mmtk_block_for_gc_leave,
    jl_gc_mmtk_defer_alloc_if_disabled, jl_gc_mmtk_resume_the_world,
    jl_gc_mmtk_run_pending_finalizers, jl_gc_mmtk_stop_the_world, jl_gc_safe_enter,
    jl_gc_safe_leave, jl_gc_update_stats, jl_hrtime, jl_throw_out_of_memory_error,
};
use crate::{JuliaVM, USER_TRIGGERED_GC};
use log::{info, trace};
use mmtk::util::alloc::AllocationError;
use mmtk::util::heap::GCTriggerPolicy;
use mmtk::util::opaque_pointer::*;
use mmtk::vm::{Collection, GCThreadContext};
use mmtk::Mutator;
#[cfg(feature = "concurrentimmix")]
use std::sync::atomic::AtomicBool;
use std::sync::atomic::{AtomicIsize, AtomicU64, Ordering};

pub static GC_START: AtomicU64 = AtomicU64::new(0);

use std::collections::HashSet;
use std::sync::RwLock;
use std::thread::ThreadId;

lazy_static! {
    static ref GC_THREADS: RwLock<HashSet<ThreadId>> = RwLock::new(HashSet::new());
}

#[cfg(feature = "concurrentimmix")]
pub static CONCURRENT_MARKING_ACTIVE: AtomicBool = AtomicBool::new(false);

pub(crate) fn register_gc_thread() {
    let id = std::thread::current().id();
    GC_THREADS.write().unwrap().insert(id);
}
pub(crate) fn unregister_gc_thread() {
    let id = std::thread::current().id();
    GC_THREADS.write().unwrap().remove(&id);
}
pub(crate) fn is_gc_thread() -> bool {
    let id = std::thread::current().id();
    GC_THREADS.read().unwrap().contains(&id)
}

pub struct VMCollection {}

impl Collection<JuliaVM> for VMCollection {
    fn stop_all_mutators<F>(_tls: VMWorkerThread, mut mutator_visitor: F)
    where
        F: FnMut(&'static mut Mutator<JuliaVM>),
    {
        // We try to match MMTk's collection/pause kind to Julia's jl_gc_collection_t if it is appropriate.
        // We can also use MMTk's pause kind to be more expressive.
        const JL_GC_FULL: i32 = 1;
        const JL_GC_INCREMENTAL: i32 = 2;
        let collection: i32 = if let Some(gen_plan) = SINGLETON.get_plan().generational() {
            // For generational plans, we can easily map to Julia's enum.
            if gen_plan.is_current_gc_nursery() { JL_GC_INCREMENTAL } else { JL_GC_FULL }
        } else if let Some(concurrent_plan) = SINGLETON.get_plan().concurrent() {
            // For concurrent plans, we do a very rough mapping now.
            match concurrent_plan.current_pause().map(|pause| pause as u8) {
                // TODO: Switch to MMTK's PauseKind when it is public.
                // Pause::Full
                Some(1) => JL_GC_FULL,
                // Pause::InitialMark / Pause::FinalMark
                Some(_) => JL_GC_INCREMENTAL,
                None => JL_GC_FULL,
            }
        } else {
            JL_GC_FULL
        };

        // Arm the safepoint and wait for every registered mutator to reach it. mmtk-core
        // guarantees this function has exactly one caller at a time for the current pause.
        unsafe { jl_gc_mmtk_stop_the_world(collection) };

        assert!(
            crate::api::mmtk_is_collection_enabled() != 0,
            "Collection is disabled when threads are stopped for a GC. This is a concurrency bug, see https://github.com/mmtk/mmtk-julia/issues/278."
        );

        trace!("Stopped the world!");

        // STW -- concurrent marking is not active.
        #[cfg(feature = "concurrentimmix")]
        CONCURRENT_MARKING_ACTIVE.store(false, Ordering::SeqCst);

        // Tell MMTk the stacks are ready.
        {
            use mmtk::vm::ActivePlan;
            for mutator in crate::active_plan::VMActivePlan::mutators() {
                info!("stop_all_mutators: visiting {:?}", mutator.mutator_tls);
                mutator_visitor(mutator);
            }
        }

        // Record the start time of the GC
        let now = unsafe { jl_hrtime() };
        trace!("gc_start = {}", now);
        GC_START.store(now, Ordering::Relaxed);
    }

    fn resume_mutators(_tls: VMWorkerThread) {
        // Get the end time of the GC
        let end = unsafe { jl_hrtime() };
        trace!("gc_end = {}", end);
        let gc_time = end - GC_START.load(Ordering::Relaxed);
        unsafe {
            jl_gc_update_stats(
                gc_time,
                crate::api::mmtk_used_bytes(),
                is_current_gc_nursery(),
            )
        }

        #[cfg(feature = "concurrentimmix")]
        {
            // For concurrent Immix, we need to check if SATB is active
            let concurrent_plan = SINGLETON.get_plan().concurrent().unwrap();
            let concurrent_marking_active = concurrent_plan.concurrent_work_in_progress();

            if !concurrent_marking_active {
                crate::scanning::GC_STACK_SNAPSHOTS.clear_snapshots();
            }

            CONCURRENT_MARKING_ACTIVE.store(concurrent_marking_active, Ordering::SeqCst);
            log::info!("Set CONCURRENT_MARKING_ACTIVE to {concurrent_marking_active}");
        }

        AtomicIsize::store(&USER_TRIGGERED_GC, 0, Ordering::SeqCst);

        // Disarm the safepoint and let mutators run again.
        unsafe { jl_gc_mmtk_resume_the_world() };

        // `resume_mutators()` is called after every stop-the-world pause, including the pause
        // that ends a concurrent GC's background-work phase (there's no more targeted mmtk-core
        // hook for that specifically). Advance the GC epoch to wake every waiter: a mutator
        // retrying `mmtk_disable_collection()` after it failed with
        // `MMTK_DISABLE_COLLECTION_WAIT_FOR_NEW_GC_EPOCH`, and `block_for_gc` below, both waiting
        // on the same pause finishing (the retry/return is cheap either way; if disable still
        // fails, or another pause is already needed, the waiter just waits again).
        let (lock, cvar) = &*crate::GC_EPOCH_COND.clone();
        let mut epoch = lock.lock().unwrap();
        *epoch = epoch.wrapping_add(1);
        cvar.notify_all();
        drop(epoch);

        info!(
            "Live bytes = {}, total bytes = {}",
            crate::api::mmtk_used_bytes(),
            crate::api::mmtk_total_bytes()
        );

        trace!("Resuming mutators.");
    }

    fn block_for_gc(_tls: VMMutatorThread) {
        // The pause is already scheduled and `stop_all_mutators` drives it on its own, so this
        // mutator just needs to get out of the way (GC-safe region) and wait for it to finish.

        if unsafe { jl_gc_mmtk_defer_alloc_if_disabled() } != 0 {
            // Collection was disabled by the time we got here; nothing to wait for.
            return;
        }

        // Get the current epoch and wait for it to advance.
        let epoch = crate::api::mmtk_gc_epoch();
        let saved_errno = unsafe { jl_gc_mmtk_block_for_gc_enter() };
        let gc_state = unsafe { jl_gc_safe_enter() };
        crate::api::mmtk_wait_for_new_gc_epoch(epoch);
        unsafe { jl_gc_safe_leave(gc_state) };
        unsafe { jl_gc_mmtk_block_for_gc_leave() };

        // `GC.gc()` must run pending finalizers before returning (see
        // `jl_gc_mmtk_run_pending_finalizers`'s doc comment), so run them now. This also restores
        // the errno/last-error `jl_gc_mmtk_block_for_gc_enter` saved above.
        unsafe { jl_gc_mmtk_run_pending_finalizers(saved_errno) };
    }

    fn spawn_gc_thread(_tls: VMThread, ctx: GCThreadContext<JuliaVM>) {
        // Just drop the join handle. The thread will run until the process quits.
        let _ = std::thread::Builder::new()
            .name("MMTk Worker".to_string())
            .spawn(move || {
                use mmtk::util::opaque_pointer::*;
                use mmtk::util::Address;

                // Remember this GC thread
                register_gc_thread();

                // Start the worker loop
                let worker_tls = VMWorkerThread(VMThread(OpaquePointer::from_address(unsafe {
                    Address::from_usize(thread_id::get())
                })));
                match ctx {
                    GCThreadContext::Worker(w) => {
                        mmtk::memory_manager::start_worker(&SINGLETON, worker_tls, w)
                    }
                }

                // The GC thread quits somehow. Unregister this GC thread
                unregister_gc_thread();
            });
    }

    fn schedule_finalization(_tls: VMWorkerThread) {}

    fn out_of_memory(_tls: VMThread, _err_kind: AllocationError) {
        println!("Out of Memory!");
        unsafe { jl_throw_out_of_memory_error() };
    }

    fn vm_live_bytes() -> usize {
        crate::api::JULIA_MALLOC_BYTES.load(Ordering::SeqCst)
    }

    fn create_gc_trigger() -> Box<dyn GCTriggerPolicy<JuliaVM>> {
        use crate::gc_trigger::*;
        Box::new(JuliaGCTrigger::new())
    }
}

pub fn is_current_gc_nursery() -> bool {
    match crate::SINGLETON.get_plan().generational() {
        Some(gen) => gen.is_current_gc_nursery(),
        None => false,
    }
}
