use crate::{spawn_collector_thread, UPCALLS};
use crate::{JuliaVM, USER_TRIGGERED_GC};
use log::{info, trace};
use mmtk::util::alloc::AllocationError;
use mmtk::util::opaque_pointer::*;
use mmtk::vm::{Collection, GCThreadContext};
use mmtk::Mutator;
use mmtk::MutatorContext;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};

use crate::{BLOCK_FOR_GC, STW_COND, WORLD_HAS_STOPPED};

const GC_THREAD_KIND_CONTROLLER: libc::c_int = 0;
const GC_THREAD_KIND_WORKER: libc::c_int = 1;

static GC_START: AtomicU64 = AtomicU64::new(0);

pub struct VMCollection {}

impl Collection<JuliaVM> for VMCollection {
    fn stop_all_mutators<F>(_tls: VMWorkerThread, _mutator_visitor: F)
    where
        F: FnMut(&'static mut Mutator<JuliaVM>),
    {
        // Wait for all mutators to stop and all finalizers to run
        while !AtomicBool::load(&WORLD_HAS_STOPPED, Ordering::SeqCst) {
            // Stay here while the world has not stopped
            // FIXME add wait var
        }

        trace!("Stopped the world!");

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
        unsafe { ((*UPCALLS).update_gc_time)(gc_time) }

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

    fn block_for_gc(tls: VMMutatorThread) {
        info!("Triggered GC!");

        AtomicBool::store(&BLOCK_FOR_GC, true, Ordering::SeqCst);

        let tls_ptr = match tls {
            VMMutatorThread(t) => match t {
                VMThread(ptr) => ptr,
            },
        };

        let old_state = unsafe { ((*UPCALLS).set_gc_initial_state)(tls_ptr) };

        if old_state == -1 {
            info!("Multiple threads entered GC simultaneously.");
            return;
        }

        unsafe { ((*UPCALLS).wait_for_the_world)() };

        let last_err = unsafe { ((*UPCALLS).get_jl_last_err)() };

        unsafe {
            ((*UPCALLS).calculate_roots)(tls_ptr);
        }

        {
            let &(ref lock, ref cvar) = &*STW_COND.clone();
            let mut count = lock.lock().unwrap();

            info!("Blocking for GC!");

            AtomicBool::store(&WORLD_HAS_STOPPED, true, Ordering::SeqCst);

            while AtomicBool::load(&BLOCK_FOR_GC, Ordering::SeqCst) {
                count = cvar.wait(count).unwrap();
            }
        }

        info!("GC Done!");
        if AtomicBool::load(&USER_TRIGGERED_GC, Ordering::SeqCst) {
            AtomicBool::store(&USER_TRIGGERED_GC, false, Ordering::SeqCst);
        }

        unsafe { ((*UPCALLS).set_gc_final_state)(old_state) };

        info!("Finalizing objects!");
        unsafe { ((*UPCALLS).mmtk_jl_run_finalizers)(tls_ptr) };

        unsafe { ((*UPCALLS).set_jl_last_err)(last_err) };
        info!("Finished blocking mutator for GC!");
    }

    fn spawn_gc_thread(tls: VMThread, ctx: GCThreadContext<JuliaVM>) {
        let (ctx_ptr, kind) = match ctx {
            GCThreadContext::Controller(c) => (
                Box::into_raw(c) as *mut libc::c_void,
                GC_THREAD_KIND_CONTROLLER,
            ),
            GCThreadContext::Worker(w) => {
                (Box::into_raw(w) as *mut libc::c_void, GC_THREAD_KIND_WORKER)
            }
        };

        unsafe {
            spawn_collector_thread(tls, ctx_ptr as usize as _, kind);
        }
    }

    fn schedule_finalization(_tls: VMWorkerThread) {}

    fn out_of_memory(_tls: VMThread, _err_kind: AllocationError) {
        println!("Out of Memory!");
        unsafe { ((*UPCALLS).jl_throw_out_of_memory_error)() };
    }

    fn prepare_mutator<T: MutatorContext<JuliaVM>>(
        _tls_w: VMWorkerThread,
        _tls_m: VMMutatorThread,
        _mutator: &T,
    ) {
    }

    fn vm_live_bytes() -> usize {
        crate::api::JULIA_MALLOC_BYTES.load(Ordering::SeqCst)
    }
}
