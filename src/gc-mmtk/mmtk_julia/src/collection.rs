use crate::{spawn_collector_thread, UPCALLS, SINGLETON, BI_METADATA_START_ALIGNED_DOWN, BI_METADATA_END_ALIGNED_UP};
use crate::JuliaVM;
use crate::object_model::BI_MARKING_METADATA_SPEC;
use mmtk::Mutator;
use mmtk::util::alloc::AllocationError;
use mmtk::memory_manager;
use mmtk::util::opaque_pointer::*;
use mmtk::util::Address;
use mmtk::vm::{Collection, GCThreadContext};
use mmtk::MutatorContext;
use std::sync::atomic::{AtomicBool, Ordering};
use log::{info, trace};


use crate::{BLOCK_FOR_GC, STW_COND, STFF_COND, WORLD_HAS_STOPPED, FINALIZERS_RUNNING};

const GC_THREAD_KIND_CONTROLLER: libc::c_int = 0;
const GC_THREAD_KIND_WORKER: libc::c_int = 1;

pub struct VMCollection {}

impl Collection<JuliaVM> for VMCollection {
    fn stop_all_mutators<F>(_tls: VMWorkerThread, _mutator_visitor: F)
    where
        F: FnMut(&'static mut Mutator<JuliaVM>),
    {
        // Wait for all mutators to stop and all finalizers to run
        unsafe {
            while !AtomicBool::load(&WORLD_HAS_STOPPED, Ordering::SeqCst) {
                // Stay here while the world has not stopped
                // FIXME add wait var
            }

            let &(ref lock, ref cvar) = &*STFF_COND.clone();
            let mut count = lock.lock().unwrap();
            *count += 1;

            while AtomicBool::load(&FINALIZERS_RUNNING, Ordering::SeqCst) {
                count = cvar.wait(count).unwrap();
            }
        }
        trace!("Stopped the world!")
    }

    fn resume_mutators(_tls: VMWorkerThread) {
        unsafe {
            AtomicBool::store(&BLOCK_FOR_GC, false, Ordering::SeqCst);
            AtomicBool::store(&WORLD_HAS_STOPPED, false, Ordering::SeqCst);
        }

        let &(_, ref cvar) = &*STW_COND.clone();
        cvar.notify_all();
        unsafe {
            BI_MARKING_METADATA_SPEC.bzero_metadata(Address::from_usize(BI_METADATA_START_ALIGNED_DOWN), BI_METADATA_END_ALIGNED_UP - BI_METADATA_START_ALIGNED_DOWN)
        }

        info!("Live bytes = {}, total bytes = {}", crate::api::used_bytes(), crate::api::total_bytes());
        trace!("Resuming mutators.");
    }

    fn block_for_gc(tls: VMMutatorThread) {
        trace!("Triggered GC!");
        
        unsafe {
            AtomicBool::store(&BLOCK_FOR_GC, true, Ordering::SeqCst);

            let &(ref lock, ref cvar) = &*STFF_COND.clone();
            let mut count = lock.lock().unwrap();
            *count += 1;

            while AtomicBool::load(&FINALIZERS_RUNNING, Ordering::SeqCst) {
                count = cvar.wait(count).unwrap();
            }
        }   

        let tls_ptr = match tls {
            VMMutatorThread(t) => match t {
                VMThread(ptr) => ptr
            }
        };


        let old_state = unsafe {
            ((*UPCALLS).set_gc_initial_state)(tls_ptr)
        };      

        if old_state as u32 == u32::MAX {
            info!("Multiple threads entered GC simultaneously.");           
            return;
        }

        unsafe {
            ((*UPCALLS).wait_for_the_world)()
        };

        let last_err = unsafe {
            ((*UPCALLS).get_jl_last_err)()
        };

        unsafe {
            ((*UPCALLS).calculate_roots)(tls_ptr);
        }

        let &(ref lock, ref cvar) = &*STW_COND.clone();
        let mut count = lock.lock().unwrap();
        *count += 1;

        info!("Blocking for GC!");  

        unsafe {
            AtomicBool::store(&WORLD_HAS_STOPPED, true, Ordering::SeqCst);
        }

        unsafe {
            while AtomicBool::load(&BLOCK_FOR_GC, Ordering::SeqCst) {
                count = cvar.wait(count).unwrap();
            }
        }

        info!("GC Done!");

        unsafe {
            ((*UPCALLS).set_gc_final_state)(old_state)
        };
        

        info!("Finalizing objects!");
        unsafe {
            ((*UPCALLS).mmtk_jl_run_finalizers)(tls_ptr)
        };

        
        unsafe {
            ((*UPCALLS).set_jl_last_err)(last_err)
        };
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

    fn schedule_finalization(_tls: VMWorkerThread) {
    }

    fn out_of_memory(_tls: VMThread, _err_kind: AllocationError) {
        println!("Out of Memory!");
        unsafe {
            ((*UPCALLS).jl_throw_out_of_memory_error)()
        };
    }

    fn prepare_mutator<T: MutatorContext<JuliaVM>>(
        _tls_w: VMWorkerThread,
        _tls_m: VMMutatorThread,
        _mutator: &T,
    ) {
    }
}

#[no_mangle]
pub extern "C" fn mmtk_run_finalizers(at_exit : bool) {
    AtomicBool::store(&FINALIZERS_RUNNING, true, Ordering::SeqCst);
    
    if at_exit {
        let mut all_finalizable = memory_manager::get_all_finalizers(&SINGLETON);
        loop {
            let to_be_finalized = all_finalizable.pop();

            match to_be_finalized {
                Some(obj) => {
                    unsafe {
                        ((*UPCALLS).run_finalizer_function)(obj.0, obj.1, obj.2)
                    }
                },
                None =>  {
                    break
                }
            }
        }
    } else {
        loop {
            let to_be_finalized = memory_manager::get_finalized_object(&SINGLETON);
    
            match to_be_finalized {
                Some(obj) => {
                    unsafe {
                        ((*UPCALLS).run_finalizer_function)(obj.0, obj.1, obj.2)
                    }
                },
                None =>  {
                    break
                }
            }
        }
    }
    

    AtomicBool::store(&FINALIZERS_RUNNING, false, Ordering::SeqCst);
    let &(_, ref cvar) = &*STFF_COND.clone();
    cvar.notify_all();    
}