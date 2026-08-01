use crate::SINGLETON;
use crate::{
    jl_gc_prepare_to_collect, jl_gc_update_stats, jl_hrtime, jl_throw_out_of_memory_error,
};
use crate::{JuliaVM, USER_TRIGGERED_GC};
use log::{info, trace};
use mmtk::util::alloc::AllocationError;
use mmtk::util::heap::GCTriggerPolicy;
use mmtk::util::opaque_pointer::*;
use mmtk::vm::{Collection, GCThreadContext};
use mmtk::Mutator;
use std::sync::atomic::{AtomicBool, AtomicIsize, AtomicU64, Ordering};

use crate::{BLOCK_FOR_GC, STW_COND, WORLD_HAS_STOPPED};

pub static GC_START: AtomicU64 = AtomicU64::new(0);

use std::collections::HashSet;
use std::sync::RwLock;
use std::thread::ThreadId;

lazy_static! {
    static ref GC_THREADS: RwLock<HashSet<ThreadId>> = RwLock::new(HashSet::new());
}

#[cfg(feature = "concurrent_marking")]
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
    /// Called by `LXR::release` at the start of every pause.
    ///
    /// `lxr` is true when the caller is the LXR plan, which is the only plan with no working
    /// finalizer path. See [`crate::julia_finalizer::drop_all_finalizers`].
    fn update_weak_processor(lxr: bool) {
        if !lxr {
            return;
        }
        crate::julia_finalizer::drop_all_finalizers();
        // Prune `ptls->live_tasks`, which nothing else does under LXR.
        //
        // `SweepVMSpecific` would normally do this, but it is scheduled from
        // `process_weak_refs`, which LXR never calls. Left unpruned, dead tasks stay on a list
        // that `gather_mutator_roots` deliberately scans *unrooted*, and their gcstacks get
        // walked after the memory is recycled -- the crash #10 segfault in `mmtk_scan_gcstack`.
        //
        // This depends on `mmtk_is_live_object` being LXR-aware (see `api.rs`): the sweep asks
        // it about every entry, and the stock path faults on unmarked objects under
        // `lxr_no_evac`. An earlier attempt to call this before that fix made `sys-o.a` fail
        // *earlier* (GC 2 rather than 4-5).
        unsafe {
            crate::jl_gc_sweep_stack_pools_and_mtarraylist_buffers();
        }
    }

    fn stop_all_mutators<F>(_tls: VMWorkerThread, mut mutator_visitor: F)
    where
        F: FnMut(&'static mut Mutator<JuliaVM>),
    {
        // Wait for all mutators to stop and all finalizers to run
        while !AtomicBool::load(&WORLD_HAS_STOPPED, Ordering::SeqCst) {
            // Stay here while the world has not stopped
            // FIXME add wait var
        }

        assert!(
            crate::api::mmtk_is_collection_enabled() != 0,
            "Collection is disabled when threads are stopped for a GC. This is a concurrency bug, see https://github.com/mmtk/mmtk-julia/issues/278."
        );

        trace!("Stopped the world!");

        // STW -- concurrent marking is not active.
        #[cfg(feature = "concurrent_marking")]
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
        if std::env::var_os("MMTK_LXR_VERIFY").is_some() {
            recheck_live_closure_after_sweep();
        }
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

        // Holding the mutex here guarantees that any mutator that observed
        // `BLOCK_FOR_GC == true` is already enqueued in `wait()` by the time
        // we call `notify_all`.
        let (lock, cvar) = &*STW_COND.clone();
        let count = lock.lock().unwrap();

        #[cfg(feature = "concurrent_marking")]
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

        AtomicBool::store(&BLOCK_FOR_GC, false, Ordering::SeqCst);
        AtomicBool::store(&WORLD_HAS_STOPPED, false, Ordering::SeqCst);
        cvar.notify_all();
        drop(count);

        // `resume_mutators()` is called after every stop-the-world pause, including the pause
        // that ends a concurrent GC's background-work phase (there's no more targeted mmtk-core
        // hook for that specifically). Advance the GC epoch to wake any mutator retrying
        // `mmtk_disable_collection()` after it failed with
        // `MMTK_DISABLE_COLLECTION_WAIT_FOR_NEW_GC_EPOCH`: since a pause just completed, it's
        // worth retrying (the retry is cheap; if it still fails, the mutator just waits again).
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
        info!("Triggered GC!");

        unsafe { jl_gc_prepare_to_collect() };

        info!("Finished blocking mutator for GC!");
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

    /// Called early in the plan's release, before anything is reclaimed, which is where
    /// the bring-up verifier below needs to run.
    fn vm_release() {
        if std::env::var_os("MMTK_LXR_VERIFY").is_some() {
            verify_rc_covers_live_closure();
        }
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

#[no_mangle]
pub extern "C" fn mmtk_block_thread_for_gc() {
    AtomicBool::store(&BLOCK_FOR_GC, true, Ordering::SeqCst);

    let (lock, cvar) = &*STW_COND.clone();
    let mut count = lock.lock().unwrap();

    info!("Blocking for GC!");

    AtomicBool::store(&WORLD_HAS_STOPPED, true, Ordering::SeqCst);

    while AtomicBool::load(&BLOCK_FOR_GC, Ordering::SeqCst) {
        count = cvar.wait(count).unwrap();
    }

    AtomicIsize::store(&USER_TRIGGERED_GC, 0, Ordering::SeqCst);
}

/// Bring-up verifier for LXR: walk the live closure from the roots recorded in this
/// pause and report any object that reached it with a zero reference count.
///
/// LXR reclaims a nursery object purely on the strength of its count, so an object that
/// is reachable but uncounted is one that some store failed to record. Reporting the
/// Julia type of each such object points straight at the write barrier that is missing.
/// Runs before anything is reclaimed, from `vm_release`.
/// The live closure captured before sweeping, as (object, header word) pairs, so it can
/// be rechecked once the collector has finished reclaiming.
static LIVE_SNAPSHOT: std::sync::Mutex<Vec<(usize, usize)>> = std::sync::Mutex::new(Vec::new());

/// Second half of the bring-up verifier: after the collector has swept, confirm that
/// everything that was reachable before it ran is still intact.
///
/// A changed header word means the object's memory was handed out again while it was
/// still live, which is a reclamation or line-reuse fault rather than a counting one. A
/// count that fell to zero means it was released outright.
fn recheck_live_closure_after_sweep() {
    let plan = crate::SINGLETON.get_plan();
    let Some(lxr) = plan.downcast_ref::<mmtk::plan::lxr::LXR<JuliaVM>>() else {
        return;
    };
    let snapshot = std::mem::take(&mut *LIVE_SNAPSHOT.lock().unwrap());
    let mut reused = 0usize;
    let mut zeroed = 0usize;
    let mut on_free_line = 0usize;
    let mut examples: Vec<String> = vec![];
    for &(addr, header) in &snapshot {
        let a = unsafe { mmtk::util::Address::from_usize(addr) };
        let o = a.to_object_reference::<JuliaVM>();
        let now = unsafe { (a - 8usize).load::<usize>() };
        let count_zero = lxr.is_rc_object(o) && lxr.rc.count(o) == 0;
        // The mutator overwrites live data only after the GC ends, so catching it needs
        // the state the allocator will act on, not the bytes as they stand right now.
        if lxr.object_occupies_free_line(o) {
            on_free_line += 1;
            if examples.len() < 16 {
                let name = unsafe {
                    std::ffi::CStr::from_ptr(crate::jl_typeof_str(a))
                        .to_str()
                        .unwrap_or("?")
                };
                examples.push(format!(
                    "live object on a line the allocator will reuse: {:?} type={} size={}",
                    o,
                    name,
                    o.get_size::<JuliaVM>()
                ));
            }
        }
        if now != header {
            reused += 1;
            if examples.len() < 16 {
                examples.push(format!("reused {:?} header {:x} -> {:x}", o, header, now));
            }
        } else if count_zero {
            zeroed += 1;
            if examples.len() < 16 {
                examples.push(format!("count dropped to zero {:?}", o));
            }
        }
    }
    if reused != 0 || zeroed != 0 || on_free_line != 0 {
        eprintln!(
            "[lxr-verify] after sweep: reused={} count_zeroed={} on_free_line={}",
            reused, zeroed, on_free_line
        );
        for e in examples {
            eprintln!("[lxr-verify]   {}", e);
        }
    }
    let (zeroed, kept) = mmtk::plan::lxr::sweep_dead_cycle_counts();
    eprintln!("[lxr-verify] sweep dead cycles: zeroed={zeroed} kept_marked={kept} (cumulative)");
    audit_reference_counts(lxr, &snapshot);
}

/// Audit every live object's reference count against the number of references to it, rather
/// than only asking whether a count reached zero.
///
/// The expected count of an object is the number of counted incoming edges: fields of live
/// objects that hold it, excluding derived slots (which alias another field and are deliberately
/// not counted, see `Slot::is_derived`) and excluding fields of objects the plan does not
/// reference-count.
///
/// Only an **undercount** is a soundness bug -- it is what lets a live object be reclaimed. An
/// overcount is expected here and is not reported as a fault: a root reference contributes an
/// increment that is only released by the following pause's `process_prev_roots`, so anything
/// held by a root legitimately reads high at this point. Counts saturated at the maximum are
/// skipped for the same reason.
fn audit_reference_counts(lxr: &mmtk::plan::lxr::LXR<JuliaVM>, snapshot: &[(usize, usize)]) {
    use mmtk::vm::slot::Slot;

    // Objects whose header changed are already reported as reused; walking their fields would
    // be reading whatever now occupies the memory.
    let intact = |addr: usize, header: usize| unsafe {
        (mmtk::util::Address::from_usize(addr) - 8usize).load::<usize>() == header
    };

    let mut expected: std::collections::HashMap<usize, usize> = Default::default();
    for &(addr, header) in snapshot {
        if !intact(addr, header) {
            continue;
        }
        let o = unsafe { mmtk::util::Address::from_usize(addr).to_object_reference::<JuliaVM>() };
        o.iterate_fields::<JuliaVM, _>(|s| {
            if s.is_derived() {
                return;
            }
            if let Some(t) = s.load() {
                if lxr.is_rc_object(t) {
                    *expected.entry(t.to_raw_address().as_usize()).or_insert(0) += 1;
                }
            }
        });
    }

    let mut under = 0usize;
    let mut over = 0usize;
    let mut unmarked = 0usize;
    let mut examples: Vec<String> = vec![];
    for &(addr, header) in snapshot {
        if !intact(addr, header) {
            continue;
        }
        let o = unsafe { mmtk::util::Address::from_usize(addr).to_object_reference::<JuliaVM>() };
        if !lxr.is_rc_object(o) {
            continue;
        }
        // `SweepDeadCycles` zeroes the count of anything it finds unmarked, so a live object the
        // trace failed to mark is doomed regardless of its count.
        if !lxr.is_marked(o) {
            unmarked += 1;
        }
        let got = usize::from(lxr.rc.count(o));
        // Counts are two bits wide, so at `MAX_REF_COUNT` (3) the count is sticky: it stops
        // tracking the true number of references and the object is never decremented again.
        // Anything at that value is immortal by design, not undercounted.
        if got >= usize::from(mmtk::util::rc::MAX_REF_COUNT) {
            continue;
        }
        let want = expected.get(&addr).copied().unwrap_or(0);
        if got < want {
            under += 1;
            if examples.len() < 16 {
                let name = unsafe {
                    std::ffi::CStr::from_ptr(crate::jl_typeof_str(o.to_raw_address()))
                        .to_str()
                        .unwrap_or("?")
                };
                examples.push(format!(
                    "UNDERCOUNT {:?} type={} rc={} but {} live fields hold it marked={}",
                    o,
                    name,
                    got,
                    want,
                    lxr.is_marked(o)
                ));
            }
        } else if got > want {
            over += 1;
        }
    }
    eprintln!(
        "[lxr-verify] rc audit: undercounted={} overcounted={} live_but_unmarked={} \
         (overcount is expected: roots)",
        under, over, unmarked
    );
    for e in examples {
        eprintln!("[lxr-verify]   {}", e);
    }
}

/// Collects reported roots instead of turning them into work packets, so the verifier can
/// ask the VM for its root set outside of an actual root-scanning stage.
#[derive(Clone, Default)]
struct CollectingRootsFactory {
    slots: std::sync::Arc<std::sync::Mutex<Vec<crate::slots::JuliaVMSlot>>>,
    nodes: std::sync::Arc<std::sync::Mutex<Vec<mmtk::util::ObjectReference>>>,
}

impl mmtk::vm::RootsWorkFactory<crate::slots::JuliaVMSlot> for CollectingRootsFactory {
    fn create_process_roots_work(
        &mut self,
        slots: Vec<crate::slots::JuliaVMSlot>,
        _kind: mmtk::scheduler::RootKind,
    ) {
        self.slots.lock().unwrap().extend(slots);
    }

    fn create_process_pinning_roots_work(&mut self, nodes: Vec<mmtk::util::ObjectReference>) {
        self.nodes.lock().unwrap().extend(nodes);
    }

    fn create_process_tpinning_roots_work(&mut self, nodes: Vec<mmtk::util::ObjectReference>) {
        self.nodes.lock().unwrap().extend(nodes);
    }
}

/// The root set as *the VM* sees it: every thread's shadow stacks, tasks, exception and
/// backtrace buffer, plus the VM-specific roots.
///
/// Deliberately not `LXR::snapshot_curr_roots()`. That returns the roots LXR itself
/// counted, so checking reference counts against it is circular and reports success on a
/// heap that has already lost objects. This is derived independently, from the same code
/// the real root scan uses.
fn vm_roots() -> Vec<mmtk::util::ObjectReference> {
    use mmtk::vm::slot::Slot;
    use mmtk::vm::ActivePlan;

    let mut roots = vec![];
    for mutator in <crate::active_plan::VMActivePlan as ActivePlan<JuliaVM>>::mutators() {
        let ptls: &mut crate::julia_types::_jl_tls_states_t =
            unsafe { std::mem::transmute(mutator.mutator_tls) };
        let (slot_buffer, node_buffer) = crate::scanning::gather_mutator_roots(ptls);
        roots.extend(slot_buffer.buffer);
        roots.extend(node_buffer);
    }

    let factory = CollectingRootsFactory::default();
    {
        let mut f = factory.clone();
        let mut closure = crate::slots::RootsWorkClosure::from_roots_work_factory(&mut f);
        unsafe {
            crate::jl_gc_scan_vm_specific_roots(&mut closure as _);
        }
    }
    roots.extend(factory.nodes.lock().unwrap().iter().copied());
    for s in factory.slots.lock().unwrap().iter() {
        if let Some(o) = s.load() {
            roots.push(o);
        }
    }
    roots
}

fn verify_rc_covers_live_closure() {
    use mmtk::vm::slot::Slot;
    use std::collections::HashSet;

    let plan = crate::SINGLETON.get_plan();
    let Some(lxr) = plan.downcast_ref::<mmtk::plan::lxr::LXR<JuliaVM>>() else {
        return;
    };

    let mut seen: HashSet<usize> = HashSet::new();
    let mut stack: Vec<mmtk::util::ObjectReference> = vm_roots();
    let mut missing: Vec<(mmtk::util::ObjectReference, &'static str)> = vec![];

    while let Some(o) = stack.pop() {
        if !seen.insert(o.to_raw_address().as_usize()) {
            continue;
        }
        if lxr.is_rc_object(o) && lxr.rc.count(o) == 0 {
            let name = unsafe {
                std::ffi::CStr::from_ptr(crate::jl_typeof_str(o.to_raw_address()))
                    .to_str()
                    .unwrap_or("?")
            };
            if missing.len() < 32 {
                missing.push((o, name));
            }
        }
        o.iterate_fields::<JuliaVM, _>(|s| {
            if let Some(t) = s.load() {
                stack.push(t);
            }
        });
    }

    // Hand the live set to the plan so a decrement that takes one of these objects to zero
    // reports itself as it happens. The post-sweep check below can only say *that* it
    // happened, never which decrement did it.
    mmtk::plan::lxr::set_live_set(seen.clone());

    // Keep the closure with each object's header word, so `recheck_live_closure_after_sweep`
    // can tell whether the collector handed any of this memory out again.
    {
        let mut snap = LIVE_SNAPSHOT.lock().unwrap();
        snap.clear();
        for a in &seen {
            let header = unsafe { mmtk::util::Address::from_usize(*a - 8).load::<usize>() };
            snap.push((*a, header));
        }
    }
    eprintln!(
        "[lxr-verify] reachable={} uncounted={}",
        seen.len(),
        missing.len()
    );
    for (o, name) in &missing {
        eprintln!("[lxr-verify]   uncounted {:?} type={}", o, name);
        mmtk::plan::lxr::dump_rc_events(*o);
    }

    // An uncounted object is only interesting through its referrers: it has a zero count while
    // some live object points at it, so one of those edges was never counted. Walk the closure
    // again looking for fields that hold it, and report what the barrier thought of each -- a
    // logged field means the barrier believed it had already snapshotted that edge this epoch.
    if !missing.is_empty() {
        let targets: HashSet<usize> = missing
            .iter()
            .map(|(o, _)| o.to_raw_address().as_usize())
            .collect();
        for a in &seen {
            let Some(referrer) = mmtk::util::ObjectReference::from_raw_address(unsafe {
                mmtk::util::Address::from_usize(*a)
            }) else {
                continue;
            };
            referrer.iterate_fields::<JuliaVM, _>(|s| {
                let Some(t) = s.load() else { return };
                if !targets.contains(&t.to_raw_address().as_usize()) {
                    return;
                }
                let name = unsafe {
                    std::ffi::CStr::from_ptr(crate::jl_typeof_str(referrer.to_raw_address()))
                        .to_str()
                        .unwrap_or("?")
                };
                eprintln!(
                    "[lxr-verify]     referrer of {:?}: {:?} type={} rc={} obj_unlogged={} \
                     field={} field_logged={:?} derived={}",
                    t,
                    referrer,
                    name,
                    lxr.rc.count(referrer),
                    mmtk::plan::lxr::object_is_unlogged::<JuliaVM>(referrer),
                    s.to_address(),
                    mmtk::plan::lxr::field_is_logged::<JuliaVM>(s.to_address()),
                    mmtk::vm::slot::Slot::is_derived(&s),
                );
            });
        }
    }
}
