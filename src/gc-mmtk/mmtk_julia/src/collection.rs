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
#[cfg(any(feature = "concurrentimmix", feature = "concurrent_marking"))]
use std::sync::atomic::AtomicBool;
use std::sync::atomic::{AtomicIsize, AtomicU64, Ordering};

pub static GC_START: AtomicU64 = AtomicU64::new(0);
/// `jl_hrtime` at which the previous pause released the mutators (stats only).
pub static LAST_RESUME: AtomicU64 = AtomicU64::new(0);

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
        // Deliberately *not* `drop_all_finalizers()`. The lists root the objects they name
        // (see `collect_finalizer_roots`); zeroing them frees live objects that libuv and
        // others still point at. The entries are retained and rooted at root-scanning time
        // instead, which leaks them until LXR grows a real finalizer path.
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
        // Map MMTk's pause kind to Julia's `jl_gc_collection_t` where appropriate.
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
        // Same instant the reported pause duration is measured from, so the timeline lines up with
        // the `us=` figure on the `[lxr] stw pause=` line.
        mmtk::scheduler::stage_timeline::start();
        mmtk::scheduler::packet_timing::reset();
    }

    fn resume_mutators(_tls: VMWorkerThread) {
        /// Whether to report one `stw pause=` line per pause and nothing else. Set
        /// `MMTK_LXR_PAUSES=1`.
        fn pauses() -> bool {
            static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
            *ON.get_or_init(|| std::env::var_os("MMTK_LXR_PAUSES").is_some())
        }

        if std::env::var_os("MMTK_LXR_VERIFY").is_some() {
            recheck_live_closure_after_sweep();
        }
        // Get the end time of the GC
        let end = unsafe { jl_hrtime() };
        trace!("gc_end = {}", end);
        let gc_time = end - GC_START.load(Ordering::Relaxed);
        // `MMTK_LXR_TIMELINE` alone reports the pause and its critical path without enabling
        // `MMTK_LXR_STATS`. That matters for measuring the pause honestly: the stats path makes
        // `LXR::release` write three lines to stderr *inside* the pause, so leaving it on while
        // attributing time to the `Release` packet measures the instrumentation too.
        let stats = std::env::var_os("MMTK_LXR_STATS").is_some();
        let timeline = mmtk::scheduler::stage_timeline::enabled();
        // `MMTK_LXR_PAUSES` reports just the one `stw pause=` line per pause, so a pause
        // distribution can be measured without any of the instrumentation that runs *inside* the
        // pause: the stats path writes three lines from `LXR::release`, and the timeline path takes
        // a mutex at every stage boundary.
        if stats || timeline || pauses() {
            // Attribute the STW window to the pause kind that produced it. The aggregate
            // `gc_num.total_time` mixes RefCount, InitialMark and FinalMark together, which
            // hides which of the three is actually costing the mutator.
            // `current_pause` is already cleared by `gc_pause_end` at this point, so read
            // `previous_pause`, which that same function sets to the pause just finished.
            let kind = crate::SINGLETON
                .get_plan()
                .downcast_ref::<mmtk::plan::lxr::LXR<JuliaVM>>()
                .and_then(|lxr| lxr.previous_pause())
                .map(|p| format!("{:?}", p))
                .unwrap_or_else(|| "?".to_string());
            // Also report the mutator window since the previous pause ended. If
            // InitialMark -> FinalMark leaves the mutator almost no wall-clock time,
            // concurrent marking cannot make progress and the whole transitive closure
            // lands inside the FinalMark STW pause.
            let prev_end = LAST_RESUME.swap(end, Ordering::Relaxed);
            let mutator_us = if prev_end == 0 {
                0
            } else {
                (GC_START.load(Ordering::Relaxed).saturating_sub(prev_end)) / 1000
            };
            eprintln!(
                "[lxr] stw pause={} us={} mutator_us_before={}",
                kind,
                gc_time / 1000,
                mutator_us
            );
            // Wall-clock critical path of this pause. Unlike the packet table below, the deltas
            // are real elapsed time and include the inter-stage handshake.
            let timeline = mmtk::scheduler::stage_timeline::take();
            let mut prev = 0u128;
            for (label, nanos) in &timeline {
                eprintln!(
                    "[lxr]   at={:>8}us +{:>8}us {}",
                    nanos / 1000,
                    (nanos - prev) / 1000,
                    label
                );
                prev = *nanos;
            }
            if !timeline.is_empty() {
                eprintln!(
                    "[lxr]   at={:>8}us +{:>8}us <resume_mutators>",
                    gc_time / 1000,
                    (gc_time as u128).saturating_sub(prev) / 1000
                );
            }
            // Attribute the pause to the work packet types that ran in it. This is the last
            // point in the pause, so everything the pause scheduled has already executed.
            if stats {
                for (name, count, nanos) in mmtk::scheduler::packet_timing::take_top(12) {
                    eprintln!("[lxr]   packet n={:<6} us={:<8} {}", count, nanos / 1000, name);
                }
            }
        }
        unsafe {
            jl_gc_update_stats(
                gc_time,
                crate::api::mmtk_used_bytes(),
                is_current_gc_nursery(),
            )
        }

        #[cfg(feature = "concurrent_marking")]
        {
            // Every concurrent plan (concurrentimmix's SATB, and LXR) needs this: whether its
            // background marking is still in flight decides whether gcstack scans have to keep
            // using the stable snapshot in `GC_STACK_SNAPSHOTS` rather than live stack memory.
            // This used to be spelled `concurrentimmix`, which silently excluded LXR -- so a
            // task discovered live via ordinary heap tracing (not just root scanning) got its
            // gcstack read directly out of live memory while marking ran concurrently with the
            // mutator, racing a stack-pool release/reuse of that same memory and segfaulting in
            // `mmtk_scan_gcstack`.
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

        // Advance the GC epoch to wake every waiter: a mutator
        // retrying `mmtk_disable_collection()` after it failed with
        // `MMTK_DISABLE_COLLECTION_WAIT_FOR_NEW_GC_EPOCH`, and `block_for_gc` below, both waiting
        // on the same pause finishing.
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

        // `GC.gc()` must run pending finalizers before returning, so run them now.
        // This also restores the errno/last-error `jl_gc_mmtk_block_for_gc_enter` saved above.
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

    /// Called early in the plan's release, before anything is reclaimed, which is where
    /// the bring-up verifier below needs to run.
    fn vm_release() {
        if std::env::var_os("MMTK_LXR_VERIFY").is_some() {
            verify_rc_covers_live_closure();
        }
    }

    /// Under LXR the concurrent phase (decrements, mature sweeping) ends by simply running out
    /// of work rather than with a `FinalMark` pause, so `resume_mutators` -- the other place
    /// that advances the epoch -- never runs for it. A thread parked in
    /// `mmtk_wait_for_new_gc_epoch()` after a failed `mmtk_disable_collection()` would wait
    /// forever; the sysimage writer's `jl_gc_enable(0)` is one such thread.
    fn concurrent_work_finished() {
        let (lock, cvar) = &*crate::GC_EPOCH_COND.clone();
        let mut epoch = lock.lock().unwrap();
        *epoch = epoch.wrapping_add(1);
        cvar.notify_all();
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
        let o = unsafe { mmtk::util::ObjectReference::from_raw_address_unchecked(a) };
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
        let o = unsafe {
            mmtk::util::ObjectReference::from_raw_address_unchecked(mmtk::util::Address::from_usize(
                addr,
            ))
        };
        o.iterate_fields::<JuliaVM, _>(mmtk::util::VMThread::UNINITIALIZED, |s| {
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
        let o = unsafe {
            mmtk::util::ObjectReference::from_raw_address_unchecked(mmtk::util::Address::from_usize(
                addr,
            ))
        };
        if !lxr.is_rc_object(o) {
            continue;
        }
        // `SweepDeadCycles` zeroes the count of anything it finds unmarked, so a live object the
        // trace failed to mark is doomed regardless of its count.
        if !lxr.is_marked(o) {
            unmarked += 1;
        }
        let got = usize::from(lxr.rc.count(o));
        // Counts are two bits wide, so at the maximum count (3) the count is sticky: it stops
        // tracking the true number of references and the object is never decremented again.
        // Anything at that value is immortal by design, not undercounted.
        if lxr.rc.is_stuck(o) {
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
    fn create_process_roots_work_with_root_kind(
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
        o.iterate_fields::<JuliaVM, _>(mmtk::util::VMThread::UNINITIALIZED, |s| {
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
            referrer.iterate_fields::<JuliaVM, _>(mmtk::util::VMThread::UNINITIALIZED, |s| {
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
