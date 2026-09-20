use crate::slots::JuliaVMSlot;
use crate::SINGLETON;
use mmtk::memory_manager;
use mmtk::scheduler::*;
use mmtk::util::opaque_pointer::*;
use mmtk::util::ObjectReference;
use mmtk::vm::slot::Slot;
use mmtk::vm::ObjectTracerContext;
use mmtk::vm::RootsWorkFactory;
use mmtk::vm::Scanning;
use mmtk::vm::SlotVisitor;
use mmtk::vm::VMBinding;
use mmtk::Mutator;
use mmtk::MutatorContext;
use mmtk::MMTK;

use crate::jl_gc_mmtk_sweep_malloced_memory;
use crate::jl_gc_scan_vm_specific_roots;
use crate::jl_gc_sweep_stack_pools_and_mtarraylist_buffers;
#[cfg(feature = "concurrentimmix")]
use crate::julia_types::_jl_task_t;
use crate::JuliaVM;
#[cfg(feature = "concurrentimmix")]
use dashmap::DashMap;
#[cfg(feature = "concurrentimmix")]
use std::sync::{Arc, Mutex};

pub(crate) struct StackRootBuffer {
    pub buffer: Vec<ObjectReference>,
}

impl SlotVisitor<JuliaVMSlot> for StackRootBuffer {
    fn visit_slot(&mut self, slot: JuliaVMSlot) {
        match slot {
            JuliaVMSlot::Simple(se) => {
                if let Some(object) = se.load() {
                    self.buffer.push(object);
                }
            }
            JuliaVMSlot::Offset(oe) => {
                if let Some(object) = oe.load() {
                    self.buffer.push(object);
                }
            }
        }
    }
}

#[cfg(feature = "concurrentimmix")]
lazy_static! {
    pub static ref GC_STACK_SNAPSHOTS: GCStackSnapshots = GCStackSnapshots::new();
}

pub struct VMScanning {}

impl Scanning<JuliaVM> for VMScanning {
    fn scan_roots_in_mutator_thread(
        _tls: VMWorkerThread,
        mutator: &'static mut Mutator<JuliaVM>,
        mut factory: impl RootsWorkFactory<JuliaVMSlot>,
    ) {
        use crate::julia_scanning::*;
        use crate::julia_types::*;
        use mmtk::util::Address;

        let ptls: &mut _jl_tls_states_t = unsafe { std::mem::transmute(mutator.mutator_tls) };
        let mut slot_buffer = StackRootBuffer { buffer: vec![] }; // need to be tpinned as they're all from the shadow stack
        let mut node_buffer = vec![];

        // Scan thread local from ptls: See gc_queue_thread_local in gc.c
        let mut root_scan_task = |task: *const _jl_task_t, task_is_root: bool| {
            if !task.is_null() {
                unsafe {
                    crate::julia_scanning::mmtk_scan_gcstack(task, &mut slot_buffer);
                }
                // Mark tasks scanned directly in this STW pause. Without this, a later concurrent
                // scan would find no record for it and trip the "not bound to a mutator" assert
                // in `gc_thread_scan_stack`, even though the task was already scanned here. The
                // roots themselves don't need to be recorded in GC_STACK_SNAPSHOTS: they were just fed into
                // `slot_buffer` above, as part of this pause's root-scan work.
                #[cfg(feature = "concurrentimmix")]
                GC_STACK_SNAPSHOTS.mark_pause_scanned(task);

                if task_is_root {
                    // captures wrong root nodes before creating the work
                    debug_assert!(
                        Address::from_ptr(task).is_aligned_to(16)
                            || Address::from_ptr(task).is_aligned_to(8),
                        "root node {:?} is not aligned to 8 or 16",
                        Address::from_ptr(task)
                    );

                    // unsafe: We checked `!task.is_null()` before.
                    let objref = unsafe {
                        ObjectReference::from_raw_address_unchecked(Address::from_ptr(task))
                    };
                    node_buffer.push(objref);
                }
            }
        };
        root_scan_task(ptls.root_task, true);

        // need to iterate over live tasks as well to process their shadow stacks
        // we should not set the task themselves as roots as we will know which ones are still alive after GC
        let mut i = 0;
        while i < ptls.gc_tls_common.heap.live_tasks.len {
            let mut task_address = Address::from_ptr(ptls.gc_tls_common.heap.live_tasks.items);
            task_address = task_address.shift::<Address>(i as isize);
            let task = unsafe { task_address.load::<*const jl_task_t>() };
            root_scan_task(task, false);
            i += 1;
        }

        root_scan_task(ptls.current_task as *mut _jl_task_t, true);
        root_scan_task(ptls.next_task, true);
        root_scan_task(ptls.previous_task, true);
        root_scan_task(ptls.abandon_victim, true);
        root_scan_task(ptls.abandon_to, true);
        if !ptls.abandon_result.is_null() {
            node_buffer.push(unsafe {
                // unsafe: We have just checked `ptls.abandon_result` is not null.
                ObjectReference::from_raw_address_unchecked(Address::from_mut_ptr(
                    ptls.abandon_result,
                ))
            });
        }
        if !ptls.previous_exception.is_null() {
            node_buffer.push(unsafe {
                // unsafe: We have just checked `ptls.previous_exception` is not null.
                ObjectReference::from_raw_address_unchecked(Address::from_mut_ptr(
                    ptls.previous_exception,
                ))
            });
        }

        // Scan backtrace buffer: See gc_queue_bt_buf in gc.c
        let mut i = 0;
        while i < ptls.bt_size {
            unsafe {
                let bt_entry = ptls.bt_data.add(i);
                let bt_entry_size = mmtk_jl_bt_entry_size(bt_entry);
                if mmtk_jl_bt_is_native(bt_entry) {
                    i += bt_entry_size;
                    continue;
                }
                let njlvals = mmtk_jl_bt_num_jlvals(bt_entry);
                for j in 0..njlvals {
                    let bt_entry_value = mmtk_jl_bt_entry_jlvalue(bt_entry, j);

                    // captures wrong root nodes before creating the work
                    debug_assert!(
                        bt_entry_value.to_raw_address().is_aligned_to(16)
                            || bt_entry_value.to_raw_address().is_aligned_to(8),
                        "root node {:?} is not aligned to 8 or 16",
                        bt_entry_value
                    );

                    node_buffer.push(bt_entry_value);
                }
                i += bt_entry_size;
            }
        }

        // We do not need gc_queue_remset from gc.c (we are not using remset in the thread)

        // Push work
        const CAPACITY_PER_PACKET: usize = 4096;
        for tpinning_roots in slot_buffer
            .buffer
            .chunks(CAPACITY_PER_PACKET)
            .map(|c| c.to_vec())
        {
            factory.create_process_tpinning_roots_work(tpinning_roots);
        }
        for nodes in node_buffer.chunks(CAPACITY_PER_PACKET).map(|c| c.to_vec()) {
            factory.create_process_pinning_roots_work(nodes);
        }

        // Flush per-mutator barrier/remset buffers before this scan packet is considered done.
        // mmtk-core will be moving this responsibility to the binding (see Task 2 of the plan).
        // For now we double-flush; that's safe because flush is idempotent (drains an empty
        // buffer the second time).
        mutator.flush();
    }

    fn scan_vm_specific_roots(
        _tls: VMWorkerThread,
        mut factory: impl RootsWorkFactory<JuliaVMSlot>,
    ) {
        use crate::slots::RootsWorkClosure;
        let mut roots_closure = RootsWorkClosure::from_roots_work_factory(&mut factory);
        unsafe {
            jl_gc_scan_vm_specific_roots(&mut roots_closure as _);
        }
    }

    fn scan_object(
        _tls: VMWorkerThread,
        object: ObjectReference,
        slot_visitor: &mut impl SlotVisitor<JuliaVMSlot>,
    ) {
        process_object(object, slot_visitor);
    }

    fn notify_initial_thread_scan_complete(_partial_scan: bool, _tls: VMWorkerThread) {}

    fn supports_return_barrier() -> bool {
        unimplemented!()
    }

    fn prepare_for_roots_re_scanning() {
        unimplemented!()
    }

    fn process_weak_refs(
        _worker: &mut GCWorker<JuliaVM>,
        tracer_context: impl ObjectTracerContext<JuliaVM>,
    ) -> bool {
        let single_thread_process_finalizer = ScanFinalizersSingleThreaded { tracer_context };
        memory_manager::add_work_packet(
            &SINGLETON,
            WorkBucketStage::VMRefClosure,
            single_thread_process_finalizer,
        );

        // We used to do this in the Compact stage, and add this work packet in notify_initial_thread_scan_complete.
        // But notify_initial_thread_scan_complete is always called, even if MMTK does not do weak reference scanning, which makes it not a good place to add the work packet.
        // I think it makes more sense to do this here -- if MMTK does not do weak ref scanning, this method will not be called and the work packet will not be added.
        let sweep_vm_specific_work = SweepVMSpecific::new();
        memory_manager::add_work_packet(
            &SINGLETON,
            WorkBucketStage::Release, // This has to happen after weak references are processed.
            sweep_vm_specific_work,
        );

        // We have pushed work. No need to repeat this method.
        false
    }
}

pub fn process_object<EV: SlotVisitor<JuliaVMSlot>>(object: ObjectReference, closure: &mut EV) {
    let addr = object.to_raw_address();
    unsafe {
        crate::julia_scanning::scan_julia_object(addr, closure);
    }
}

// Sweep malloced arrays work
pub struct SweepVMSpecific {
    swept: bool,
}

impl SweepVMSpecific {
    pub fn new() -> Self {
        Self { swept: false }
    }
}

impl Default for SweepVMSpecific {
    fn default() -> Self {
        Self::new()
    }
}

impl<VM: VMBinding> GCWork<VM> for SweepVMSpecific {
    fn do_work(&mut self, _worker: &mut GCWorker<VM>, _mmtk: &'static MMTK<VM>) {
        // call sweep malloced arrays, cancellation-source child lists, and sweep stack pools
        unsafe { jl_gc_mmtk_sweep_malloced_memory() }
        unsafe { crate::jl_gc_sweep_weak_processing() }
        unsafe { jl_gc_sweep_stack_pools_and_mtarraylist_buffers() }
        self.swept = true;
    }
}

pub struct ScanFinalizersSingleThreaded<C: ObjectTracerContext<JuliaVM>> {
    tracer_context: C,
}

impl<C: ObjectTracerContext<JuliaVM>> GCWork<JuliaVM> for ScanFinalizersSingleThreaded<C> {
    fn do_work(&mut self, worker: &mut GCWorker<JuliaVM>, _mmtk: &'static MMTK<JuliaVM>) {
        self.tracer_context.with_tracer(worker, |tracer| {
            crate::julia_finalizer::scan_finalizers_in_rust(tracer);
        });
    }
}

#[cfg(feature = "concurrentimmix")]
pub struct GCStackSnapshots {
    snapshots: DashMap<usize, Arc<[ObjectReference]>>,
    task_scan_locks: DashMap<usize, Arc<Mutex<()>>>,
}

#[cfg(feature = "concurrentimmix")]
impl GCStackSnapshots {
    fn new() -> Self {
        Self {
            snapshots: DashMap::new(),
            task_scan_locks: DashMap::new(),
        }
    }

    fn get_task_scan_lock(&self, task_key: usize) -> Arc<Mutex<()>> {
        self.task_scan_locks
            .entry(task_key)
            .or_insert_with(|| Arc::new(Mutex::new(())))
            .clone()
    }

    fn get_snapshot(&self, task: *const _jl_task_t) -> Option<Arc<[ObjectReference]>> {
        assert!(!task.is_null());

        self.snapshots
            .get(&(task as usize))
            .map(|snapshot| snapshot.clone())
    }

    /// Record that a task's stack was already scanned directly during the STW root scan, for
    /// tasks that won't otherwise get a record through `resume_barrier_scan_task` (see the call
    /// site in `scan_roots_in_mutator_thread`). The roots don't need to be stored here: they
    /// were already fed into that pause's root-scan work at the call site, so this is purely a
    /// marker telling `gc_thread_scan_stack` not to (unsafely) re-scan this task on demand.
    pub fn mark_pause_scanned(&self, task: *const _jl_task_t) {
        assert!(!task.is_null());

        self.snapshots
            .insert(task as usize, Arc::from(Vec::new().into_boxed_slice()));
    }

    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub fn gc_thread_scan_stack(&self, task: *const _jl_task_t) -> Option<Arc<[ObjectReference]>> {
        assert!(!task.is_null());

        if let Some(snapshot) = self.get_snapshot(task) {
            return Some(snapshot);
        }

        // `task.ptls` alone is not a reliable "is this task currently running" signal: when a
        // task's owning thread exits, `jl_delete_thread` clears that ptls's `current_task` but
        // never clears the task's own `ptls` field back, so a long-dead task can have a
        // permanently non-null, stale `ptls`. Checking the round trip -- does that ptls still
        // consider this task its current one -- is what actually distinguishes "genuinely
        // running on a live mutator" from "used to run here, but that thread is long gone".
        let ptls = unsafe { (*task).ptls };
        let bound_to_running_mutator =
            !ptls.is_null() && unsafe { (*ptls).current_task as *const _jl_task_t } == task;
        assert!(
            !bound_to_running_mutator,
            "Capturing the stack of a task bound to a mutator thread. This means the task might be running, and we try to snapshot its stack."
        );

        let task_key = task as usize;
        let task_lock = self.get_task_scan_lock(task_key);
        let _task_lock_guard = task_lock.lock().unwrap();

        if let Some(snapshot) = self.snapshots.get(&task_key) {
            Some(snapshot.clone())
        } else {
            let snapshot = self.capture_snapshot(task);
            self.snapshots.insert(task_key, snapshot.clone());
            Some(snapshot)
        }
    }

    pub fn resume_barrier_scan_task(&self, task: *const _jl_task_t) {
        assert!(!task.is_null());

        let task_key = task as usize;
        let task_lock = self.get_task_scan_lock(task_key);
        let _task_lock_guard = task_lock.lock().unwrap();
        if self.snapshots.contains_key(&task_key) {
            return;
        }
        self.snapshots.insert(task_key, self.capture_snapshot(task));
    }

    fn capture_snapshot(&self, task: *const _jl_task_t) -> Arc<[ObjectReference]> {
        let mut snapshot_roots = StackRootBuffer { buffer: vec![] };
        unsafe {
            crate::julia_scanning::mmtk_scan_gcstack(task, &mut snapshot_roots);
        }
        log::info!(
            "Took snapshot of stack roots for task {:?}, num roots = {}",
            task,
            snapshot_roots.buffer.len()
        );
        Arc::from(snapshot_roots.buffer.into_boxed_slice())
    }

    pub fn clear_snapshots(&self) {
        self.snapshots.clear();
        self.task_scan_locks.clear();
    }
}
