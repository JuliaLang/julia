use crate::edges::JuliaVMEdge;
use crate::{ROOT_EDGES, ROOT_NODES, SINGLETON, UPCALLS};
use mmtk::memory_manager;
use mmtk::scheduler::*;
use mmtk::util::opaque_pointer::*;
use mmtk::util::ObjectReference;
use mmtk::vm::EdgeVisitor;
use mmtk::vm::ObjectTracerContext;
use mmtk::vm::RootsWorkFactory;
use mmtk::vm::Scanning;
use mmtk::vm::VMBinding;
use mmtk::Mutator;
use mmtk::MMTK;

use crate::JuliaVM;
use log::info;
use std::collections::HashSet;
use std::sync::MutexGuard;

pub struct VMScanning {}

impl Scanning<JuliaVM> for VMScanning {
    fn scan_roots_in_all_mutator_threads(
        _tls: VMWorkerThread,
        _factory: impl RootsWorkFactory<JuliaVMEdge>,
    ) {
        // Thread roots are collected by Julia before stopping the world
    }

    fn scan_roots_in_mutator_thread(
        _tls: VMWorkerThread,
        _mutator: &'static mut Mutator<JuliaVM>,
        _factory: impl RootsWorkFactory<JuliaVMEdge>,
    ) {
        unimplemented!()
    }
    fn scan_vm_specific_roots(
        _tls: VMWorkerThread,
        mut factory: impl RootsWorkFactory<JuliaVMEdge>,
    ) {
        let mut roots: MutexGuard<HashSet<ObjectReference>> = ROOT_NODES.lock().unwrap();
        info!("{} thread root nodes", roots.len());

        let mut roots_to_scan = vec![];

        for obj in roots.drain() {
            roots_to_scan.push(obj);
        }
        factory.create_process_node_roots_work(roots_to_scan);

        let roots: Vec<JuliaVMEdge> = ROOT_EDGES
            .lock()
            .unwrap()
            .drain()
            .map(|e| JuliaVMEdge::Simple(mmtk::vm::edge_shape::SimpleEdge::from_address(e)))
            .collect();
        info!("{} thread root edges", roots.len());
        factory.create_process_edge_roots_work(roots);
    }

    fn scan_object<EV: EdgeVisitor<JuliaVMEdge>>(
        _tls: VMWorkerThread,
        object: ObjectReference,
        edge_visitor: &mut EV,
    ) {
        process_object(object, edge_visitor);
    }
    fn notify_initial_thread_scan_complete(_partial_scan: bool, _tls: VMWorkerThread) {
        let sweep_malloced_arrays_work = SweepMallocedArrays::new();
        memory_manager::add_work_packet(
            &SINGLETON,
            WorkBucketStage::Compact,
            sweep_malloced_arrays_work,
        );
    }
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

        // We have pushed work. No need to repeat this method.
        false
    }
}

pub fn process_object<EV: EdgeVisitor<JuliaVMEdge>>(object: ObjectReference, closure: &mut EV) {
    let addr = object.to_raw_address();
    unsafe {
        crate::julia_scanning::scan_julia_object(addr, closure);
    }
}

// Sweep malloced arrays work
pub struct SweepMallocedArrays {
    swept: bool,
}

impl SweepMallocedArrays {
    pub fn new() -> Self {
        Self { swept: false }
    }
}

impl<VM: VMBinding> GCWork<VM> for SweepMallocedArrays {
    fn do_work(&mut self, _worker: &mut GCWorker<VM>, _mmtk: &'static MMTK<VM>) {
        // call sweep malloced arrays from UPCALLS
        unsafe { ((*UPCALLS).mmtk_sweep_malloced_array)() }
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
