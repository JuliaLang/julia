use crate::edges::JuliaVMEdge;
use crate::{SINGLETON, UPCALLS};
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

pub struct VMScanning {}

impl Scanning<JuliaVM> for VMScanning {
    fn scan_roots_in_mutator_thread(
        _tls: VMWorkerThread,
        mutator: &'static mut Mutator<JuliaVM>,
        mut factory: impl RootsWorkFactory<JuliaVMEdge>,
    ) {
        // This allows us to reuse mmtk_scan_gcstack which expectes an EdgeVisitor
        struct EdgeBuffer {
            pub buffer: Vec<JuliaVMEdge>,
        }
        impl mmtk::vm::EdgeVisitor<JuliaVMEdge> for EdgeBuffer {
            fn visit_edge(&mut self, edge: JuliaVMEdge) {
                self.buffer.push(edge);
            }
        }

        use crate::julia_scanning::*;
        use crate::julia_types::*;
        use mmtk::util::Address;

        let ptls: &mut mmtk__jl_tls_states_t = unsafe { std::mem::transmute(mutator.mutator_tls) };
        let mut edge_buffer = EdgeBuffer { buffer: vec![] };
        let mut node_buffer = vec![];

        // Scan thread local from ptls: See gc_queue_thread_local in gc.c
        let mut root_scan_task = |task: *const mmtk__jl_task_t| {
            if !task.is_null() {
                unsafe {
                    crate::julia_scanning::mmtk_scan_gcstack(task, &mut edge_buffer);
                }
                node_buffer.push(ObjectReference::from_raw_address(Address::from_ptr(task)));
            }
        };
        root_scan_task(ptls.root_task);
        root_scan_task(ptls.current_task as *mut mmtk__jl_task_t);
        root_scan_task(ptls.next_task);
        root_scan_task(ptls.previous_task);
        if !ptls.previous_exception.is_null() {
            node_buffer.push(ObjectReference::from_raw_address(Address::from_mut_ptr(
                ptls.previous_exception,
            )));
        }

        // Scan backtrace buffer: See gc_queue_bt_buf in gc.c
        let mut i = 0;
        while i < ptls.bt_size {
            let bt_entry = unsafe { ptls.bt_data.add(i) };
            let bt_entry_size = mmtk_jl_bt_entry_size(bt_entry);
            if mmtk_jl_bt_is_native(bt_entry) {
                i += bt_entry_size;
                continue;
            }
            let njlvals = mmtk_jl_bt_num_jlvals(bt_entry);
            for j in 0..njlvals {
                let bt_entry_value = mmtk_jl_bt_entry_jlvalue(bt_entry, j);
                node_buffer.push(bt_entry_value);
            }
            i += bt_entry_size;
        }

        // We do not need gc_queue_remset from gc.c (we are not using remset in the thread)

        // Push work
        const CAPACITY_PER_PACKET: usize = 4096;
        for edges in edge_buffer
            .buffer
            .chunks(CAPACITY_PER_PACKET)
            .map(|c| c.to_vec())
        {
            factory.create_process_edge_roots_work(edges);
        }
        for nodes in node_buffer.chunks(CAPACITY_PER_PACKET).map(|c| c.to_vec()) {
            factory.create_process_node_roots_work(nodes);
        }
    }

    fn scan_vm_specific_roots(
        _tls: VMWorkerThread,
        mut factory: impl RootsWorkFactory<JuliaVMEdge>,
    ) {
        use crate::edges::RootsWorkClosure;
        let mut roots_closure = RootsWorkClosure::from_roots_work_factory(&mut factory);
        unsafe {
            ((*UPCALLS).scan_vm_specific_roots)(&mut roots_closure as _);
        }
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
