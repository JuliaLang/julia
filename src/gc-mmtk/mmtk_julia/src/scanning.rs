use crate::edges::JuliaVMEdge;
#[cfg(feature = "scan_obj_c")]
use crate::julia_scanning::process_edge;
#[cfg(feature = "scan_obj_c")]
use crate::julia_scanning::process_offset_edge;
use crate::FINALIZER_ROOTS;
use crate::{ROOTS, SINGLETON, UPCALLS};
use mmtk::memory_manager;
use mmtk::scheduler::*;
use mmtk::util::opaque_pointer::*;
use mmtk::util::Address;
use mmtk::util::ObjectReference;
use mmtk::vm::EdgeVisitor;
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
    fn scan_thread_roots(_tls: VMWorkerThread, _factory: impl RootsWorkFactory<JuliaVMEdge>) {
        // Thread roots are collected by Julia before stopping the world
    }

    fn scan_thread_root(
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
        let mut roots: MutexGuard<HashSet<Address>> = ROOTS.lock().unwrap();
        info!("{} thread roots", roots.len());

        let mut roots_to_scan = vec![];

        for obj in roots.drain() {
            let obj_ref = ObjectReference::from_raw_address(obj);
            roots_to_scan.push(obj_ref);
        }

        let fin_roots = FINALIZER_ROOTS.read().unwrap();

        // processing finalizer roots
        for obj in fin_roots.iter() {
            if !obj.2 {
                // not a void pointer
                let obj_ref = ObjectReference::from_raw_address((*obj).1);
                roots_to_scan.push(obj_ref);
            }
            roots_to_scan.push((*obj).0);
        }

        factory.create_process_node_roots_work(roots_to_scan);
    }

    fn scan_object<EV: EdgeVisitor<JuliaVMEdge>>(
        _tls: VMWorkerThread,
        object: ObjectReference,
        edge_visitor: &mut EV,
    ) {
        process_object(object, edge_visitor);
    }
    fn notify_initial_thread_scan_complete(_partial_scan: bool, _tls: VMWorkerThread) {
        // Specific to JikesRVM - using it to load the work for sweeping malloced arrays
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
}

pub fn process_object(object: ObjectReference, closure: &mut dyn EdgeVisitor<JuliaVMEdge>) {
    let addr = object.to_raw_address();

    #[cfg(feature = "scan_obj_c")]
    {
        unsafe {
            ((*UPCALLS).scan_julia_obj)(addr, closure, process_edge as _, process_offset_edge as _)
        };
    }

    #[cfg(not(feature = "scan_obj_c"))]
    unsafe {
        crate::julia_scanning::scan_julia_object(addr, closure);
    }
}

#[no_mangle]
pub extern "C" fn object_is_managed_by_mmtk(addr: usize) -> bool {
    crate::api::is_mapped_address(unsafe { Address::from_usize(addr) })
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
