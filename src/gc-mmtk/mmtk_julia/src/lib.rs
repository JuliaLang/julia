extern crate libc;
extern crate log;
extern crate mmtk;
#[macro_use]
extern crate lazy_static;

use mmtk::util::opaque_pointer::*;
use mmtk::util::Address;
use mmtk::vm::VMBinding;
use mmtk::MMTKBuilder;
use mmtk::MMTK;

use std::collections::HashMap;
use std::ptr::null_mut;
use std::sync::atomic::AtomicIsize;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Condvar, Mutex, RwLock};

pub mod active_plan;
pub mod api;
pub mod collection;
pub mod edges;
pub mod object_model;
pub mod reference_glue;
pub mod scanning;
pub mod util;

pub mod julia_finalizer;
pub mod julia_scanning;
#[allow(non_camel_case_types)]
#[allow(improper_ctypes_definitions)]
#[allow(non_upper_case_globals)]
#[allow(non_snake_case)]
pub mod julia_types;

#[derive(Default)]
pub struct JuliaVM;

use crate::edges::JuliaVMEdge;

impl VMBinding for JuliaVM {
    const MAX_ALIGNMENT: usize = 64;
    const MIN_ALIGNMENT: usize = 4;
    type VMObjectModel = object_model::VMObjectModel;
    type VMScanning = scanning::VMScanning;
    type VMCollection = collection::VMCollection;
    type VMActivePlan = active_plan::VMActivePlan;
    type VMReferenceGlue = reference_glue::VMReferenceGlue;
    type VMMemorySlice = edges::JuliaMemorySlice;
    type VMEdge = JuliaVMEdge;
}

/// This is used to ensure we initialize MMTk at a specified timing.
pub static MMTK_INITIALIZED: AtomicBool = AtomicBool::new(false);

lazy_static! {
    pub static ref BUILDER: Mutex<MMTKBuilder> = Mutex::new(MMTKBuilder::new());
    pub static ref SINGLETON: MMTK<JuliaVM> = {
        let builder = BUILDER.lock().unwrap();
        debug_assert!(!MMTK_INITIALIZED.load(Ordering::SeqCst));
        let ret = mmtk::memory_manager::mmtk_init(&builder);
        MMTK_INITIALIZED.store(true, std::sync::atomic::Ordering::Relaxed);
        *ret
    };
}

pub static mut JULIA_HEADER_SIZE: usize = 0;
pub static mut JULIA_BUFF_TAG: usize = 0;

#[no_mangle]
pub static BLOCK_FOR_GC: AtomicBool = AtomicBool::new(false);

#[no_mangle]
pub static WORLD_HAS_STOPPED: AtomicBool = AtomicBool::new(false);

#[no_mangle]
pub static DISABLED_GC: AtomicBool = AtomicBool::new(false);

#[no_mangle]
pub static USER_TRIGGERED_GC: AtomicIsize = AtomicIsize::new(0);

lazy_static! {
    pub static ref STW_COND: Arc<(Mutex<usize>, Condvar)> =
        Arc::new((Mutex::new(0), Condvar::new()));
    pub static ref STOP_MUTATORS: Arc<(Mutex<usize>, Condvar)> =
        Arc::new((Mutex::new(0), Condvar::new()));

    // We create a boxed mutator with MMTk core, and we mem copy its content to jl_tls_state_t (shallow copy).
    // This map stores the pair of the mutator address in jl_tls_state_t and the original boxed mutator.
    // As we only do a shallow copy, we should not free the original boxed mutator, until the thread is getting destroyed.
    // Otherwise, we will have dangling pointers.
    pub static ref MUTATORS: RwLock<HashMap<Address, Address>> = RwLock::new(HashMap::new());
}

type ProcessEdgeFn = *const extern "C" fn(closure: Address, slot: Address);

#[repr(C)]
pub struct Julia_Upcalls {
    pub scan_julia_exc_obj:
        extern "C" fn(obj: Address, closure: Address, process_edge: ProcessEdgeFn),
    pub get_stackbase: extern "C" fn(tid: u16) -> usize,
    pub mmtk_jl_run_finalizers: extern "C" fn(tls: OpaquePointer),
    pub jl_throw_out_of_memory_error: extern "C" fn(),
    pub mmtk_sweep_malloced_array: extern "C" fn(),
    pub wait_in_a_safepoint: extern "C" fn(),
    pub exit_from_safepoint: extern "C" fn(old_state: i8),
    pub jl_hrtime: extern "C" fn() -> u64,
    pub update_gc_time: extern "C" fn(u64),
    pub get_abi_structs_checksum_c: extern "C" fn() -> usize,
    pub get_thread_finalizer_list: extern "C" fn(tls: OpaquePointer) -> Address,
    pub get_to_finalize_list: extern "C" fn() -> Address,
    pub get_marked_finalizers_list: extern "C" fn() -> Address,
    pub arraylist_grow: extern "C" fn(Address, usize),
    pub get_jl_gc_have_pending_finalizers: extern "C" fn() -> *mut i32,
    pub scan_vm_specific_roots: extern "C" fn(closure: *mut crate::edges::RootsWorkClosure),
    pub prepare_to_collect: extern "C" fn(),
}

pub static mut UPCALLS: *const Julia_Upcalls = null_mut();
