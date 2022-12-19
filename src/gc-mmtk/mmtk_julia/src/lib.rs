extern crate libc;
extern crate log;
extern crate mmtk;
#[macro_use]
extern crate lazy_static;

use mmtk::scheduler::*;
use mmtk::util::opaque_pointer::*;
use mmtk::util::Address;
use mmtk::util::ObjectReference;
use mmtk::vm::edge_shape;
use mmtk::vm::EdgeVisitor;
use mmtk::vm::VMBinding;
use mmtk::MMTKBuilder;
use mmtk::Mutator;
use mmtk::MMTK;
use reference_glue::JuliaFinalizableObject;

use std::collections::{HashMap, HashSet};
use std::ptr::null_mut;
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
    type VMMemorySlice = edge_shape::UnimplementedMemorySlice<JuliaVMEdge>;
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

#[link(kind = "static", name = "runtime_gc_c")]
extern "C" {
    pub static BLOCK_FOR_GC: AtomicBool;
    pub static CURRENT_BLOCK_SIZE: usize;
    pub static WORLD_HAS_STOPPED: AtomicBool;
    pub static JULIA_HEADER_SIZE: usize;
    pub static BI_METADATA_START_ALIGNED_DOWN: usize;
    pub static BI_METADATA_END_ALIGNED_UP: usize;
}

#[no_mangle]
pub static SCHEDULED_FINALIZATION: AtomicBool = AtomicBool::new(false);

#[no_mangle]
pub static FINALIZERS_RUNNING: AtomicBool = AtomicBool::new(false);

#[no_mangle]
pub static DISABLED_GC: AtomicBool = AtomicBool::new(false);

#[no_mangle]
pub static USER_TRIGGERED_GC: AtomicBool = AtomicBool::new(false);

lazy_static! {
    pub static ref ARE_MUTATORS_BLOCKED: RwLock<HashMap<usize, AtomicBool>> =
        RwLock::new(HashMap::new());
    pub static ref STW_COND: Arc<(Mutex<usize>, Condvar)> =
        Arc::new((Mutex::new(0), Condvar::new()));
    pub static ref UC_COND: Arc<(Mutex<usize>, Condvar)> =
        Arc::new((Mutex::new(0), Condvar::new()));
    pub static ref STOP_MUTATORS: Arc<(Mutex<usize>, Condvar)> =
        Arc::new((Mutex::new(0), Condvar::new()));
    pub static ref ROOTS: Mutex<HashSet<Address>> = { Mutex::new(HashSet::new()) };
    pub static ref FINALIZER_ROOTS: RwLock<Vec<JuliaFinalizableObject>> = { RwLock::new(vec![]) };
    pub static ref MUTATOR_TLS: RwLock<HashSet<String>> = RwLock::new(HashSet::new());
    pub static ref MUTATORS: RwLock<Vec<ObjectReference>> = RwLock::new(vec![]);
}

#[link(name = "runtime_gc_c")]
#[allow(improper_ctypes)]
extern "C" {
    pub fn spawn_collector_thread(tls: VMThread, ctx: *mut GCWorker<JuliaVM>, kind: i32);
    pub fn set_julia_obj_header_size(size: usize);
    pub fn increase_alloc_number();
    pub fn reset_mutator_count();
    pub fn get_next_julia_mutator() -> usize;
    pub fn get_mutator_ref(mutator: *mut Mutator<JuliaVM>) -> ObjectReference;
    pub fn get_mutator_from_ref(mutator: ObjectReference) -> *mut Mutator<JuliaVM>;
    pub fn init_boot_image_metadata_info(start_aligned_down: usize, end_aligned_up: usize);
}

type ProcessEdgeFn =
    *const extern "C" fn(closure: &mut dyn EdgeVisitor<JuliaVMEdge>, slot: Address);

type ProcessOffsetEdgeFn =
    *const extern "C" fn(closure: &mut dyn EdgeVisitor<JuliaVMEdge>, slot: Address, offset: usize);

#[repr(C)]
pub struct Julia_Upcalls {
    pub scan_julia_obj: extern "C" fn(
        obj: Address,
        closure: &mut dyn EdgeVisitor<JuliaVMEdge>,
        process_edge: ProcessEdgeFn,
        process_offset_edge: ProcessOffsetEdgeFn,
    ),
    pub scan_julia_exc_obj: extern "C" fn(
        obj: Address,
        closure: &mut dyn EdgeVisitor<JuliaVMEdge>,
        process_edge: ProcessEdgeFn,
    ),
    pub get_stackbase: extern "C" fn(tid: u16) -> u64,
    pub calculate_roots: extern "C" fn(tls: OpaquePointer),
    pub run_finalizer_function:
        extern "C" fn(obj: ObjectReference, function: Address, is_ptr: bool),
    pub get_jl_last_err: extern "C" fn() -> u64,
    pub set_jl_last_err: extern "C" fn(errno: u64),
    pub get_lo_size: extern "C" fn(object: ObjectReference) -> u64,
    pub get_so_size: extern "C" fn(object: ObjectReference) -> u64,
    pub get_object_start_ref: extern "C" fn(object: ObjectReference) -> Address,
    pub wait_for_the_world: extern "C" fn(),
    pub set_gc_initial_state: extern "C" fn(tls: OpaquePointer) -> i8,
    pub set_gc_final_state: extern "C" fn(old_state: i8),
    pub set_gc_old_state: extern "C" fn(old_state: i8),
    pub mmtk_jl_run_finalizers: extern "C" fn(tls: OpaquePointer),
    pub jl_throw_out_of_memory_error: extern "C" fn(),
    pub mark_julia_object_as_scanned: extern "C" fn(obj: Address),
    pub julia_object_has_been_scanned: extern "C" fn(obj: Address) -> u8,
    pub mmtk_sweep_malloced_array: extern "C" fn(),
    pub wait_in_a_safepoint: extern "C" fn() -> i8,
    pub exit_from_safepoint: extern "C" fn(old_state: i8),
}

pub static mut UPCALLS: *const Julia_Upcalls = null_mut();
