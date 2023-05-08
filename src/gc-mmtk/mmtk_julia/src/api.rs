// All functions here are extern function. There is no point for marking them as unsafe.
#![allow(clippy::not_unsafe_ptr_arg_deref)]

use crate::reference_glue::JuliaFinalizableObject;
use crate::JuliaVM;
use crate::Julia_Upcalls;
use crate::BLOCK_FOR_GC;
use crate::FINALIZER_ROOTS;
use crate::SINGLETON;
use crate::UPCALLS;
use crate::{
    get_mutator_ref, set_julia_obj_header_size, ARE_MUTATORS_BLOCKED, BUILDER, DISABLED_GC,
    FINALIZERS_RUNNING, MUTATORS, MUTATOR_TLS, USER_TRIGGERED_GC,
};
use libc::c_char;
use log::info;
use mmtk::memory_manager;
use mmtk::scheduler::GCController;
use mmtk::scheduler::GCWorker;
use mmtk::util::opaque_pointer::*;
use mmtk::util::{Address, ObjectReference, OpaquePointer};
use mmtk::AllocationSemantics;
use mmtk::Mutator;
use std::collections::HashMap;
use std::ffi::CStr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::RwLockWriteGuard;

#[no_mangle]
pub extern "C" fn gc_init(
    min_heap_size: usize,
    max_heap_size: usize,
    calls: *const Julia_Upcalls,
    header_size: usize,
) {
    unsafe {
        UPCALLS = calls;
        set_julia_obj_header_size(header_size);
    };

    {
        let mut builder = BUILDER.lock().unwrap();
        use mmtk::util::options::PlanSelector;
        let force_plan = if cfg!(feature = "nogc") {
            Some(PlanSelector::NoGC)
        } else if cfg!(feature = "semispace") {
            Some(PlanSelector::SemiSpace)
        } else if cfg!(feature = "gencopy") {
            Some(PlanSelector::GenCopy)
        } else if cfg!(feature = "marksweep") {
            Some(PlanSelector::MarkSweep)
        } else if cfg!(feature = "markcompact") {
            Some(PlanSelector::MarkCompact)
        } else if cfg!(feature = "pageprotect") {
            Some(PlanSelector::PageProtect)
        } else if cfg!(feature = "immix") {
            Some(PlanSelector::Immix)
        } else {
            None
        };
        if let Some(plan) = force_plan {
            builder.options.plan.set(plan);
        }
        let success;
        if min_heap_size != 0 {
            info!(
                "Setting mmtk heap size to a variable size with min-max of {}-{} (in bytes)",
                min_heap_size, max_heap_size
            );
            success = builder.options.gc_trigger.set(
                mmtk::util::options::GCTriggerSelector::DynamicHeapSize(
                    min_heap_size,
                    max_heap_size,
                ),
            );
        } else {
            info!(
                "Setting mmtk heap size to a fixed max of {} (in bytes)",
                max_heap_size
            );
            success = builder.options.gc_trigger.set(
                mmtk::util::options::GCTriggerSelector::FixedHeapSize(max_heap_size),
            );
        }
        assert!(
            success,
            "Failed to set heap size to {}-{}",
            min_heap_size, max_heap_size
        );
        let success = builder.options.no_reference_types.set(false);
        assert!(success, "Failed to set no_reference_types to false");
    }

    // Make sure that we haven't initialized MMTk (by accident) yet
    assert!(!crate::MMTK_INITIALIZED.load(Ordering::SeqCst));
    // Make sure we initialize MMTk here
    lazy_static::initialize(&SINGLETON);
}

#[no_mangle]
pub extern "C" fn start_control_collector(
    tls: VMWorkerThread,
    gc_controller: *mut GCController<JuliaVM>,
) {
    let mut gc_controller = unsafe { Box::from_raw(gc_controller) };
    memory_manager::start_control_collector(&SINGLETON, tls, &mut gc_controller);
}

#[no_mangle]
pub extern "C" fn bind_mutator(tls: VMMutatorThread, tid: usize) -> *mut Mutator<JuliaVM> {
    let mut are_mutators_blocked: RwLockWriteGuard<HashMap<usize, AtomicBool>> =
        ARE_MUTATORS_BLOCKED.write().unwrap();
    are_mutators_blocked.insert(tid, AtomicBool::new(false));
    let mutator_box = memory_manager::bind_mutator(&SINGLETON, tls);

    let res = Box::into_raw(mutator_box);

    let mutator_ref = unsafe { get_mutator_ref(res) };

    info!("Binding mutator {:?} to thread id = {}", res, tid);

    MUTATORS.write().unwrap().push(mutator_ref);

    let tls_str = format!("{:?}", tls.0);
    MUTATOR_TLS.write().unwrap().insert(tls_str);
    res
}

#[no_mangle]
pub extern "C" fn add_mutator_ref(mutator_ref: ObjectReference) {
    MUTATORS.write().unwrap().push(mutator_ref);
}

#[no_mangle]
pub extern "C" fn destroy_mutator(mutator: *mut Mutator<JuliaVM>) {
    memory_manager::destroy_mutator(unsafe { &mut *mutator })
}

#[no_mangle]
pub extern "C" fn alloc(
    mutator: *mut Mutator<JuliaVM>,
    size: usize,
    align: usize,
    offset: isize,
    semantics: AllocationSemantics,
) -> Address {
    memory_manager::alloc::<JuliaVM>(unsafe { &mut *mutator }, size, align, offset, semantics)
}

#[no_mangle]
pub extern "C" fn alloc_large(
    mutator: *mut Mutator<JuliaVM>,
    size: usize,
    align: usize,
    offset: isize,
) -> Address {
    memory_manager::alloc::<JuliaVM>(
        unsafe { &mut *mutator },
        size,
        align,
        offset,
        AllocationSemantics::Los,
    )
}

#[no_mangle]
pub extern "C" fn post_alloc(
    mutator: *mut Mutator<JuliaVM>,
    refer: ObjectReference,
    bytes: usize,
    semantics: AllocationSemantics,
) {
    match semantics {
        AllocationSemantics::Los => {
            memory_manager::post_alloc::<JuliaVM>(unsafe { &mut *mutator }, refer, bytes, semantics)
        }
        _ => {
            memory_manager::post_alloc::<JuliaVM>(unsafe { &mut *mutator }, refer, bytes, semantics)
        }
    }
}

#[no_mangle]
pub extern "C" fn will_never_move(object: ObjectReference) -> bool {
    !object.is_movable()
}

#[no_mangle]
pub extern "C" fn start_worker(tls: VMWorkerThread, worker: *mut GCWorker<JuliaVM>) {
    let mut worker = unsafe { Box::from_raw(worker) };
    memory_manager::start_worker::<JuliaVM>(&SINGLETON, tls, &mut worker)
}

#[no_mangle]
pub extern "C" fn initialize_collection(tls: VMThread) {
    memory_manager::initialize_collection(&SINGLETON, tls);
}

#[no_mangle]
pub extern "C" fn enable_collection() {
    if AtomicBool::load(&DISABLED_GC, Ordering::SeqCst) {
        memory_manager::enable_collection(&SINGLETON);
        AtomicBool::store(&DISABLED_GC, false, Ordering::SeqCst);
    }
}

#[no_mangle]
pub extern "C" fn disable_collection() {
    if AtomicBool::load(&DISABLED_GC, Ordering::SeqCst) == false {
        AtomicBool::store(&DISABLED_GC, true, Ordering::SeqCst);
        memory_manager::disable_collection(&SINGLETON);
    }

    // if user has triggered GC, wait until GC is finished
    while AtomicBool::load(&USER_TRIGGERED_GC, Ordering::SeqCst)
        || AtomicBool::load(&BLOCK_FOR_GC, Ordering::SeqCst)
    {
        info!("Waiting for a triggered gc to finish...");
        unsafe { ((*UPCALLS).wait_in_a_safepoint)() };
    }
}

#[no_mangle]
pub extern "C" fn used_bytes() -> usize {
    memory_manager::used_bytes(&SINGLETON)
}

#[no_mangle]
pub extern "C" fn free_bytes() -> usize {
    memory_manager::free_bytes(&SINGLETON)
}

#[no_mangle]
pub extern "C" fn total_bytes() -> usize {
    memory_manager::total_bytes(&SINGLETON)
}

#[no_mangle]
pub extern "C" fn is_live_object(object: ObjectReference) -> bool {
    object.is_live()
}

#[no_mangle]
pub extern "C" fn is_mapped_address(address: Address) -> bool {
    address.is_mapped()
}

#[no_mangle]
pub extern "C" fn modify_check(object: ObjectReference) {
    memory_manager::modify_check(&SINGLETON, object)
}

#[no_mangle]
pub extern "C" fn handle_user_collection_request(tls: VMMutatorThread) {
    AtomicBool::store(&USER_TRIGGERED_GC, true, Ordering::SeqCst);
    if AtomicBool::load(&DISABLED_GC, Ordering::SeqCst) {
        AtomicBool::store(&USER_TRIGGERED_GC, false, Ordering::SeqCst);
        return;
    }
    memory_manager::handle_user_collection_request::<JuliaVM>(&SINGLETON, tls);
}

#[no_mangle]
pub extern "C" fn mmtk_add_weak_candidate(reff: ObjectReference) {
    memory_manager::add_weak_candidate(&SINGLETON, reff)
}

#[no_mangle]
pub extern "C" fn mmtk_add_soft_candidate(reff: ObjectReference) {
    memory_manager::add_soft_candidate(&SINGLETON, reff)
}

#[no_mangle]
pub extern "C" fn mmtk_add_phantom_candidate(reff: ObjectReference) {
    memory_manager::add_phantom_candidate(&SINGLETON, reff)
}

#[no_mangle]
pub extern "C" fn harness_begin(tls: VMMutatorThread) {
    memory_manager::harness_begin(&SINGLETON, tls)
}

#[no_mangle]
pub extern "C" fn harness_end(_tls: OpaquePointer) {
    memory_manager::harness_end(&SINGLETON)
}

#[no_mangle]
pub extern "C" fn register_finalizer(
    obj: ObjectReference,
    finalizer_fn: Address,
    is_obj_ptr: bool,
) {
    memory_manager::add_finalizer(
        &SINGLETON,
        JuliaFinalizableObject(obj, finalizer_fn, is_obj_ptr),
    );
}

#[no_mangle]
pub extern "C" fn run_finalizers_for_obj(obj: ObjectReference) {
    let finalizers_running = AtomicBool::load(&FINALIZERS_RUNNING, Ordering::SeqCst);

    if !finalizers_running {
        AtomicBool::store(&FINALIZERS_RUNNING, true, Ordering::SeqCst);
    }

    let finalizable_objs = memory_manager::get_finalizers_for(&SINGLETON, obj);

    for obj in finalizable_objs.iter() {
        // if the finalizer function triggers GC you don't want the object to be GC-ed
        {
            let mut fin_roots = FINALIZER_ROOTS.write().unwrap();
            let inserted = fin_roots.insert(*obj);
            assert!(inserted);
        }
    }

    for obj in finalizable_objs {
        unsafe { ((*UPCALLS).run_finalizer_function)(obj.0, obj.1, obj.2) }
        {
            let mut fin_roots = FINALIZER_ROOTS.write().unwrap();
            let removed = fin_roots.remove(&obj);
            assert!(removed);
        }
    }

    if !finalizers_running {
        AtomicBool::store(&FINALIZERS_RUNNING, false, Ordering::SeqCst);
    }
}

#[no_mangle]
pub extern "C" fn process(name: *const c_char, value: *const c_char) -> bool {
    let name_str: &CStr = unsafe { CStr::from_ptr(name) };
    let value_str: &CStr = unsafe { CStr::from_ptr(value) };
    let mut builder = BUILDER.lock().unwrap();
    memory_manager::process(
        &mut builder,
        name_str.to_str().unwrap(),
        value_str.to_str().unwrap(),
    )
}

#[no_mangle]
pub extern "C" fn starting_heap_address() -> Address {
    memory_manager::starting_heap_address()
}

#[no_mangle]
pub extern "C" fn last_heap_address() -> Address {
    memory_manager::last_heap_address()
}

#[no_mangle]
#[cfg(feature = "malloc_counted_size")]
pub extern "C" fn mmtk_counted_malloc(size: usize) -> Address {
    memory_manager::counted_malloc::<JuliaVM>(&SINGLETON, size)
}

#[no_mangle]
pub extern "C" fn mmtk_malloc(size: usize) -> Address {
    memory_manager::malloc(size)
}

#[no_mangle]
#[cfg(feature = "malloc_counted_size")]
pub extern "C" fn mmtk_malloc_aligned(size: usize, align: usize) -> Address {
    // allocate extra bytes to account for original memory that needs to be allocated and its size
    let ptr_size = std::mem::size_of::<Address>();
    let size_size = std::mem::size_of::<usize>();
    assert!(align % ptr_size == 0 && align != 0 && (align / ptr_size).is_power_of_two());

    let extra = (align - 1) + ptr_size + size_size;
    let mem = memory_manager::counted_malloc(&SINGLETON, size + extra);
    let result = (mem + extra) & !(align - 1);
    let result = unsafe { Address::from_usize(result) };

    unsafe {
        (result - ptr_size).store::<Address>(mem);
        (result - ptr_size - size_size).store::<usize>(size + extra);
    }

    return result;
}

#[no_mangle]
#[cfg(feature = "malloc_counted_size")]
pub extern "C" fn mmtk_counted_calloc(num: usize, size: usize) -> Address {
    memory_manager::counted_calloc::<JuliaVM>(&SINGLETON, num, size)
}

#[no_mangle]
pub extern "C" fn mmtk_calloc(num: usize, size: usize) -> Address {
    memory_manager::calloc(num, size)
}

#[no_mangle]
#[cfg(feature = "malloc_counted_size")]
pub extern "C" fn mmtk_realloc_with_old_size(
    addr: Address,
    size: usize,
    old_size: usize,
) -> Address {
    memory_manager::realloc_with_old_size::<JuliaVM>(&SINGLETON, addr, size, old_size)
}

#[no_mangle]
pub extern "C" fn mmtk_realloc(addr: Address, size: usize) -> Address {
    memory_manager::realloc(addr, size)
}

#[no_mangle]
#[cfg(feature = "malloc_counted_size")]
pub extern "C" fn mmtk_free_with_size(addr: Address, old_size: usize) {
    memory_manager::free_with_size::<JuliaVM>(&SINGLETON, addr, old_size)
}

#[no_mangle]
pub extern "C" fn mmtk_free(addr: Address) {
    memory_manager::free(addr)
}

#[no_mangle]
#[cfg(feature = "malloc_counted_size")]
pub extern "C" fn mmtk_free_aligned(addr: Address) {
    let ptr_size = std::mem::size_of::<Address>();
    let size_size = std::mem::size_of::<usize>();

    let (addr, old_size) = unsafe {
        (
            (addr - ptr_size).load::<Address>(),
            (addr - ptr_size - size_size).load::<usize>(),
        )
    };

    memory_manager::free_with_size::<JuliaVM>(&SINGLETON, addr, old_size);
}

#[no_mangle]
pub extern "C" fn mmtk_gc_poll(tls: VMMutatorThread) {
    memory_manager::gc_poll(&SINGLETON, tls);
}

#[no_mangle]
pub extern "C" fn runtime_panic() {
    panic!("Panicking at runtime!")
}

#[no_mangle]
#[allow(mutable_transmutes)]
pub extern "C" fn mmtk_set_vm_space(start: Address, size: usize) {
    let mmtk: &mmtk::MMTK<JuliaVM> = &SINGLETON;
    let mmtk_mut: &mut mmtk::MMTK<JuliaVM> = unsafe { std::mem::transmute(mmtk) };
    memory_manager::lazy_init_vm_space(mmtk_mut, start, size);
}

#[no_mangle]
pub extern "C" fn mmtk_memory_region_copy(
    mutator: *mut Mutator<JuliaVM>,
    src_obj: ObjectReference,
    src_addr: Address,
    dst_obj: ObjectReference,
    dst_addr: Address,
    count: usize,
) {
    use crate::edges::JuliaMemorySlice;
    let src = JuliaMemorySlice {
        owner: src_obj,
        start: src_addr,
        count,
    };
    let dst = JuliaMemorySlice {
        owner: dst_obj,
        start: dst_addr,
        count,
    };
    let mutator = unsafe { &mut *mutator };
    memory_manager::memory_region_copy(mutator, src, dst);
}

#[no_mangle]
pub extern "C" fn mmtk_object_reference_write_post(
    mutator: *mut Mutator<JuliaVM>,
    src: ObjectReference,
    target: ObjectReference,
) {
    let mutator = unsafe { &mut *mutator };
    memory_manager::object_reference_write_post(
        mutator,
        src,
        crate::edges::JuliaVMEdge::Simple(mmtk::vm::edge_shape::SimpleEdge::from_address(
            Address::ZERO,
        )),
        target,
    )
}

#[no_mangle]
pub extern "C" fn mmtk_needs_write_barrier() -> u8 {
    use mmtk::plan::BarrierSelector;
    match SINGLETON.get_plan().constraints().barrier {
        BarrierSelector::NoBarrier => 0,
        BarrierSelector::ObjectBarrier => 1,
    }
}
