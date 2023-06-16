// All functions here are extern function. There is no point for marking them as unsafe.
#![allow(clippy::not_unsafe_ptr_arg_deref)]

use crate::reference_glue::JuliaFinalizableObject;
use crate::JuliaVM;
use crate::Julia_Upcalls;
use crate::BLOCK_FOR_GC;
use crate::FINALIZER_ROOTS;
use crate::JULIA_HEADER_SIZE;
use crate::SINGLETON;
use crate::UPCALLS;
use crate::{
    set_julia_obj_header_size, BUILDER, DISABLED_GC, FINALIZERS_RUNNING, MUTATORS,
    USER_TRIGGERED_GC,
};
use crate::{ROOT_EDGES, ROOT_NODES};

use libc::c_char;
use log::*;
use mmtk::memory_manager;
use mmtk::scheduler::GCController;
use mmtk::scheduler::GCWorker;
use mmtk::util::opaque_pointer::*;
use mmtk::util::{Address, ObjectReference, OpaquePointer};
use mmtk::AllocationSemantics;
use mmtk::Mutator;
use std::ffi::CStr;
use std::sync::atomic::{AtomicBool, Ordering};

#[no_mangle]
pub extern "C" fn mmtk_gc_init(
    min_heap_size: usize,
    max_heap_size: usize,
    n_gcthreads: usize,
    calls: *const Julia_Upcalls,
    header_size: usize,
) {
    unsafe {
        UPCALLS = calls;
        set_julia_obj_header_size(header_size);
    };

    // Assert to make sure our ABI is correct
    assert_eq!(
        unsafe { ((*UPCALLS).get_abi_structs_checksum_c)() },
        crate::util::get_abi_structs_checksum_rust()
    );

    {
        let mut builder = BUILDER.lock().unwrap();

        // Set plan
        use mmtk::util::options::PlanSelector;
        let force_plan = if cfg!(feature = "nogc") {
            Some(PlanSelector::NoGC)
        } else if cfg!(feature = "marksweep") {
            Some(PlanSelector::MarkSweep)
        } else if cfg!(feature = "immix") {
            Some(PlanSelector::Immix)
        } else if cfg!(feature = "stickyimmix") {
            Some(PlanSelector::StickyImmix)
        } else {
            None
        };
        if let Some(plan) = force_plan {
            builder.options.plan.set(plan);
        }

        // Set heap size
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

        // Set using weak references
        let success = builder.options.no_reference_types.set(false);
        assert!(success, "Failed to set no_reference_types to false");

        // Set GC threads
        if n_gcthreads > 0 {
            let success = builder.options.threads.set(n_gcthreads);
            assert!(success, "Failed to set GC threads to {}", n_gcthreads);
        }
    }

    // Make sure that we haven't initialized MMTk (by accident) yet
    assert!(!crate::MMTK_INITIALIZED.load(Ordering::SeqCst));
    // Make sure we initialize MMTk here
    lazy_static::initialize(&SINGLETON);
}

#[no_mangle]
pub extern "C" fn mmtk_start_control_collector(
    tls: VMWorkerThread,
    gc_controller: *mut GCController<JuliaVM>,
) {
    let mut gc_controller = unsafe { Box::from_raw(gc_controller) };
    memory_manager::start_control_collector(&SINGLETON, tls, &mut gc_controller);
}

#[no_mangle]
pub extern "C" fn mmtk_bind_mutator(tls: VMMutatorThread, tid: usize) -> *mut Mutator<JuliaVM> {
    let mutator_box = memory_manager::bind_mutator(&SINGLETON, tls);

    let res = Box::into_raw(mutator_box);

    info!("Binding mutator {:?} to thread id = {}", res, tid);
    res
}

#[no_mangle]
pub extern "C" fn mmtk_post_bind_mutator(
    mutator: *mut Mutator<JuliaVM>,
    original_box_mutator: *mut Mutator<JuliaVM>,
) {
    // We have to store the original boxed mutator. Otherwise, we may have dangling pointers in mutator.
    MUTATORS.write().unwrap().insert(
        Address::from_mut_ptr(mutator),
        Address::from_mut_ptr(original_box_mutator),
    );
}

#[no_mangle]
pub extern "C" fn mmtk_destroy_mutator(mutator: *mut Mutator<JuliaVM>) {
    // destroy the mutator with MMTk.
    memory_manager::destroy_mutator(unsafe { &mut *mutator });

    let mut mutators = MUTATORS.write().unwrap();
    let key = Address::from_mut_ptr(mutator);

    // Clear the original boxed mutator
    let orig_mutator = mutators.get(&key).unwrap();
    let _ = unsafe { Box::from_raw(orig_mutator.to_mut_ptr::<Mutator<JuliaVM>>()) };

    // Remove from our hashmap
    mutators.remove(&key);
}

#[no_mangle]
pub extern "C" fn mmtk_alloc(
    mutator: *mut Mutator<JuliaVM>,
    size: usize,
    align: usize,
    offset: usize,
    semantics: AllocationSemantics,
) -> Address {
    memory_manager::alloc::<JuliaVM>(unsafe { &mut *mutator }, size, align, offset, semantics)
}

#[no_mangle]
pub extern "C" fn mmtk_alloc_large(
    mutator: *mut Mutator<JuliaVM>,
    size: usize,
    align: usize,
    offset: usize,
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
pub extern "C" fn mmtk_post_alloc(
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
pub extern "C" fn mmtk_will_never_move(object: ObjectReference) -> bool {
    !object.is_movable()
}

#[no_mangle]
pub extern "C" fn mmtk_start_worker(tls: VMWorkerThread, worker: *mut GCWorker<JuliaVM>) {
    let mut worker = unsafe { Box::from_raw(worker) };
    memory_manager::start_worker::<JuliaVM>(&SINGLETON, tls, &mut worker)
}

#[no_mangle]
pub extern "C" fn mmtk_initialize_collection(tls: VMThread) {
    memory_manager::initialize_collection(&SINGLETON, tls);
}

#[no_mangle]
pub extern "C" fn mmtk_enable_collection() {
    if AtomicBool::load(&DISABLED_GC, Ordering::SeqCst) {
        memory_manager::enable_collection(&SINGLETON);
        AtomicBool::store(&DISABLED_GC, false, Ordering::SeqCst);
    }
}

#[no_mangle]
pub extern "C" fn mmtk_disable_collection() {
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
pub extern "C" fn mmtk_used_bytes() -> usize {
    memory_manager::used_bytes(&SINGLETON)
}

#[no_mangle]
pub extern "C" fn mmtk_free_bytes() -> usize {
    memory_manager::free_bytes(&SINGLETON)
}

#[no_mangle]
pub extern "C" fn mmtk_total_bytes() -> usize {
    memory_manager::total_bytes(&SINGLETON)
}

#[no_mangle]
pub extern "C" fn mmtk_is_live_object(object: ObjectReference) -> bool {
    object.is_live()
}

#[no_mangle]
pub extern "C" fn mmtk_is_mapped_address(address: Address) -> bool {
    address.is_mapped()
}

#[no_mangle]
pub extern "C" fn mmtk_modify_check(object: ObjectReference) {
    memory_manager::modify_check(&SINGLETON, object)
}

#[no_mangle]
pub extern "C" fn mmtk_handle_user_collection_request(tls: VMMutatorThread, collection: u8) {
    AtomicBool::store(&USER_TRIGGERED_GC, true, Ordering::SeqCst);
    if AtomicBool::load(&DISABLED_GC, Ordering::SeqCst) {
        AtomicBool::store(&USER_TRIGGERED_GC, false, Ordering::SeqCst);
        return;
    }
    // See jl_gc_collection_t
    match collection {
        // auto
        0 => memory_manager::handle_user_collection_request::<JuliaVM>(&SINGLETON, tls),
        // full
        1 => SINGLETON
            .get_plan()
            .handle_user_collection_request(tls, true, true),
        // incremental
        2 => SINGLETON
            .get_plan()
            .handle_user_collection_request(tls, true, false),
        _ => unreachable!(),
    }
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
pub extern "C" fn mmtk_harness_begin(tls: VMMutatorThread) {
    memory_manager::harness_begin(&SINGLETON, tls)
}

#[no_mangle]
pub extern "C" fn mmtk_harness_end(_tls: OpaquePointer) {
    memory_manager::harness_end(&SINGLETON)
}

#[no_mangle]
pub extern "C" fn mmtk_register_finalizer(
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
pub extern "C" fn mmtk_run_finalizers_for_obj(obj: ObjectReference) {
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
pub extern "C" fn mmtk_process(name: *const c_char, value: *const c_char) -> bool {
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
pub extern "C" fn mmtk_starting_heap_address() -> Address {
    memory_manager::starting_heap_address()
}

#[no_mangle]
pub extern "C" fn mmtk_last_heap_address() -> Address {
    memory_manager::last_heap_address()
}

#[no_mangle]
pub extern "C" fn mmtk_counted_malloc(size: usize) -> Address {
    memory_manager::counted_malloc::<JuliaVM>(&SINGLETON, size)
}

#[no_mangle]
pub extern "C" fn mmtk_malloc(size: usize) -> Address {
    memory_manager::malloc(size)
}

#[no_mangle]
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
pub extern "C" fn mmtk_counted_calloc(num: usize, size: usize) -> Address {
    memory_manager::counted_calloc::<JuliaVM>(&SINGLETON, num, size)
}

#[no_mangle]
pub extern "C" fn mmtk_calloc(num: usize, size: usize) -> Address {
    memory_manager::calloc(num, size)
}

#[no_mangle]
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
pub extern "C" fn mmtk_free_with_size(addr: Address, old_size: usize) {
    memory_manager::free_with_size::<JuliaVM>(&SINGLETON, addr, old_size)
}

#[no_mangle]
pub extern "C" fn mmtk_free(addr: Address) {
    memory_manager::free(addr)
}

#[no_mangle]
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
pub extern "C" fn mmtk_runtime_panic() {
    panic!("Panicking at runtime!")
}

#[no_mangle]
pub extern "C" fn mmtk_unreachable() {
    unreachable!()
}

#[no_mangle]
#[allow(mutable_transmutes)]
pub extern "C" fn mmtk_set_vm_space(start: Address, size: usize) {
    let mmtk: &mmtk::MMTK<JuliaVM> = &SINGLETON;
    let mmtk_mut: &mut mmtk::MMTK<JuliaVM> = unsafe { std::mem::transmute(mmtk) };
    memory_manager::lazy_init_vm_space(mmtk_mut, start, size);

    #[cfg(feature = "stickyimmix")]
    set_side_log_bit_for_region(start, size);
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
#[allow(unused_variables)] // Args are only used for sticky immix.
pub extern "C" fn mmtk_immortal_region_post_alloc(start: Address, size: usize) {
    #[cfg(feature = "stickyimmix")]
    set_side_log_bit_for_region(start, size);
}

#[cfg(feature = "stickyimmix")]
fn set_side_log_bit_for_region(start: Address, size: usize) {
    debug!("Bulk set {} to {} ({} bytes)", start, start + size, size);
    use crate::mmtk::vm::ObjectModel;
    match <JuliaVM as mmtk::vm::VMBinding>::VMObjectModel::GLOBAL_LOG_BIT_SPEC.as_spec() {
        mmtk::util::metadata::MetadataSpec::OnSide(side) => side.bset_metadata(start, size),
        _ => unimplemented!(),
    }
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
pub extern "C" fn mmtk_object_reference_write_slow(
    mutator: &'static mut Mutator<JuliaVM>,
    src: ObjectReference,
    target: ObjectReference,
) {
    use mmtk::MutatorContext;
    mutator.barrier().object_reference_write_slow(
        src,
        crate::edges::JuliaVMEdge::Simple(mmtk::vm::edge_shape::SimpleEdge::from_address(
            Address::ZERO,
        )),
        target,
    );
}

/// Side log bit is the first side metadata spec starting.
#[no_mangle]
pub static MMTK_SIDE_LOG_BIT_BASE_ADDRESS: Address =
    mmtk::util::metadata::side_metadata::GLOBAL_SIDE_METADATA_VM_BASE_ADDRESS;

#[no_mangle]
pub static MMTK_NO_BARRIER: u8 = 0;
#[no_mangle]
pub static MMTK_OBJECT_BARRIER: u8 = 1;

#[no_mangle]
#[cfg(feature = "immix")]
pub static MMTK_NEEDS_WRITE_BARRIER: u8 = 0;

#[no_mangle]
#[cfg(feature = "stickyimmix")]
pub static MMTK_NEEDS_WRITE_BARRIER: u8 = 1;

#[no_mangle]
pub extern "C" fn mmtk_run_finalizers(at_exit: bool) {
    AtomicBool::store(&FINALIZERS_RUNNING, true, Ordering::SeqCst);

    if at_exit {
        let mut all_finalizable = memory_manager::get_all_finalizers(&SINGLETON);

        {
            // if the finalizer function triggers GC you don't want the objects to be GC-ed
            let mut fin_roots = FINALIZER_ROOTS.write().unwrap();

            for obj in all_finalizable.iter() {
                let inserted = fin_roots.insert(*obj);
                assert!(inserted);
            }
        }

        loop {
            let to_be_finalized = all_finalizable.pop();

            match to_be_finalized {
                Some(obj) => unsafe {
                    ((*UPCALLS).run_finalizer_function)(obj.0, obj.1, obj.2);
                    {
                        let mut fin_roots = FINALIZER_ROOTS.write().unwrap();
                        let removed = fin_roots.remove(&obj);
                        assert!(removed);
                    }
                },
                None => break,
            }
        }
    } else {
        loop {
            let to_be_finalized = memory_manager::get_finalized_object(&SINGLETON);

            match to_be_finalized {
                Some(obj) => {
                    {
                        // if the finalizer function triggers GC you don't want the objects to be GC-ed
                        let mut fin_roots = FINALIZER_ROOTS.write().unwrap();

                        let inserted = fin_roots.insert(obj);
                        assert!(inserted);
                    }
                    unsafe { ((*UPCALLS).run_finalizer_function)(obj.0, obj.1, obj.2) }
                    {
                        let mut fin_roots = FINALIZER_ROOTS.write().unwrap();
                        let removed = fin_roots.remove(&obj);
                        assert!(removed);
                    }
                }
                None => break,
            }
        }
    }

    AtomicBool::store(&FINALIZERS_RUNNING, false, Ordering::SeqCst);
}

#[no_mangle]
pub extern "C" fn mmtk_object_is_managed_by_mmtk(addr: usize) -> bool {
    crate::api::mmtk_is_mapped_address(unsafe { Address::from_usize(addr) })
}

#[no_mangle]
pub extern "C" fn mmtk_start_spawned_worker_thread(
    tls: VMWorkerThread,
    ctx: *mut GCWorker<JuliaVM>,
) {
    mmtk_start_worker(tls, ctx);
}

#[no_mangle]
pub extern "C" fn mmtk_start_spawned_controller_thread(
    tls: VMWorkerThread,
    ctx: *mut GCController<JuliaVM>,
) {
    mmtk_start_control_collector(tls, ctx);
}

#[no_mangle]
pub extern "C" fn mmtk_add_object_to_mmtk_roots(obj: ObjectReference) {
    // if object is not managed by mmtk it needs to be processed to look for pointers to managed objects (i.e. roots)
    ROOT_NODES.lock().unwrap().insert(obj);
}

use crate::JuliaVMEdge;
use mmtk::vm::EdgeVisitor;

// Pass this as 'process_edge' so we can reuse scan_julia_task_obj.
#[no_mangle]
#[allow(improper_ctypes_definitions)] // closure is a fat pointer, we propelry define its type in C header.
pub extern "C" fn mmtk_process_root_edges(
    _closure: &mut dyn EdgeVisitor<JuliaVMEdge>,
    addr: Address,
) {
    ROOT_EDGES.lock().unwrap().insert(addr);
}

#[inline(always)]
pub fn store_obj_size(obj: ObjectReference, size: usize) {
    let addr_size = obj.to_raw_address() - 16;
    unsafe {
        addr_size.store::<u64>(size as u64);
    }
}

#[no_mangle]
pub extern "C" fn mmtk_store_obj_size_c(obj: ObjectReference, size: usize) {
    let addr_size = obj.to_raw_address() - 16;
    unsafe {
        addr_size.store::<u64>(size as u64);
    }
}

#[no_mangle]
pub extern "C" fn mmtk_get_obj_size(obj: ObjectReference) -> usize {
    unsafe {
        let addr_size = obj.to_raw_address() - 2 * JULIA_HEADER_SIZE;
        addr_size.load::<u64>() as usize
    }
}
