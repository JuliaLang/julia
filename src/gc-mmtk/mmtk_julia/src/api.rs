// All functions here are extern function. There is no point for marking them as unsafe.
#![allow(clippy::not_unsafe_ptr_arg_deref)]

use crate::JuliaVM;
use crate::Julia_Upcalls;
use crate::JULIA_HEADER_SIZE;
use crate::SINGLETON;
use crate::UPCALLS;
use crate::{BUILDER, DISABLED_GC, MUTATORS, USER_TRIGGERED_GC};

use libc::c_char;
use log::*;
use mmtk::memory_manager;
use mmtk::scheduler::GCWorker;
use mmtk::util::api_util::NullableObjectReference;
use mmtk::util::opaque_pointer::*;
use mmtk::util::{Address, ObjectReference, OpaquePointer};
use mmtk::AllocationSemantics;
use mmtk::Mutator;
use std::ffi::CStr;
use std::sync::atomic::AtomicIsize;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

#[no_mangle]
pub extern "C" fn mmtk_gc_init(
    min_heap_size: usize,
    max_heap_size: usize,
    n_gcthreads: usize,
    calls: *const Julia_Upcalls,
    header_size: usize,
    buffer_tag: usize,
) {
    unsafe {
        UPCALLS = calls;
        crate::JULIA_HEADER_SIZE = header_size;
        crate::JULIA_BUFF_TAG = buffer_tag;
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

    // Assert to make sure our fastpath allocation is correct.
    {
        // If the assertion failed, check the allocation fastpath in Julia
        // - runtime fastpath: mmtk_immix_alloc_fast and mmtk_immortal_alloc_fast in julia.h
        // - compiler inserted fastpath: llvm-final-gc-lowering.cpp
        use mmtk::util::alloc::AllocatorSelector;
        let default_allocator = memory_manager::get_allocator_mapping::<JuliaVM>(
            &SINGLETON,
            AllocationSemantics::Default,
        );
        assert_eq!(default_allocator, AllocatorSelector::Immix(0));
        let immortal_allocator = memory_manager::get_allocator_mapping::<JuliaVM>(
            &SINGLETON,
            AllocationSemantics::Immortal,
        );
        assert_eq!(immortal_allocator, AllocatorSelector::BumpPointer(0));
    }

    // Assert to make sure alignment used in C is correct
    {
        // If the assertion failed, check MMTK_MIN_ALIGNMENT in julia.h
        assert_eq!(<JuliaVM as mmtk::vm::VMBinding>::MIN_ALIGNMENT, 4);
    }
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
    debug_assert!(
        mmtk::util::conversions::raw_is_aligned(
            size,
            <JuliaVM as mmtk::vm::VMBinding>::MIN_ALIGNMENT
        ),
        "Alloc size {} is not aligned to min alignment",
        size
    );
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
    memory_manager::post_alloc::<JuliaVM>(unsafe { &mut *mutator }, refer, bytes, semantics)
}

#[no_mangle]
pub extern "C" fn mmtk_will_never_move(object: ObjectReference) -> bool {
    !object.is_movable::<JuliaVM>()
}

#[no_mangle]
pub extern "C" fn mmtk_start_worker(tls: VMWorkerThread, worker: *mut GCWorker<JuliaVM>) {
    let worker = unsafe { Box::from_raw(worker) };
    memory_manager::start_worker::<JuliaVM>(&SINGLETON, tls, worker)
}

#[no_mangle]
pub extern "C" fn mmtk_initialize_collection(tls: VMThread) {
    memory_manager::initialize_collection(&SINGLETON, tls);
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
    object.is_live::<JuliaVM>()
}

#[no_mangle]
pub extern "C" fn mmtk_is_mapped_address(address: Address) -> bool {
    address.is_mapped()
}

#[no_mangle]
pub extern "C" fn mmtk_handle_user_collection_request(tls: VMMutatorThread, collection: u8) {
    AtomicIsize::fetch_add(&USER_TRIGGERED_GC, 1, Ordering::SeqCst);
    if AtomicBool::load(&DISABLED_GC, Ordering::SeqCst) {
        AtomicIsize::fetch_add(&USER_TRIGGERED_GC, -1, Ordering::SeqCst);
        return;
    }
    // See jl_gc_collection_t
    match collection {
        // auto
        0 => memory_manager::handle_user_collection_request::<JuliaVM>(&SINGLETON, tls),
        // full
        1 => SINGLETON.handle_user_collection_request(tls, true, true),
        // incremental
        2 => SINGLETON.handle_user_collection_request(tls, true, false),
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

// Accessed from C to count the bytes we allocated with jl_gc_counted_malloc etc.
#[no_mangle]
pub static JULIA_MALLOC_BYTES: AtomicUsize = AtomicUsize::new(0);

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
    memory_manager::set_vm_space(mmtk_mut, start, size);

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
    use crate::slots::JuliaMemorySlice;
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
    target: NullableObjectReference,
) {
    let mutator = unsafe { &mut *mutator };
    memory_manager::object_reference_write_post(
        mutator,
        src,
        crate::slots::JuliaVMSlot::Simple(mmtk::vm::slot::SimpleSlot::from_address(Address::ZERO)),
        target.into(),
    )
}

#[no_mangle]
pub extern "C" fn mmtk_object_reference_write_slow(
    mutator: &'static mut Mutator<JuliaVM>,
    src: ObjectReference,
    target: NullableObjectReference,
) {
    use mmtk::MutatorContext;
    mutator.barrier().object_reference_write_slow(
        src,
        crate::slots::JuliaVMSlot::Simple(mmtk::vm::slot::SimpleSlot::from_address(Address::ZERO)),
        target.into(),
    );
}

/// Side log bit is the first side metadata spec starting.
#[no_mangle]
pub static MMTK_SIDE_LOG_BIT_BASE_ADDRESS: Address =
    mmtk::util::metadata::side_metadata::GLOBAL_SIDE_METADATA_VM_BASE_ADDRESS;

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

#[cfg(all(feature = "object_pinning", not(feature = "non_moving")))]
#[no_mangle]
pub extern "C" fn mmtk_pin_object(object: ObjectReference) -> bool {
    // We may in the future replace this with a check for the immix space (bound check), which should be much cheaper.
    if mmtk_object_is_managed_by_mmtk(object.to_raw_address().as_usize()) {
        memory_manager::pin_object::<JuliaVM>(object)
    } else {
        debug!("Object is not managed by mmtk - (un)pinning it via this function isn't supported.");
        false
    }
}

#[cfg(all(feature = "object_pinning", not(feature = "non_moving")))]
#[no_mangle]
pub extern "C" fn mmtk_unpin_object(object: ObjectReference) -> bool {
    if mmtk_object_is_managed_by_mmtk(object.to_raw_address().as_usize()) {
        memory_manager::unpin_object::<JuliaVM>(object)
    } else {
        debug!("Object is not managed by mmtk - (un)pinning it via this function isn't supported.");
        false
    }
}

#[cfg(all(feature = "object_pinning", not(feature = "non_moving")))]
#[no_mangle]
pub extern "C" fn mmtk_is_pinned(object: ObjectReference) -> bool {
    if mmtk_object_is_managed_by_mmtk(object.to_raw_address().as_usize()) {
        memory_manager::is_pinned::<JuliaVM>(object)
    } else {
        debug!("Object is not managed by mmtk - checking via this function isn't supported.");
        false
    }
}

// If the `non-moving` feature is selected, pinning/unpinning is a noop and simply returns false
#[cfg(all(feature = "object_pinning", feature = "non_moving"))]
#[no_mangle]
pub extern "C" fn mmtk_pin_object(_object: ObjectReference) -> bool {
    false
}

#[cfg(all(feature = "object_pinning", feature = "non_moving"))]
#[no_mangle]
pub extern "C" fn mmtk_unpin_object(_object: ObjectReference) -> bool {
    false
}

#[cfg(all(feature = "object_pinning", feature = "non_moving"))]
#[no_mangle]
pub extern "C" fn mmtk_is_pinned(_object: ObjectReference) -> bool {
    false
}
