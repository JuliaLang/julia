use atomic::Atomic;
use mmtk::{
    util::{Address, ObjectReference},
    vm::{
        slot::{SimpleSlot, Slot},
        RootsWorkFactory,
    },
};

/// If a VM supports multiple kinds of slots, we can use tagged union to represent all of them.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum JuliaVMSlot {
    Simple(SimpleSlot),
    Offset(OffsetSlot),
    /// A root reported as the object it points to, as the VM cannot (or does not) give us the location
    /// of the root. MMTk never stores to it, as the build does not move objects.
    #[cfg(feature = "non_moving")]
    RootNode(ObjectReference),
}

unsafe impl Send for JuliaVMSlot {}

impl Slot for JuliaVMSlot {
    fn load(&self) -> Option<ObjectReference> {
        match self {
            JuliaVMSlot::Simple(e) => e.load(),
            JuliaVMSlot::Offset(e) => e.load(),
            #[cfg(feature = "non_moving")]
            JuliaVMSlot::RootNode(o) => Some(*o),
        }
    }

    fn store(&self, object: ObjectReference) {
        match self {
            JuliaVMSlot::Simple(e) => e.store(object),
            JuliaVMSlot::Offset(e) => e.store(object),
            #[cfg(feature = "non_moving")]
            JuliaVMSlot::RootNode(o) => {
                panic!("Root node {} moved to {} in a non-moving build", o, object)
            }
        }
    }
}

impl std::fmt::Debug for JuliaVMSlot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Simple(e) => write!(f, "{}", e.as_address()),
            Self::Offset(e) => write!(f, "{}+{}", e.slot_address(), e.offset),
            #[cfg(feature = "non_moving")]
            Self::RootNode(o) => write!(f, "root node {}", o),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct OffsetSlot {
    slot_addr: *mut Atomic<Address>,
    offset: usize,
}

unsafe impl Send for OffsetSlot {}
unsafe impl Sync for OffsetSlot {}

impl OffsetSlot {
    pub fn new_no_offset(address: Address) -> Self {
        Self {
            slot_addr: address.to_mut_ptr(),
            offset: 0,
        }
    }

    pub fn new_with_offset(address: Address, offset: usize) -> Self {
        Self {
            slot_addr: address.to_mut_ptr(),
            offset,
        }
    }

    pub fn slot_address(&self) -> Address {
        Address::from_mut_ptr(self.slot_addr)
    }

    pub fn offset(&self) -> usize {
        self.offset
    }
}

impl Slot for OffsetSlot {
    fn load(&self) -> Option<ObjectReference> {
        let middle = unsafe { (*self.slot_addr).load(atomic::Ordering::Relaxed) };
        let begin = middle - self.offset;
        debug_assert!(!begin.is_zero());
        ObjectReference::from_raw_address(begin)
    }

    fn store(&self, object: ObjectReference) {
        let begin = object.to_raw_address();
        let middle = begin + self.offset;
        unsafe { (*self.slot_addr).store(middle, atomic::Ordering::Relaxed) }
    }
}

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
pub struct JuliaMemorySlice {
    pub owner: ObjectReference,
    pub start: Address,
    pub count: usize,
}

impl mmtk::vm::slot::MemorySlice for JuliaMemorySlice {
    type SlotType = JuliaVMSlot;
    type SlotIterator = JuliaMemorySliceSlotIterator;

    fn iter_slots(&self) -> Self::SlotIterator {
        JuliaMemorySliceSlotIterator {
            cursor: self.start,
            limit: self.start.shift::<Address>(self.count as isize),
        }
    }

    fn object(&self) -> Option<ObjectReference> {
        Some(self.owner)
    }

    fn start(&self) -> Address {
        self.start
    }

    fn bytes(&self) -> usize {
        self.count << mmtk::util::constants::LOG_BYTES_IN_ADDRESS
    }

    fn copy(src: &Self, tgt: &Self) {
        use std::sync::atomic::*;
        // Raw memory copy -- we should be consistent with jl_array_ptr_copy in array.c
        unsafe {
            let words = tgt.bytes() >> mmtk::util::constants::LOG_BYTES_IN_ADDRESS;
            // let src = src.start().to_ptr::<usize>();
            // let tgt = tgt.start().to_mut_ptr::<usize>();
            // std::ptr::copy(src, tgt, words)

            let src_addr = src.start();
            let tgt_addr = tgt.start();

            let n: isize = words as isize;

            if tgt_addr < src_addr || tgt_addr > src_addr + tgt.bytes() {
                // non overlapping
                for i in 0..n {
                    let val: usize = src_addr
                        .shift::<usize>(i)
                        .atomic_load::<AtomicUsize>(Ordering::Relaxed);
                    tgt_addr
                        .shift::<usize>(i)
                        .atomic_store::<AtomicUsize>(val, Ordering::Release);
                }
            } else {
                for i in 0..n {
                    let val = src_addr
                        .shift::<usize>(n - i - 1)
                        .atomic_load::<AtomicUsize>(Ordering::Relaxed);
                    tgt_addr
                        .shift::<usize>(n - i - 1)
                        .atomic_store::<AtomicUsize>(val, Ordering::Release);
                }
            }
        }
    }
}

pub struct JuliaMemorySliceSlotIterator {
    cursor: Address,
    limit: Address,
}

impl Iterator for JuliaMemorySliceSlotIterator {
    type Item = JuliaVMSlot;

    fn next(&mut self) -> Option<JuliaVMSlot> {
        if self.cursor >= self.limit {
            None
        } else {
            let slot = self.cursor;
            self.cursor = self.cursor.shift::<ObjectReference>(1);
            Some(JuliaVMSlot::Simple(SimpleSlot::from_address(slot)))
        }
    }
}

const ROOT_WORK_PACKET_SIZE: usize = 4096;

/// Report roots that cannot be updated. The objects in `nodes` must not move.
pub(crate) fn report_pinning_roots<F: RootsWorkFactory<JuliaVMSlot>>(
    factory: &mut F,
    nodes: Vec<ObjectReference>,
) {
    #[cfg(feature = "moving")]
    factory.create_process_pinning_roots_work(nodes);
    // Nothing moves, so the roots can go through the normal slot path.
    #[cfg(feature = "non_moving")]
    factory.create_process_roots_work(nodes.into_iter().map(JuliaVMSlot::RootNode).collect());
}

/// Report roots that cannot be updated. Neither the objects in `nodes` nor any object reachable
/// from them may move.
pub(crate) fn report_tpinning_roots<F: RootsWorkFactory<JuliaVMSlot>>(
    factory: &mut F,
    nodes: Vec<ObjectReference>,
) {
    #[cfg(feature = "moving")]
    factory.create_process_tpinning_roots_work(nodes);
    // Nothing moves, so the roots can go through the normal slot path.
    #[cfg(feature = "non_moving")]
    factory.create_process_roots_work(nodes.into_iter().map(JuliaVMSlot::RootNode).collect());
}

/// A tpinning root slot as held before it is reported. In a `moving` build, it is the object
/// in the slot, as such roots are reported as objects. Otherwise, it is the slot itself.
#[cfg(feature = "moving")]
pub(crate) type TPinningRootSlot = ObjectReference;
#[cfg(feature = "non_moving")]
pub(crate) type TPinningRootSlot = JuliaVMSlot;

/// Report root slots that cannot be updated. Neither the objects in `slots` nor any object
/// reachable from them may move.
pub(crate) fn report_tpinning_root_slots<F: RootsWorkFactory<JuliaVMSlot>>(
    factory: &mut F,
    slots: Vec<TPinningRootSlot>,
) {
    #[cfg(feature = "moving")]
    factory.create_process_tpinning_roots_work(slots);
    // Nothing moves, so the slots can go through the normal slot path.
    #[cfg(feature = "non_moving")]
    factory.create_process_roots_work(slots);
}

#[repr(C)]
pub struct RootsWorkBuffer<T: Copy> {
    pub ptr: *mut T,
    pub capacity: usize,
}

impl<T: Copy> RootsWorkBuffer<T> {
    fn empty() -> Self {
        Self {
            ptr: std::ptr::null_mut(),
            capacity: 0,
        }
    }
    fn new() -> Self {
        let (buf, _, capacity) = {
            let new_vec = Vec::with_capacity(ROOT_WORK_PACKET_SIZE);
            let mut me = std::mem::ManuallyDrop::new(new_vec);
            (me.as_mut_ptr(), me.len(), me.capacity())
        };
        Self { ptr: buf, capacity }
    }
}

#[repr(C)]
pub struct RootsWorkClosure {
    pub report_slots_func: extern "C" fn(
        buf: *mut Address,
        size: usize,
        cap: usize,
        factory_ptr: *mut libc::c_void,
        renew: bool,
    ) -> RootsWorkBuffer<Address>,
    pub report_nodes_func: extern "C" fn(
        buf: *mut ObjectReference,
        size: usize,
        cap: usize,
        factory_ptr: *mut libc::c_void,
        renew: bool,
    ) -> RootsWorkBuffer<ObjectReference>,
    pub report_tpinned_nodes_func: extern "C" fn(
        buf: *mut ObjectReference,
        size: usize,
        cap: usize,
        factory_ptr: *mut libc::c_void,
        renew: bool,
    ) -> RootsWorkBuffer<ObjectReference>,
    pub factory_ptr: *mut libc::c_void,
}

impl RootsWorkClosure {
    extern "C" fn report_simple_slots<F: RootsWorkFactory<JuliaVMSlot>>(
        buf: *mut Address,
        size: usize,
        cap: usize,
        factory_ptr: *mut libc::c_void,
        renew: bool,
    ) -> RootsWorkBuffer<Address> {
        if !buf.is_null() {
            let buf = unsafe { Vec::<Address>::from_raw_parts(buf, size, cap) }
                .into_iter()
                .map(|addr| JuliaVMSlot::Simple(SimpleSlot::from_address(addr)))
                .collect();
            let factory: &mut F = unsafe { &mut *(factory_ptr as *mut F) };
            factory.create_process_roots_work(buf);
        }

        if renew {
            RootsWorkBuffer::new()
        } else {
            RootsWorkBuffer::empty()
        }
    }

    extern "C" fn report_nodes<F: RootsWorkFactory<JuliaVMSlot>>(
        buf: *mut ObjectReference,
        size: usize,
        cap: usize,
        factory_ptr: *mut libc::c_void,
        renew: bool,
    ) -> RootsWorkBuffer<ObjectReference> {
        if !buf.is_null() {
            let buf = unsafe { Vec::<ObjectReference>::from_raw_parts(buf, size, cap) };
            let factory: &mut F = unsafe { &mut *(factory_ptr as *mut F) };
            report_pinning_roots(factory, buf);
        }

        if renew {
            RootsWorkBuffer::new()
        } else {
            RootsWorkBuffer::empty()
        }
    }

    extern "C" fn report_tpinned_nodes<F: RootsWorkFactory<JuliaVMSlot>>(
        buf: *mut ObjectReference,
        size: usize,
        cap: usize,
        factory_ptr: *mut libc::c_void,
        renew: bool,
    ) -> RootsWorkBuffer<ObjectReference> {
        if !buf.is_null() {
            let buf = unsafe { Vec::<ObjectReference>::from_raw_parts(buf, size, cap) };
            let factory: &mut F = unsafe { &mut *(factory_ptr as *mut F) };
            report_tpinning_roots(factory, buf);
        }

        if renew {
            RootsWorkBuffer::new()
        } else {
            RootsWorkBuffer::empty()
        }
    }

    pub fn from_roots_work_factory<F: RootsWorkFactory<JuliaVMSlot>>(factory: &mut F) -> Self {
        RootsWorkClosure {
            report_slots_func: Self::report_simple_slots::<F>,
            report_nodes_func: Self::report_nodes::<F>,
            report_tpinned_nodes_func: Self::report_tpinned_nodes::<F>,
            factory_ptr: factory as *mut F as *mut libc::c_void,
        }
    }
}
