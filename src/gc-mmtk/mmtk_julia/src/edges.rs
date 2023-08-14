use atomic::Atomic;
use mmtk::{
    util::{Address, ObjectReference},
    vm::{
        edge_shape::{Edge, SimpleEdge},
        RootsWorkFactory,
    },
};

/// If a VM supports multiple kinds of edges, we can use tagged union to represent all of them.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum JuliaVMEdge {
    Simple(SimpleEdge),
    Offset(OffsetEdge),
}

unsafe impl Send for JuliaVMEdge {}

impl Edge for JuliaVMEdge {
    fn load(&self) -> ObjectReference {
        match self {
            JuliaVMEdge::Simple(e) => e.load(),
            JuliaVMEdge::Offset(e) => e.load(),
        }
    }

    fn store(&self, object: ObjectReference) {
        match self {
            JuliaVMEdge::Simple(e) => e.store(object),
            JuliaVMEdge::Offset(e) => e.store(object),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct OffsetEdge {
    slot_addr: *mut Atomic<Address>,
    offset: usize,
}

unsafe impl Send for OffsetEdge {}

impl OffsetEdge {
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

impl Edge for OffsetEdge {
    fn load(&self) -> ObjectReference {
        let middle = unsafe { (*self.slot_addr).load(atomic::Ordering::Relaxed) };
        let begin = middle - self.offset;
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

impl mmtk::vm::edge_shape::MemorySlice for JuliaMemorySlice {
    type Edge = JuliaVMEdge;
    type EdgeIterator = JuliaMemorySliceEdgeIterator;

    fn iter_edges(&self) -> Self::EdgeIterator {
        JuliaMemorySliceEdgeIterator {
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
                // non overlaping
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

pub struct JuliaMemorySliceEdgeIterator {
    cursor: Address,
    limit: Address,
}

impl Iterator for JuliaMemorySliceEdgeIterator {
    type Item = JuliaVMEdge;

    fn next(&mut self) -> Option<JuliaVMEdge> {
        if self.cursor >= self.limit {
            None
        } else {
            let edge = self.cursor;
            self.cursor = self.cursor.shift::<ObjectReference>(1);
            Some(JuliaVMEdge::Simple(SimpleEdge::from_address(edge)))
        }
    }
}

const ROOT_WORK_PACKET_SIZE: usize = 4096;

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
    pub report_edges_func: extern "C" fn(
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
    pub factory_ptr: *mut libc::c_void,
}

impl RootsWorkClosure {
    extern "C" fn report_simple_edges<F: RootsWorkFactory<JuliaVMEdge>>(
        buf: *mut Address,
        size: usize,
        cap: usize,
        factory_ptr: *mut libc::c_void,
        renew: bool,
    ) -> RootsWorkBuffer<Address> {
        if !buf.is_null() {
            let buf = unsafe { Vec::<Address>::from_raw_parts(buf, size, cap) }
                .into_iter()
                .map(|addr| JuliaVMEdge::Simple(SimpleEdge::from_address(addr)))
                .collect();
            let factory: &mut F = unsafe { &mut *(factory_ptr as *mut F) };
            factory.create_process_edge_roots_work(buf);
        }

        if renew {
            RootsWorkBuffer::new()
        } else {
            RootsWorkBuffer::empty()
        }
    }

    extern "C" fn report_nodes<F: RootsWorkFactory<JuliaVMEdge>>(
        buf: *mut ObjectReference,
        size: usize,
        cap: usize,
        factory_ptr: *mut libc::c_void,
        renew: bool,
    ) -> RootsWorkBuffer<ObjectReference> {
        if !buf.is_null() {
            let buf = unsafe { Vec::<ObjectReference>::from_raw_parts(buf, size, cap) };
            let factory: &mut F = unsafe { &mut *(factory_ptr as *mut F) };
            factory.create_process_node_roots_work(buf);
        }

        if renew {
            RootsWorkBuffer::new()
        } else {
            RootsWorkBuffer::empty()
        }
    }

    pub fn from_roots_work_factory<F: RootsWorkFactory<JuliaVMEdge>>(factory: &mut F) -> Self {
        RootsWorkClosure {
            report_edges_func: Self::report_simple_edges::<F>,
            report_nodes_func: Self::report_nodes::<F>,
            factory_ptr: factory as *mut F as *mut libc::c_void,
        }
    }
}
