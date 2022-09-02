
use crate::api::{start_control_collector, start_worker};
use mmtk::util::opaque_pointer::*;
use crate::JuliaVM;
use mmtk::scheduler::{GCWorker, GCController};
use crate::{ROOTS, JULIA_HEADER_SIZE};
use mmtk::util::ObjectReference;
use mmtk::util::Address;
use enum_map::Enum;


#[repr(i32)]
#[derive(Clone, Copy, Debug, Enum, PartialEq, Hash, Eq)]
pub enum RootLabel {
    MarkAndScan = 0,
    ScanOnly = 1,
    FinList = 2,
    ObjArray = 3,
    Array8 = 4,
    Obj8 = 5,
    Obj16 = 6,
    Obj32 = 7,
    Stack = 8,
    ExcStack = 9,
    ModuleBinding = 10,
}

impl RootLabel {
    pub fn from_u32(value: u32) -> RootLabel {
        match value {
            0 => RootLabel::MarkAndScan,
            1 => RootLabel::ScanOnly,
            2  => RootLabel::FinList,
            3 => RootLabel::ObjArray,
            4 => RootLabel::Array8,
            5 => RootLabel::Obj8,
            6 => RootLabel::Obj16,
            7 => RootLabel::Obj32,
            8 => RootLabel::Stack,
            9 => RootLabel::ExcStack,
            10 => RootLabel::ModuleBinding,
            _ => panic!("Unknown value: {}", value),
        }
    }
}

#[no_mangle]
pub extern "C" fn start_spawned_worker_thread(
    tls: VMWorkerThread,
    ctx: *mut GCWorker<JuliaVM>
) {
    start_worker(tls, ctx);
}

#[no_mangle]
pub extern "C" fn start_spawned_controller_thread(tls: VMWorkerThread, ctx: *mut GCController<JuliaVM>) {
    start_control_collector(tls, ctx);
}

#[no_mangle]
pub extern "C" fn add_object_to_mmtk_roots(addr: Address) {
    // if object is not managed by mmtk it needs to be processed to look for pointers to managed objects (i.e. roots)
    ROOTS.lock().unwrap().insert(addr);
}

#[inline(always)]
pub fn store_obj_size(obj: ObjectReference, size : usize) {
    let addr_size = obj.to_address() - 16;
    unsafe {
        addr_size.store::<u64>(size as u64);
    }
}

#[no_mangle]
pub extern "C" fn store_obj_size_c(obj: ObjectReference, size : usize) {
    let addr_size = obj.to_address() - 16;
    unsafe {
        addr_size.store::<u64>(size as u64);
    }
}

#[no_mangle]
pub extern "C" fn get_obj_size(obj: ObjectReference) -> usize {
    unsafe {
        let addr_size = obj.to_address() - 2*JULIA_HEADER_SIZE;
        addr_size.load::<u64>() as usize
    }
}