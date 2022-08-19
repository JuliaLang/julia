use mmtk::util::{Address, ObjectReference};
use mmtk::vm::ObjectModel;
use std::sync::atomic::Ordering;
use mmtk::util::copy::*;
use crate::{JuliaVM, JULIA_HEADER_SIZE, UPCALLS, init_boot_image_metadata_info};
use mmtk::util::constants::BYTES_IN_PAGE;
use mmtk::util::metadata::side_metadata::{
    SideMetadataSpec, SideMetadataOffset, SideMetadataContext
};
use mmtk::util::metadata::header_metadata::HeaderMetadataSpec;
use mmtk::vm::*;

pub struct VMObjectModel {}

/// Global logging bit metadata spec
/// 1 bit per object
pub(crate) const LOGGING_SIDE_METADATA_SPEC: VMGlobalLogBitSpec =
    VMGlobalLogBitSpec::side_first();

pub(crate) const MARKING_METADATA_SPEC: VMLocalMarkBitSpec =
    VMLocalMarkBitSpec::side_after(LOS_METADATA_SPEC.as_spec());

// pub(crate) const LOCAL_FORWARDING_POINTER_METADATA_SPEC: VMLocalForwardingPointerSpec =
//     VMLocalForwardingPointerSpec::side_after(MARKING_METADATA_SPEC.as_spec());

// pub(crate) const LOCAL_FORWARDING_METADATA_BITS_SPEC: VMLocalForwardingBitsSpec =
//     VMLocalForwardingBitsSpec::side_after(LOCAL_FORWARDING_POINTER_METADATA_SPEC.as_spec());


pub(crate) const BI_MARKING_METADATA_SPEC: SideMetadataSpec =
SideMetadataSpec {
    name : "BI_MARK",
    is_global: false,
    offset: SideMetadataOffset::layout_after(MARKING_METADATA_SPEC.as_spec().extract_side_spec()),
    log_num_of_bits: 0,
    log_bytes_in_region: 3
};

lazy_static! {
    pub static ref BI_METADATA_CONTEXT: SideMetadataContext =
    SideMetadataContext {
        global : vec![],
        local : vec![BI_MARKING_METADATA_SPEC],
    };
}

/// PolicySpecific mark-and-nursery bits metadata spec
/// 2-bits per object
pub(crate) const LOS_METADATA_SPEC: VMLocalLOSMarkNurserySpec =
    VMLocalLOSMarkNurserySpec::side_first();


impl ObjectModel<JuliaVM> for VMObjectModel {
    const GLOBAL_LOG_BIT_SPEC: VMGlobalLogBitSpec = LOGGING_SIDE_METADATA_SPEC;
    const LOCAL_FORWARDING_POINTER_SPEC: VMLocalForwardingPointerSpec = VMLocalForwardingPointerSpec::in_header(0);
    const LOCAL_FORWARDING_BITS_SPEC: VMLocalForwardingBitsSpec = VMLocalForwardingBitsSpec::in_header(0);
    const LOCAL_MARK_BIT_SPEC: VMLocalMarkBitSpec = MARKING_METADATA_SPEC;
    const LOCAL_LOS_MARK_NURSERY_SPEC: VMLocalLOSMarkNurserySpec = LOS_METADATA_SPEC;
    
    fn load_metadata(
        _metadata_spec: &HeaderMetadataSpec,
        _object: ObjectReference,
        _mask: Option<usize>,
        _atomic_ordering: Option<Ordering>,
    ) -> usize {
        unimplemented!()
    }

    fn store_metadata(
        _metadata_spec: &HeaderMetadataSpec,
        _object: ObjectReference,
        _val: usize,
        _mask: Option<usize>,
        _atomic_ordering: Option<Ordering>,
    ) {
        unimplemented!()
    }

    fn compare_exchange_metadata(
        _metadata_spec: &HeaderMetadataSpec,
        _object: ObjectReference,
        _old_val: usize,
        _new_val: usize,
        _mask: Option<usize>,
        _success_order: Ordering,
        _failure_order: Ordering,
    ) -> bool {
        unimplemented!()
    }

    fn fetch_add_metadata(
        _metadata_spec: &HeaderMetadataSpec,
        _object: ObjectReference,
        _val: usize,
        _order: Ordering,
    ) -> usize {
        unimplemented!()
    }

    fn fetch_sub_metadata(
        _metadata_spec: &HeaderMetadataSpec,
        _object: ObjectReference,
        _val: usize,
        _order: Ordering,
    ) -> usize {
        unimplemented!()
    }

    fn copy(
        _from: ObjectReference,
        _semantics: CopySemantics,
        _copy_context: &mut GCWorkerCopyContext<JuliaVM>,
    ) -> ObjectReference {
        unimplemented!()
    }

    fn copy_to(_from: ObjectReference, _to: ObjectReference, _region: Address) -> Address {
        unimplemented!()
    }

    fn get_current_size(object: ObjectReference) -> usize {
        let size = if is_object_in_los(&object) {
            unsafe {
                ((*UPCALLS).get_lo_size)(object)
            }
        } else {
            unsafe {
                let addr_size = object.to_address() - 2*JULIA_HEADER_SIZE;
                addr_size.load::<u64>() as usize
            }            
        };

        size
    }

    fn get_size_when_copied(_object: ObjectReference) -> usize {
        unimplemented!()
    }

    fn get_align_when_copied(_object: ObjectReference) -> usize {
        unimplemented!()
    }

    fn get_align_offset_when_copied(_object: ObjectReference) -> isize {
        unimplemented!()
    }

    fn get_reference_when_copied_to(_from: ObjectReference, _to: Address) -> ObjectReference {
        unimplemented!()
    }

    fn get_type_descriptor(_reference: ObjectReference) -> &'static [i8] {
        unimplemented!()
    }

    fn object_start_ref(object: ObjectReference) -> Address {
        let res = if is_object_in_los(&object) {
            object.to_address() - 48
        } else {
            unsafe {
                object.to_address() - 2*JULIA_HEADER_SIZE
            }
        };
        res
    }

    fn ref_to_address(object: ObjectReference) -> Address {
        object.to_address()
    }

    fn dump_object(_object: ObjectReference) {
        unimplemented!()
    }
}

pub fn is_object_in_los(object: &ObjectReference) -> bool {
    (*object).to_address().as_usize() > 0x60000000000
}


#[no_mangle]
pub extern "C" fn map_boot_image_metadata(start: Address, end: Address) {
    let start_address_aligned_down = start.align_down(BYTES_IN_PAGE);
    let end_address_aligned_up = end.align_up(BYTES_IN_PAGE);
    unsafe {
        init_boot_image_metadata_info(start_address_aligned_down.as_usize(), end_address_aligned_up.as_usize());
    }
    let res = BI_METADATA_CONTEXT.try_map_metadata_space(start_address_aligned_down, end_address_aligned_up.as_usize() - start_address_aligned_down.as_usize());

    match res {
        Ok(_) => (),
        Err(e) => panic!("Mapping failed with error {}", e)
    }
}