use enum_map::Enum;
use mmtk::util::ObjectReference;

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
            2 => RootLabel::FinList,
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

const PRINT_STRUCT_SIZE: bool = false;

macro_rules! print_sizeof {
    ($t: ty) => {{
        let sz = std::mem::size_of::<$t>();
        if PRINT_STRUCT_SIZE {
            println!("Rust {} = {} bytes", stringify!($t), sz);
        }
        sz
    }};
}

pub(crate) fn get_abi_structs_checksum_rust() -> usize {
    use crate::julia_types::*;
    print_sizeof!(mmtk::Mutator<crate::JuliaVM>)
        ^ print_sizeof!(mmtk__jl_taggedvalue_bits)
        ^ print_sizeof!(mmtk_jl_taggedvalue_t)
        ^ print_sizeof!(mmtk_jl_array_flags_t)
        ^ print_sizeof!(mmtk_jl_datatype_layout_t)
        ^ print_sizeof!(mmtk_jl_typename_t)
        ^ print_sizeof!(mmtk_jl_svec_t)
        ^ print_sizeof!(mmtk_jl_datatype_t)
        ^ print_sizeof!(mmtk_jl_array_t)
        ^ print_sizeof!(mmtk_jl_sym_t)
        ^ print_sizeof!(mmtk_jl_binding_t)
        ^ print_sizeof!(mmtk_htable_t)
        ^ print_sizeof!(mmtk_arraylist_t)
        ^ print_sizeof!(mmtk_jl_uuid_t)
        ^ print_sizeof!(mmtk_jl_mutex_t)
        ^ print_sizeof!(mmtk_jl_module_t)
        ^ print_sizeof!(mmtk_jl_excstack_t)
        ^ print_sizeof!(mmtk_jl_bt_element_t)
        ^ print_sizeof!(mmtk_jl_stack_context_t)
        ^ print_sizeof!(mmtk_jl_ucontext_t)
        ^ print_sizeof!(mmtk__jl_gcframe_t)
        ^ print_sizeof!(mmtk_jl_task_t)
        ^ print_sizeof!(mmtk_jl_weakref_t)
        ^ print_sizeof!(mmtk_jl_tls_states_t)
        ^ print_sizeof!(mmtk_jl_thread_heap_t)
        ^ print_sizeof!(mmtk_jl_thread_gc_num_t)
}

// The functions below allow accessing the values of bitfields without performing a for loop
use crate::julia_types::{
    __BindgenBitfieldUnit, mmtk__jl_task_t, mmtk_jl_array_flags_t, mmtk_jl_datatype_layout_t,
};

impl mmtk_jl_datatype_layout_t {
    #[inline]
    pub fn fielddesc_type_custom(&self) -> u16 {
        let fielddesc_type_raw: u16 = unsafe {
            ::std::mem::transmute::<__BindgenBitfieldUnit<[u8; 2usize]>, u16>(self._bitfield_1)
        };
        fielddesc_type_raw >> 1 & 0b11
    }
}

impl mmtk_jl_array_flags_t {
    #[inline]
    pub fn how_custom(&self) -> u16 {
        let how_raw: u16 = unsafe {
            ::std::mem::transmute::<__BindgenBitfieldUnit<[u8; 2usize]>, u16>(self._bitfield_1)
        };
        how_raw & 0b11
    }
    #[inline]
    pub fn ndims_custom(&self) -> u16 {
        let ndims_raw: u16 = unsafe {
            ::std::mem::transmute::<__BindgenBitfieldUnit<[u8; 2usize]>, u16>(self._bitfield_1)
        };
        ndims_raw >> 2 & 0b111_111_111
    }
    #[inline]
    pub fn ptrarray_custom(&self) -> u16 {
        let ptrarray_raw: u16 = unsafe {
            ::std::mem::transmute::<__BindgenBitfieldUnit<[u8; 2usize]>, u16>(self._bitfield_1)
        };
        ptrarray_raw >> 12 & 0b1
    }
    #[inline]
    pub fn hasptr_custom(&self) -> u16 {
        let hasptr_raw: u16 = unsafe {
            ::std::mem::transmute::<__BindgenBitfieldUnit<[u8; 2usize]>, u16>(self._bitfield_1)
        };
        hasptr_raw >> 13 & 0b1
    }
}

impl mmtk__jl_task_t {
    #[inline]
    pub fn copy_stack_custom(&self) -> u32 {
        let copy_stack_raw: u32 = unsafe {
            ::std::mem::transmute::<__BindgenBitfieldUnit<[u8; 4usize]>, u32>(self._bitfield_1)
        };
        copy_stack_raw & 2147483647u32
    }
}

#[no_mangle]
pub extern "C" fn mmtk_julia_copy_stack_check(c_flag_is_defined: bool) {
    if c_flag_is_defined {
        #[cfg(not(feature = "julia_copy_stack"))]
        panic!("COPY_STACK flag has been defined in C, but `julia_copy_stack` feature has not been set.")
    } else {
        #[cfg(feature = "julia_copy_stack")]
        panic!("COPY_STACK flag has not been defined in C, but `julia_copy_stack` feature has been set.")
    }
}

#[no_mangle]
pub extern "C" fn mmtk_get_possibly_forwared(object: ObjectReference) -> ObjectReference {
    match object.get_forwarded_object() {
        Some(forwarded) => forwarded,
        None => object,
    }
}
