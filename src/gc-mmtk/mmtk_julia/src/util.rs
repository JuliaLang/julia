use enum_map::Enum;
use mmtk::util::ObjectReference;

/// Preserve errno and Windows last-error across MMTk calls, including finalizers.
/// The guarded operation must stay on the current thread.
pub(crate) struct PreserveErrno {
    errno_ptr: *mut libc::c_int,
    errno: libc::c_int,
    #[cfg(windows)]
    last_error: u32,
}

#[cfg(any(target_os = "linux", target_os = "android"))]
unsafe fn errno_location() -> *mut libc::c_int {
    libc::__errno_location()
}
#[cfg(any(target_os = "macos", target_os = "freebsd"))]
unsafe fn errno_location() -> *mut libc::c_int {
    libc::__error()
}
#[cfg(windows)]
unsafe fn errno_location() -> *mut libc::c_int {
    extern "C" {
        fn _errno() -> *mut libc::c_int;
    }
    _errno()
}

#[cfg(windows)]
extern "system" {
    fn GetLastError() -> u32;
    fn SetLastError(code: u32);
}

impl PreserveErrno {
    pub(crate) fn new() -> Self {
        unsafe {
            let errno_ptr = errno_location();
            PreserveErrno {
                errno_ptr,
                errno: *errno_ptr,
                #[cfg(windows)]
                last_error: GetLastError(),
            }
        }
    }
}

impl Drop for PreserveErrno {
    fn drop(&mut self) {
        unsafe {
            #[cfg(windows)]
            SetLastError(self.last_error);
            *self.errno_ptr = self.errno;
        }
    }
}

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

// The functions below allow accessing the values of bitfields without performing a for loop
use crate::julia_types::{__BindgenBitfieldUnit, jl_datatype_layout_t, jl_ucontext_t};

impl jl_datatype_layout_t {
    #[inline]
    pub fn fielddesc_type_custom(&self) -> u16 {
        let fielddesc_type_raw: u16 = unsafe {
            ::std::mem::transmute::<__BindgenBitfieldUnit<[u8; 2usize]>, u16>(
                self.flags._bitfield_1,
            )
        };
        fielddesc_type_raw >> 1 & 0b11
    }
}

impl jl_ucontext_t {
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
pub extern "C" fn mmtk_get_possibly_forwarded(object: ObjectReference) -> ObjectReference {
    match object.get_forwarded_object() {
        Some(forwarded) => forwarded,
        None => object,
    }
}
