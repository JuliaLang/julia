use std::ffi::CString;

mod raw {
    // The include imports a full list of the constants in built.rs from https://docs.rs/built/latest/built/index.html
    include!(concat!(env!("OUT_DIR"), "/built.rs"));
}

lazy_static! {
    // Owned string for the binding version, such as MMTk Julia 0.14.0 (cfc755f-dirty)
    static ref BINDING_VERSION_STRING: String = match (raw::GIT_COMMIT_HASH, raw::GIT_DIRTY) {
        (Some(hash), Some(dirty)) => format!("Built with MMTk Julia {} ({}{})", raw::PKG_VERSION, hash.split_at(7).0, if dirty { "-dirty" } else { "" }),
        (Some(hash), None) => format!("Built with MMTk Julia {} ({}{})", raw::PKG_VERSION, hash.split_at(7).0, "-?"),
        _ => format!("Built with MMTk Julia {}", raw::PKG_VERSION),
    };
    // Owned string for both binding and core version.
    #[derive(Debug)]
    pub static ref MMTK_JULIA_FULL_VERSION_STRING: CString = CString::new(format!("{}, using {}", *BINDING_VERSION_STRING, *mmtk::build_info::MMTK_FULL_BUILD_INFO)).unwrap();
}
