extern crate bindgen;
use std::path::Path;

// Use bindgen to build Rust bindings for Julia

fn main() {
    // Use environment variable $JULIA_PATH that points to Julia folder
    let julia_dir_key = "JULIA_PATH";
    let mmtk_dir_key = "MMTK_JULIA_DIR";
    let buildroot_dir_key = "JULIA_BUILDROOT";

    let mmtk_dir = match std::env::var(mmtk_dir_key) {
        Ok(mmtk_val) => mmtk_val,
        _ => "..".to_string(),
    };

    // If bindings file already exists, no need to do anything
    if !Path::new(format!("{}/mmtk/src/julia_types.rs", mmtk_dir).as_str()).exists() {
        let julia_dir = match std::env::var(julia_dir_key) {
            Ok(julia_val) => julia_val,
            _ => panic!("Must set {}", julia_dir_key),
        };

        // A build call from Julia's Makefile may build into a different directory
        // e.g., via make O=/path-to-my-build/my-julia-build
        // Check if JULIA_BUILD_ROOT is set and use it, otherwise, set it as the same dir as JULIA_PATH
        let buildroot_dir = match std::env::var(buildroot_dir_key) {
            Ok(buildroot_val) => buildroot_val,
            _ => julia_dir.clone(),
        };

        // running `make julia_version.h` in $JULIA_PATH/src to generate julia_version.h
        if !Path::new(format!("{}/src/julia_version.h", buildroot_dir).as_str()).exists() {
            std::process::Command::new("make")
                .current_dir(format!("{}/src", julia_dir))
                .env("BUILDDIR", buildroot_dir.clone())
                .args(["julia_version.h"])
                .output()
                .expect("failed to execute process");
        }

        // runing `make` in $JULIA_PATH/deps to generate $JULIA_PATH/usr/include, in particular libunwind.h
        // skip this process if that path already exists since
        // the .h files could have already beeen generated when building via Makefile
        if !Path::new(format!("{}/usr/include", buildroot_dir).as_str()).exists() {
            std::process::Command::new("make")
                .current_dir(format!("{}/deps", julia_dir))
                .env("BUILDDIR", buildroot_dir.clone())
                .env("MMTK_PLAN", "None") // Make sure this call doesn't try to compile the binding again
                .output()
                .expect("failed to execute process");
        }

        let bindings = bindgen::Builder::default()
            .header(format!("{}/src/julia.h", julia_dir))
            .header(format!("{}/src/julia_internal.h", julia_dir))
            // Including the paths to depending .h files
            .clang_arg("-I")
            .clang_arg(format!("{}/mmtk/api", mmtk_dir))
            .clang_arg("-I")
            .clang_arg(format!("{}/src", julia_dir))
            .clang_arg("-I")
            .clang_arg(format!("{}/src/support", julia_dir))
            .clang_arg("-I")
            .clang_arg(format!("{}/usr/include", buildroot_dir))
            // all types that we generate bindings from
            .allowlist_item("jl_datatype_layout_t")
            .allowlist_item("jl_ucontext_t")
            .allowlist_item("jl_small_typeof_tags")
            .allowlist_item("jl_*_tag")
            .allowlist_item("jl_svec_t")
            .allowlist_item("jl_module_t")
            .allowlist_item("jl_task_t")
            .allowlist_item("jl_datatype_t")
            .allowlist_item("jl_weakref_t")
            .allowlist_item("jl_binding_partition_t")
            .allowlist_item("jl_bt_element_t")
            .allowlist_item("jl_taggedvalue_t")
            .allowlist_item("MMTkMutatorContext")
            // --opaque-type MMTkMutatorContext
            .opaque_type("MMTkMutatorContext")
            // compile using c++
            .clang_arg("-x")
            .clang_arg("c++")
            .clang_arg("-std=c++14")
            // using MMTK types
            .clang_arg("-DMMTK_GC")
            // Finish the builder and generate the bindings.
            .generate()
            // Unwrap the Result and panic on failure.
            .expect("Unable to generate bindings");

        bindings
            .write_to_file("src/julia_types.rs")
            .expect("Couldn't write bindings!");
    }

    built::write_built_file().expect("Failed to acquire build-time information");
}
