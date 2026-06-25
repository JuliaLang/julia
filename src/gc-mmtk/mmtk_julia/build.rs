extern crate bindgen;
use std::env;
use std::path::{Path, PathBuf};

// Use bindgen to build Rust bindings for Julia

fn path_string(path: PathBuf) -> String {
    path.display().to_string()
}

fn main() {
    let crate_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let mmtk_dir = crate_dir
        .parent()
        .expect("mmtk-julia should be in JULIAHOME/src/gc-mmtk/mmtk_julia")
        .to_path_buf();
    let julia_dir = mmtk_dir
        .parent()
        .and_then(Path::parent)
        .expect("mmtk-julia should be in JULIAHOME/src/gc-mmtk/mmtk_julia")
        .to_path_buf();
    let buildroot_dir = julia_dir.clone();

    println!(
        "cargo:rerun-if-changed={}",
        path_string(crate_dir.join("src/julia_types.rs"))
    );
    println!(
        "cargo:rerun-if-changed={}",
        path_string(crate_dir.join("api/mmtk.h"))
    );
    println!(
        "cargo:rerun-if-changed={}",
        path_string(crate_dir.join("api/mmtkMutator.h"))
    );
    println!(
        "cargo:rerun-if-changed={}",
        path_string(julia_dir.join("VERSION"))
    );
    // These auto-generated sources update on every Julia build, so ignore them
    const IGNORE: &[&str] = &[
        "julia_version.h",
        "jl_internal_funcs.inc",
        "jl_data_globals_defs.inc",
        "julia_flisp.boot.inc",
    ];
    for dir in [julia_dir.join("src"), julia_dir.join("src/support")] {
        for entry in std::fs::read_dir(dir).expect("failed to read Julia header directory") {
            let path = entry.expect("failed to read Julia header").path();
            let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
            if IGNORE.contains(&name) {
                continue;
            }
            if path
                .extension()
                .is_some_and(|ext| ext == "h" || ext == "inc")
            {
                println!("cargo:rerun-if-changed={}", path_string(path));
            }
        }
    }
    for entry in std::fs::read_dir(&mmtk_dir).expect("failed to read MMTk integration directory") {
        let path = entry.expect("failed to read MMTk integration file").path();
        if path.extension().is_some_and(|ext| ext == "h") {
            println!("cargo:rerun-if-changed={}", path_string(path));
        }
    }
    for entry in
        std::fs::read_dir(crate_dir.join("api")).expect("failed to read MMTk API directory")
    {
        let path = entry.expect("failed to read MMTk API file").path();
        if path.extension().is_some_and(|ext| ext == "h") {
            println!("cargo:rerun-if-changed={}", path_string(path));
        }
    }

    // running `make julia_version.h` in $JULIAHOME/src to generate julia_version.h
    if !buildroot_dir.join("src/julia_version.h").exists() {
        std::process::Command::new("make")
            .current_dir(julia_dir.join("src"))
            .env("BUILDDIR", &buildroot_dir)
            .args(["julia_version.h"])
            .output()
            .expect("failed to execute process");
    }

    // running `make` in $JULIAHOME/deps to generate $JULIAHOME/usr/include, in particular libunwind.h
    // skip this process if that path already exists since
    // the .h files could have already been generated when building via Makefile
    if !buildroot_dir.join("usr/include").exists() {
        std::process::Command::new("make")
            .current_dir(julia_dir.join("deps"))
            .env("BUILDDIR", &buildroot_dir)
            .env("MMTK_PLAN", "None") // Make sure this call doesn't try to compile the binding again
            .output()
            .expect("failed to execute process");
    }

    let mut builder = bindgen::Builder::default()
        .header(path_string(julia_dir.join("src/julia.h")))
        .header(path_string(julia_dir.join("src/julia_internal.h")))
        // Including the paths to depending .h files
        .clang_arg("-I")
        .clang_arg(path_string(crate_dir.join("api")))
        .clang_arg("-I")
        .clang_arg(path_string(mmtk_dir.clone()))
        .clang_arg("-I")
        .clang_arg(path_string(julia_dir.join("src")))
        .clang_arg("-I")
        .clang_arg(path_string(julia_dir.join("src/support")))
        .clang_arg("-I")
        .clang_arg(path_string(buildroot_dir.join("usr/include")));

    if let Some(include_dirs) = env::var_os("MMTK_JULIA_BINDGEN_INCLUDE_DIRS") {
        for include_dir in env::split_paths(&include_dirs) {
            builder = builder.clang_arg("-I").clang_arg(path_string(include_dir));
        }
    }

    let bindings = builder
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
        .clang_arg("-DWITH_THIRD_PARTY_HEAP=1")
        // using sticky, but it should not matter for the FFI bindings
        .clang_arg("-DMMTK_PLAN_STICKYIMMIX")
        // Finish the builder and generate the bindings.
        .generate()
        // Unwrap the Result and panic on failure.
        .expect("Unable to generate bindings");

    bindings
        .write_to_file("src/julia_types.rs")
        .expect("Couldn't write bindings!");

    built::write_built_file().expect("Failed to acquire build-time information");
}
