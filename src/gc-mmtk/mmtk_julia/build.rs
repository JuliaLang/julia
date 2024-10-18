extern crate bindgen;

// Use bindgen to build Rust bindings for Julia

fn main() {
    // Use environment variable $JULIA_PATH that points to Julia folder
    let julia_dir_key = "JULIA_PATH";
    let mmtk_dir_key = "MMTK_JULIA_DIR";

    let (mmtk_dir, julia_dir) = match (std::env::var(mmtk_dir_key), std::env::var(julia_dir_key)) {
        (Ok(mmtk_val), Ok(julia_val)) => (mmtk_val, julia_val),
        _ => panic!("Must set {} and {}", julia_dir_key, mmtk_dir_key),
    };

    // running `make julia_version.h` in $JULIA_PATH/src to generate julia_version.h
    std::process::Command::new("make")
        .current_dir(format!("{}/src", julia_dir))
        .args(["julia_version.h"])
        .output()
        .expect("failed to execute process");

    // runing `make` in $JULIA_PATH/deps to generate $JULIA_PATH/usr/include, in particular libunwind.h
    std::process::Command::new("make")
        .current_dir(format!("{}/deps", julia_dir))
        .output()
        .expect("failed to execute process");

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
        .clang_arg(format!("{}/usr/include", julia_dir))
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
