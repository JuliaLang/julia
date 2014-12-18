# NOTE: This type needs to be kept in sync with jl_options in src/julia.h
immutable JLOptions
    version::Int8
    quiet::Int8
    julia_home::Ptr{Cchar}
    julia_bin::Ptr{Cchar}
    build_path::Ptr{Cchar}
    eval::Ptr{Cchar}
    print::Ptr{Cchar}
    postboot::Ptr{Cchar}
    load::Ptr{Cchar}
    image_file::Ptr{Cchar}
    cpu_target::Ptr{Cchar}
    nprocs::Clong
    machinefile::Ptr{Cchar}
    isinteractive::Int8
    color::Int8
    historyfile::Int8
    startupfile::Int8
    compile_enabled::Int8
    code_coverage::Int8
    malloc_log::Int8
    opt_level::Int8
    check_bounds::Int8
    int_literals::Cint
    dumpbitcode::Int8
    depwarn::Int8
    can_inline::Int8
    fast_math::Int8
    worker::Int8
    bindto::Ptr{Cchar}
end

JLOptions() = unsafe_load(cglobal(:jl_options, JLOptions))
