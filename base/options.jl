# This file is a part of Julia. License is MIT: http://julialang.org/license

# NOTE: This type needs to be kept in sync with jl_options in src/julia.h
immutable JLOptions
    quiet::Int8
    julia_home::Ptr{UInt8}
    julia_bin::Ptr{UInt8}
    build_path::Ptr{UInt8}
    eval::Ptr{UInt8}
    print::Ptr{UInt8}
    postboot::Ptr{UInt8}
    load::Ptr{UInt8}
    image_file::Ptr{UInt8}
    cpu_target::Ptr{UInt8}
    nprocs::Int32
    machinefile::Ptr{UInt8}
    isinteractive::Int8
    color::Int8
    historyfile::Int8
    startupfile::Int8
    compile_enabled::Int8
    code_coverage::Int8
    malloc_log::Int8
    opt_level::Int8
    check_bounds::Int8
    dumpbitcode::Int8
    depwarn::Int8
    can_inline::Int8
    fast_math::Int8
    worker::Int8
    bindto::Ptr{UInt8}
end

JLOptions() = unsafe_load(cglobal(:jl_options, JLOptions))
