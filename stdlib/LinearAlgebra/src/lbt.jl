# This file is a part of Julia. License is MIT: https://julialang.org/license

## This file contains libblastrampoline-specific APIs

# Keep these in sync with `src/libblastrampoline_internal.h`
struct lbt_library_info_t
    libname::Cstring
    handle::Ptr{Cvoid}
    suffix::Cstring
    active_forwards::Ptr{UInt8}
    interface::Int32
    complex_retstyle::Int32
    f2c::Int32
    cblas::Int32
end
const LBT_INTERFACE_LP64    = 32
const LBT_INTERFACE_ILP64   = 64
const LBT_INTERFACE_UNKNOWN = -1
const LBT_INTERFACE_MAP = Dict(
    LBT_INTERFACE_LP64    => :lp64,
    LBT_INTERFACE_ILP64   => :ilp64,
    LBT_INTERFACE_UNKNOWN => :unknown,
)
const LBT_INV_INTERFACE_MAP = Dict(v => k for (k, v) in LBT_INTERFACE_MAP)

const LBT_F2C_PLAIN         =  0
const LBT_F2C_REQUIRED      =  1
const LBT_F2C_UNKNOWN       = -1
const LBT_F2C_MAP = Dict(
    LBT_F2C_PLAIN    => :plain,
    LBT_F2C_REQUIRED => :required,
    LBT_F2C_UNKNOWN  => :unknown,
)
const LBT_INV_F2C_MAP = Dict(v => k for (k, v) in LBT_F2C_MAP)

const LBT_COMPLEX_RETSTYLE_NORMAL   =  0
const LBT_COMPLEX_RETSTYLE_ARGUMENT =  1
const LBT_COMPLEX_RETSTYLE_UNKNOWN  = -1
const LBT_COMPLEX_RETSTYLE_MAP = Dict(
    LBT_COMPLEX_RETSTYLE_NORMAL   => :normal,
    LBT_COMPLEX_RETSTYLE_ARGUMENT => :argument,
    LBT_COMPLEX_RETSTYLE_UNKNOWN  => :unknown,
)
const LBT_INV_COMPLEX_RETSTYLE_MAP = Dict(v => k for (k, v) in LBT_COMPLEX_RETSTYLE_MAP)

const LBT_CBLAS_CONFORMANT =  0
const LBT_CBLAS_DIVERGENT  =  1
const LBT_CBLAS_UNKNOWN    = -1
const LBT_CBLAS_MAP = Dict(
    LBT_CBLAS_CONFORMANT => :conformant,
    LBT_CBLAS_DIVERGENT  => :divergent,
    LBT_CBLAS_UNKNOWN    => :unknown,
)
const LBT_INV_CBLAS_MAP = Dict(v => k for (k, v) in LBT_CBLAS_MAP)

struct LBTLibraryInfo
    libname::String
    handle::Ptr{Cvoid}
    suffix::String
    active_forwards::Vector{UInt8}
    interface::Symbol
    complex_retstyle::Symbol
    f2c::Symbol
    cblas::Symbol

    function LBTLibraryInfo(lib_info::lbt_library_info_t, num_exported_symbols::UInt32)
        return new(
            unsafe_string(lib_info.libname),
            lib_info.handle,
            unsafe_string(lib_info.suffix),
            unsafe_wrap(Vector{UInt8}, lib_info.active_forwards, div(num_exported_symbols,8)+1),
            LBT_INTERFACE_MAP[lib_info.interface],
            LBT_COMPLEX_RETSTYLE_MAP[lib_info.complex_retstyle],
            LBT_F2C_MAP[lib_info.f2c],
            LBT_CBLAS_MAP[lib_info.cblas],
        )
    end
end

struct lbt_config_t
    loaded_libs::Ptr{Ptr{lbt_library_info_t}}
    build_flags::UInt32
    exported_symbols::Ptr{Cstring}
    num_exported_symbols::UInt32
end
const LBT_BUILDFLAGS_DEEPBINDLESS = 0x01
const LBT_BUILDFLAGS_F2C_CAPABLE  = 0x02
const LBT_BUILDFLAGS_MAP = Dict(
    LBT_BUILDFLAGS_DEEPBINDLESS => :deepbindless,
    LBT_BUILDFLAGS_F2C_CAPABLE => :f2c_capable,
)

struct LBTConfig
    loaded_libs::Vector{LBTLibraryInfo}
    build_flags::Vector{Symbol}
    exported_symbols::Vector{String}

    function LBTConfig(config::lbt_config_t)
        # Decode OR'ed flags into a list of names
        build_flag_names = Symbol[]
        for (flag, name) in LBT_BUILDFLAGS_MAP
            if config.build_flags & flag != 0x00
                push!(build_flag_names, name)
            end
        end

        # Load all exported symbol names
        exported_symbols = String[]
        for sym_idx in 1:config.num_exported_symbols
            str_ptr = unsafe_load(config.exported_symbols, sym_idx)
            if str_ptr != C_NULL
                push!(exported_symbols, unsafe_string(str_ptr))
            else
                @error("NULL string in lbt_config.exported_symbols[$(sym_idx)]")
            end
        end

        # Unpack library info structures
        libs = LBTLibraryInfo[]
        idx = 1
        lib_ptr = unsafe_load(config.loaded_libs, idx)
        while lib_ptr != C_NULL
            push!(libs, LBTLibraryInfo(unsafe_load(lib_ptr), config.num_exported_symbols))

            idx += 1
            lib_ptr = unsafe_load(config.loaded_libs, idx)
        end
        return new(
            libs,
            build_flag_names,
            exported_symbols,
        )
    end
end

Base.show(io::IO, lbt::LBTLibraryInfo) = print(io, "LBTLibraryInfo(", basename(lbt.libname), ", ", lbt.interface, ")")
function Base.show(io::IO, mime::MIME{Symbol("text/plain")}, lbt::LBTLibraryInfo)
    summary(io, lbt); println(io)
    println(io, "├ Library: ", basename(lbt.libname))
    println(io, "├ Interface: ", lbt.interface)
    println(io, "├ Complex return style: ", lbt.complex_retstyle)
    println(io, "├ F2C: ", lbt.f2c)
      print(io, "└ CBLAS: ", lbt.cblas)
end

function Base.show(io::IO, lbt::LBTConfig)
    if length(lbt.loaded_libs) <= 3
        print(io, "LBTConfig(")
        gen = (string("[", uppercase(string(l.interface)), "] ",
            basename(l.libname)) for l in lbt.loaded_libs)
        print(io, join(gen, ", "))
        print(io, ")")
    else
        print(io, "LBTConfig(...)")
    end
end
function Base.show(io::IO, mime::MIME{Symbol("text/plain")}, lbt::LBTConfig)
    summary(io, lbt); println(io)
    println(io, "Libraries: ")
    for (i,l) in enumerate(lbt.loaded_libs)
        char = i == length(lbt.loaded_libs) ? "└" : "├"
        interface_str = if l.interface == :ilp64
            "ILP64"
        elseif l.interface == :lp64
            " LP64"
        else
            "UNKWN"
        end
        print(io, char, " [", interface_str,"] ", basename(l.libname))
        i !== length(lbt.loaded_libs) && println()
    end
end

mutable struct ConfigCache
    @atomic config::Union{Nothing,LBTConfig}
    lock::ReentrantLock
end

# In the event that users want to call `lbt_get_config()` multiple times (e.g. for
# runtime checks of which BLAS vendor is providing a symbol), let's cache the value
# and clear it only when someone calls something that would cause it to change.
const _CACHED_CONFIG = ConfigCache(nothing, ReentrantLock())

function lbt_get_config()
    config = @atomic :acquire _CACHED_CONFIG.config
    config === nothing || return config
    return lock(_CACHED_CONFIG.lock) do
        local config = @atomic :monotonic _CACHED_CONFIG.config
        config === nothing || return config
        config_ptr = ccall((:lbt_get_config, libblastrampoline), Ptr{lbt_config_t}, ())
        @atomic :release _CACHED_CONFIG.config = LBTConfig(unsafe_load(config_ptr))
    end
end

function _clear_config_with(f)
    lock(_CACHED_CONFIG.lock) do
        @atomic :release _CACHED_CONFIG.config = nothing
        f()
    end
end

function lbt_get_num_threads()
    return ccall((:lbt_get_num_threads, libblastrampoline), Int32, ())
end

function lbt_set_num_threads(nthreads)
    return ccall((:lbt_set_num_threads, libblastrampoline), Cvoid, (Int32,), nthreads)
end

function lbt_forward(path; clear::Bool = false, verbose::Bool = false, suffix_hint::Union{String,Nothing} = nothing)
    _clear_config_with() do
        return ccall((:lbt_forward, libblastrampoline), Int32, (Cstring, Int32, Int32, Cstring), path, clear ? 1 : 0, verbose ? 1 : 0, something(suffix_hint, C_NULL))
    end
end

function lbt_set_default_func(addr)
    _clear_config_with() do
        return ccall((:lbt_set_default_func, libblastrampoline), Cvoid, (Ptr{Cvoid},), addr)
    end
end

function lbt_get_default_func()
    return ccall((:lbt_get_default_func, libblastrampoline), Ptr{Cvoid}, ())
end

"""
    lbt_find_backing_library(symbol_name, interface; config::LBTConfig = lbt_get_config())

Return the `LBTLibraryInfo` that represents the backing library for the given symbol
exported from libblastrampoline.  This allows us to discover which library will service
a particular BLAS call from Julia code.  This method returns `nothing` if either of the
following conditions are met:

 * No loaded library exports the desired symbol (the default function will be called)
 * The symbol was set via `lbt_set_forward()`, which does not track library provenance.

If the given `symbol_name` is not contained within the list of exported symbols, an
`ArgumentError` will be thrown.
"""
function lbt_find_backing_library(symbol_name, interface::Symbol;
                                  config::LBTConfig = lbt_get_config())
    if interface ∉ (:ilp64, :lp64)
        throw(Argument("Invalid interface specification: '$(interface)'"))
    end
    symbol_idx = findfirst(s -> s == symbol_name, config.exported_symbols)
    if symbol_idx === nothing
        throw(ArgumentError("Invalid exported symbol name '$(symbol_name)'"))
    end
    # Convert to zero-indexed
    symbol_idx -= 1

    forward_byte_offset = div(symbol_idx, 8)
    forward_byte_mask = 1 << mod(symbol_idx, 8)
    for lib in filter(l -> l.interface == interface, config.loaded_libs)
        if lib.active_forwards[forward_byte_offset+1] & forward_byte_mask != 0x00
            return lib
        end
    end

    # No backing library was found
    return nothing
end


## NOTE: Manually setting forwards is referred to as the 'footgun API'.  It allows truly
## bizarre and complex setups to be created.  If you run into strange errors while using
## it, the first thing you should ask yourself is whether you've set things up properly.
function lbt_set_forward(symbol_name, addr, interface,
                         complex_retstyle = LBT_COMPLEX_RETSTYLE_NORMAL,
                         f2c = LBT_F2C_PLAIN; verbose::Bool = false)
    _clear_config_with() do
        return ccall(
            (:lbt_set_forward, libblastrampoline),
            Int32,
            (Cstring, Ptr{Cvoid}, Int32, Int32, Int32, Int32),
            string(symbol_name),
            addr,
            Int32(interface),
            Int32(complex_retstyle),
            Int32(f2c),
            verbose ? Int32(1) : Int32(0),
        )
    end
end
function lbt_set_forward(symbol_name, addr, interface::Symbol,
                         complex_retstyle::Symbol = :normal,
                         f2c::Symbol = :plain; kwargs...)
    return lbt_set_forward(symbol_name, addr,
                           LBT_INV_INTERFACE_MAP[interface],
                           LBT_INV_COMPLEX_RETSTYLE_MAP[complex_retstyle],
                           LBT_INV_F2C_MAP[f2c];
                           kwargs...)
end

function lbt_get_forward(symbol_name, interface, f2c = LBT_F2C_PLAIN)
    return ccall(
        (:lbt_get_forward, libblastrampoline),
        Ptr{Cvoid},
        (Cstring, Int32, Int32),
        string(symbol_name),
        Int32(interface),
        Int32(f2c),
    )
end
function lbt_get_forward(symbol_name, interface::Symbol, f2c::Symbol = :plain)
    return lbt_get_forward(symbol_name, LBT_INV_INTERFACE_MAP[interface], LBT_INV_F2C_MAP[f2c])
end
