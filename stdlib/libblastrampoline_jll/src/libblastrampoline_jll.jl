# This file is a part of Julia. License is MIT: https://julialang.org/license

## dummy stub for https://github.com/JuliaBinaryWrappers/libblastrampoline_jll.jl

baremodule libblastrampoline_jll
using Base, Libdl

export libblastrampoline

# These get calculated in __init__()
const PATH = Ref("")
const PATH_list = String[]
const LIBPATH = Ref("")
const LIBPATH_list = String[]
artifact_dir::String = ""

# Because LBT needs to have a weak-dependence on OpenBLAS (or any other BLAS)
# we must manually construct a list of which modules and libraries we're going
# to be using with it, as well as the on load callbacks they may or may not need.
#
# Each callback should be a `Libdl.LazyLibraryCallback` to be usable under `--trim`.
const on_load_callbacks::Vector{Any} = Any[]
const eager_mode_modules::Vector{Module} = Module[]
struct LBTOnLoadCallback <: Libdl.LazyLibraryCallback end
function (::LBTOnLoadCallback)()
    for callback = on_load_callbacks
        Base.Libc.Libdl._invoke_on_load_callback(callback)
    end
end

function add_dependency!(mod::Module, lib::LazyLibrary, on_load_callback = nothing)
    Libdl.add_dependency!(libblastrampoline, lib)
    push!(eager_mode_modules, mod)
    on_load_callback === nothing || push!(on_load_callbacks, on_load_callback)
    return nothing
end

libblastrampoline_path::String = ""
const libblastrampoline = LazyLibrary(
    # NOTE: keep in sync with `Base.libblas_name` and `Base.liblapack_name`.
    if Sys.iswindows()
        BundledLazyLibraryPath("libblastrampoline-5.dll")
    elseif Sys.isapple()
        BundledLazyLibraryPath("libblastrampoline.5.dylib")
    else
        BundledLazyLibraryPath("libblastrampoline.so.5")
    end,
    id = LibraryID(Base.UUID("8e850b90-86db-534c-a0d3-1478176c7d93"), "libblastrampoline"),
    dependencies = LazyLibrary[],
    on_load_callback = LBTOnLoadCallback()
)

function eager_mode()
    for mod in eager_mode_modules
        mod.eager_mode()
    end
    dlopen(libblastrampoline)
end
is_available() = true

# JLLWrappers path compatibility accessor
get_libblastrampoline_path() = libblastrampoline_path

function __init__()
    global libblastrampoline_path = string(libblastrampoline.path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libblastrampoline_path)
    push!(LIBPATH_list, LIBPATH[])
end

if Base.generating_output()
    precompile(eager_mode, ())
    precompile(is_available, ())
end

end  # module libblastrampoline_jll
