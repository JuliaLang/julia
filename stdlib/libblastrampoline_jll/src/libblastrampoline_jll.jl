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
const on_load_callbacks::Vector{Ptr{Cvoid}} = Ptr{Cvoid}[]

const eager_mode_modules::Vector{Module} = Module[]
function libblastrampoline_on_load_callback()
    for callback in on_load_callbacks
        ccall(callback, Cvoid, ())
    end
end

function add_dependency!(mod::Module, lib::LazyLibrary, on_load_callback::Ptr{Cvoid} = C_NULL)
    Libdl.add_dependency!(libblastrampoline, lib)
    push!(eager_mode_modules, mod)
    if on_load_callback !== C_NULL
        push!(on_load_callbacks, on_load_callback)
    end
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
    dependencies = LazyLibrary[],
    _on_load_c_callback = @cfunction(libblastrampoline_on_load_callback, Cvoid, ())
)

function eager_mode()
    for mod in eager_mode_modules
        mod.eager_mode()
    end
    dlopen(libblastrampoline)
end
is_available() = true

function __init__()
    # _on_load_c_callback does not survive precompilation so it needs
    # to be manually restored in `__init__`
    # FIXME: Delete this once `on_load_callback` is trim-compatible.
    fptr = @cfunction(libblastrampoline_on_load_callback, Cvoid, ())
    @atomic :release libblastrampoline._on_load_c_callback = fptr

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
