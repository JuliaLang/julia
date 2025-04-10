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
libblastrampoline_path::String = ""


# Because LBT needs to have a weak-dependence on OpenBLAS (or any other BLAS)
# we must manually construct a list of which modules and libraries we're going
# to be using with it, as well as the on load callbacks they may or may not need.
const on_load_callbacks::Vector{Function} = Function[]
const eager_mode_modules::Vector{Module} = Module[]
function libblastrampoline_on_load_callback()
    for callback = on_load_callbacks
        callback()
    end
end

function add_dependency!(mod::Module, lib::LazyLibrary, on_load_callback::Function = () -> nothing)
    Libdl.add_dependency!(libblastrampoline, lib)
    push!(eager_mode_modules, mod)
    push!(on_load_callbacks, on_load_callback)
end

# NOTE: keep in sync with `Base.libblas_name` and `Base.liblapack_name`.
const _libblastrampoline_path = if Sys.iswindows()
    BundledLazyLibraryPath("libblastrampoline-5.dll")
elseif Sys.isapple()
    BundledLazyLibraryPath("libblastrampoline.5.dylib")
else
    BundledLazyLibraryPath("libblastrampoline.so.5")
end
const libblastrampoline = LazyLibrary(_libblastrampoline_path, dependencies=[],
                                      on_load_callback=libblastrampoline_on_load_callback)

function eager_mode()
    for mod in eager_mode_modules
        mod.eager_mode()
    end
    dlopen(libblastrampoline)
end
is_available() = true

function __init__()
    global libblastrampoline_path = string(_libblastrampoline_path)
    global artifact_dir = dirname(Sys.BINDIR)
    LIBPATH[] = dirname(libblastrampoline_path)
    push!(LIBPATH_list, LIBPATH[])
end
end  # module libblastrampoline_jll
