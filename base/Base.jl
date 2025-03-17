# This file is a part of Julia. License is MIT: https://julialang.org/license

const start_base_include = time_ns()

include("reflection.jl")
include("refpointer.jl")

# now replace the Pair constructor (relevant for NamedTuples) with one that calls our Base.convert
delete_method(which(Pair{Any,Any}, (Any, Any)))
@eval function (P::Type{Pair{A, B}})(@nospecialize(a), @nospecialize(b)) where {A, B}
    @inline
    return $(Expr(:new, :P, :(a isa A ? a : convert(A, a)), :(b isa B ? b : convert(B, b))))
end

# The REPL stdlib hooks into Base using this Ref
const REPL_MODULE_REF = Ref{Module}(Base)
process_sysimg_args!()

include(strcat(BUILDROOT, "build_h.jl"))     # include($BUILDROOT/base/build_h.jl)
include(strcat(BUILDROOT, "version_git.jl")) # include($BUILDROOT/base/version_git.jl)

# Initialize DL_LOAD_PATH as early as possible.  We are defining things here in
# a slightly more verbose fashion than usual, because we're running so early.
let os = ccall(:jl_get_UNAME, Any, ())
    if os === :Darwin || os === :Apple
        if Base.DARWIN_FRAMEWORK
            push!(DL_LOAD_PATH, "@loader_path/Frameworks")
        end
        push!(DL_LOAD_PATH, "@loader_path")
    end
end

# subarrays
include("subarray.jl")
include("views.jl")

# numeric operations
include("hashing.jl")
include("div.jl")
include("twiceprecision.jl")
include("complex.jl")
include("rational.jl")
include("multinverses.jl")
using .MultiplicativeInverses
include("abstractarraymath.jl")
include("arraymath.jl")
include("slicearray.jl")

# SIMD loops
sizeof(s::String) = Core.sizeof(s)  # needed by gensym as called from simdloop
include("simdloop.jl")
using .SimdLoop

# map-reduce operators
include("reduce.jl")

## core structures
include("reshapedarray.jl")
include("reinterpretarray.jl")

include("multimedia.jl")
using .Multimedia

# Some type
include("some.jl")

include("dict.jl")
include("set.jl")

# Strings
include("char.jl")
function array_new_memory(mem::Memory{UInt8}, newlen::Int)
    # add an optimization to array_new_memory for StringVector
    if (@assume_effects :total @ccall jl_genericmemory_owner(mem::Any,)::Any) === mem
        # TODO: when implemented, this should use a memory growing call
        return typeof(mem)(undef, newlen)
    else
        # If data is in a String, keep it that way.
        # When implemented, this could use jl_gc_expand_string(oldstr, newlen) as an optimization
        str = _string_n(newlen)
        return (@assume_effects :total !:consistent @ccall jl_string_to_genericmemory(str::Any,)::Memory{UInt8})
    end
end
include("strings/basic.jl")
include("strings/string.jl")
include("strings/substring.jl")
include("strings/cstring.jl")

include("osutils.jl")

# Core I/O
include("io.jl")
include("iobuffer.jl")

# Concurrency (part 1)
include("linked_list.jl")
include("condition.jl")
include("threads.jl")
include("lock.jl")

# strings & printing
include("intfuncs.jl")
include("strings/strings.jl")
include("regex.jl")
include("parse.jl")
include("shell.jl")
const IRShow = Compiler.IRShow # an alias for compatibility
include("stacktraces.jl")
using .StackTraces
include("show.jl")
include("arrayshow.jl")
include("methodshow.jl")

# multidimensional arrays
include("cartesian.jl")
using .Cartesian
include("multidimensional.jl")

include("broadcast.jl")
using .Broadcast
using .Broadcast: broadcasted, broadcasted_kwsyntax, materialize, materialize!,
                  broadcast_preserving_zero_d, andand, oror

# missing values
include("missing.jl")

# version
include("version.jl")

# system & environment
include("sysinfo.jl")
include("libc.jl")
using .Libc: getpid, gethostname, time, memcpy, memset, memmove, memcmp

const USING_STOCK_GC = occursin("stock", GC.gc_active_impl())

# These used to be in build_h.jl and are retained for backwards compatibility.
# NOTE: keep in sync with `libblastrampoline_jll.libblastrampoline`.
const libblas_name = "libblastrampoline" * (Sys.iswindows() ? "-5" : "")
const liblapack_name = libblas_name

# Concurrency (part 2)
# Note that `atomics.jl` here should be deprecated
Core.eval(Threads, :(include("atomics.jl")))
include("channels.jl")
include("partr.jl")
include("task.jl")
include("threads_overloads.jl")
include("weakkeydict.jl")

# ScopedValues
include("scopedvalues.jl")

# metaprogramming
include("meta.jl")

# Logging
include("logging/logging.jl")
using .CoreLogging

include("env.jl")

# functions defined in Random
function rand end
function randn end

# I/O
include("libuv.jl")
include("asyncevent.jl")
include("iostream.jl")
include("stream.jl")
include("filesystem.jl")
using .Filesystem
include("cmd.jl")
include("process.jl")
include("terminfo.jl")
include("secretbuffer.jl")

# core math functions
include("floatfuncs.jl")
include("math.jl")
using .Math
const (√)=sqrt
const (∛)=cbrt
const (∜)=fourthroot

# now switch to a simple, race-y TLS, relative include for the rest of Base
delete_method(which(include, (Module, String)))
let SOURCE_PATH = ""
    global function include(mod::Module, path::String)
        prev = SOURCE_PATH::String
        path = normpath(joinpath(dirname(prev), path))
        Core.println(path)
        ccall(:jl_uv_flush, Nothing, (Ptr{Nothing},), Core.io_pointer(Core.stdout))
        push!(_included_files, (mod, abspath(path)))
        SOURCE_PATH = path
        result = Core.include(mod, path)
        SOURCE_PATH = prev
        return result
    end
end

# reduction along dims
include("reducedim.jl")  # macros in this file rely on string.jl
include("accumulate.jl")

include("permuteddimsarray.jl")
using .PermutedDimsArrays

# Combinatorics
include("sort.jl")
using .Sort

# Fast math
include("fastmath.jl")
using .FastMath

function deepcopy_internal end

# enums
include("Enums.jl")
using .Enums

# BigInts
include("gmp.jl")
using .GMP

# float printing: requires BigInt
include("ryu/Ryu.jl")
using .Ryu

# BigFloats
include("mpfr.jl")
using .MPFR

include("combinatorics.jl")

# irrational mathematical constants
include("irrationals.jl")
include("mathconstants.jl")
using .MathConstants: ℯ, π, pi

# experimental API's
include("experimental.jl")

# utilities
include("deepcopy.jl")
include("download.jl")
include("summarysize.jl")
include("errorshow.jl")
include("util.jl")

include("initdefs.jl")
Filesystem.__postinit__()

# worker threads
include("threadcall.jl")

# code loading
include("uuid.jl")
include("pkgid.jl")
include("toml_parser.jl")
include("linking.jl")
include("staticdata.jl")
include("loading.jl")

# BinaryPlatforms, used by Artifacts.  Needs `Sort`.
include("binaryplatforms.jl")

# misc useful functions & macros
include("timing.jl")
include("client.jl")
include("asyncmap.jl")

# deprecated functions
include("deprecated.jl")
#
# Some additional basic documentation
include("docs/basedocs.jl")

# Documentation -- should always be included last in sysimg.
include("docs/Docs.jl")
using .Docs
Docs.loaddocs(CoreDocs.DOCS)
@eval CoreDocs DOCS = DocLinkedList()

include("precompilation.jl")

# finally, now make `include` point to the full version
for m in methods(include)
    delete_method(m)
end
for m in methods(IncludeInto(Base))
    delete_method(m)
end

# This method is here only to be overwritten during the test suite to test
# various sysimg related invalidation scenarios.
a_method_to_overwrite_in_test() = inferencebarrier(1)

# These functions are duplicated in client.jl/include(::String) for
# nicer stacktraces. Modifications here have to be backported there
@noinline include(mod::Module, _path::AbstractString) = _include(identity, mod, _path)
@noinline include(mapexpr::Function, mod::Module, _path::AbstractString) = _include(mapexpr, mod, _path)
(this::IncludeInto)(fname::AbstractString) = include(identity, this.m, fname)
(this::IncludeInto)(mapexpr::Function, fname::AbstractString) = include(mapexpr, this.m, fname)

# Compatibility with when Compiler was in Core
@eval Core const Compiler = Main.Base.Compiler
@eval Compiler const fl_parse = Core.Main.Base.fl_parse

# External libraries vendored into Base
Core.println("JuliaSyntax/src/JuliaSyntax.jl")
include(@__MODULE__, string(BUILDROOT, "JuliaSyntax/src/JuliaSyntax.jl")) # include($BUILDROOT/base/JuliaSyntax/JuliaSyntax.jl)

end_base_include = time_ns()

const _sysimage_modules = PkgId[]
in_sysimage(pkgid::PkgId) = pkgid in _sysimage_modules

if is_primary_base_module

# Profiling helper
# triggers printing the report and (optionally) saving a heap snapshot after a SIGINFO/SIGUSR1 profile request
# Needs to be in Base because Profile is no longer loaded on boot
function profile_printing_listener(cond::Base.AsyncCondition)
    profile = nothing
    try
        while _trywait(cond)
            profile = @something(profile, require_stdlib(PkgId(UUID("9abbd945-dff8-562f-b5e8-e1ebf5ef1b79"), "Profile")))::Module
            invokelatest(profile.peek_report[])
            if Base.get_bool_env("JULIA_PROFILE_PEEK_HEAP_SNAPSHOT", false) === true
                println(stderr, "Saving heap snapshot...")
                fname = invokelatest(profile.take_heap_snapshot)
                println(stderr, "Heap snapshot saved to `$(fname)`")
            end
        end
    catch ex
        if !isa(ex, InterruptException)
            @error "Profile printing listener crashed" exception=ex,catch_backtrace()
        end
    end
    nothing
end

function start_profile_listener()
    cond = Base.AsyncCondition()
    Base.uv_unref(cond.handle)
    t = errormonitor(Threads.@spawn(profile_printing_listener(cond)))
    atexit() do
        # destroy this callback when exiting
        ccall(:jl_set_peek_cond, Cvoid, (Ptr{Cvoid},), C_NULL)
        # this will prompt any ongoing or pending event to flush also
        close(cond)
        # error-propagation is not needed, since the errormonitor will handle printing that better
        t === current_task() || _wait(t)
    end
    finalizer(cond) do c
        # if something goes south, still make sure we aren't keeping a reference in C to this
        ccall(:jl_set_peek_cond, Cvoid, (Ptr{Cvoid},), C_NULL)
    end
    ccall(:jl_set_peek_cond, Cvoid, (Ptr{Cvoid},), cond.handle)
end

function __init__()
    # Base library init
    global _atexit_hooks_finished = false
    Filesystem.__postinit__()
    reinit_stdio()
    Multimedia.reinit_displays() # since Multimedia.displays uses stdout as fallback
    # initialize loading
    init_depot_path()
    init_load_path()
    init_active_project()
    append!(empty!(_sysimage_modules), keys(loaded_modules))
    empty!(loaded_precompiles) # If we load a packageimage when building the image this might not be empty
    for mod in loaded_modules_order
        push!(get!(Vector{Module}, loaded_precompiles, PkgId(mod)), mod)
    end
    if haskey(ENV, "JULIA_MAX_NUM_PRECOMPILE_FILES")
        MAX_NUM_PRECOMPILE_FILES[] = parse(Int, ENV["JULIA_MAX_NUM_PRECOMPILE_FILES"])
    end
    # Profiling helper
    @static if !Sys.iswindows()
        # triggering a profile via signals is not implemented on windows
        start_profile_listener()
    end
    _require_world_age[] = get_world_counter()
    # Prevent spawned Julia process from getting stuck waiting on Tracy to connect.
    delete!(ENV, "JULIA_WAIT_FOR_TRACY")
    if get_bool_env("JULIA_USE_FLISP_PARSER", false) === false
        JuliaSyntax.enable_in_core!()
    end

    CoreLogging.global_logger(CoreLogging.ConsoleLogger())
    nothing
end

# enable threads support
@eval PCRE PCRE_COMPILE_LOCK = Threads.SpinLock()

# Record dependency information for files belonging to the Compiler, so that
# we know whether the .ji can just give the Base copy or not.
# TODO: We may want to do this earlier to avoid TOCTOU issues.
const _compiler_require_dependencies = Any[]
@Core.latestworld
for i = 1:length(_included_files)
    isassigned(_included_files, i) || continue
    (mod, file) = _included_files[i]
    if mod === Compiler || parentmodule(mod) === Compiler || endswith(file, "/Compiler.jl")
        _include_dependency!(_compiler_require_dependencies, true, mod, file, true, false)
    end
end
# Make relative to DATAROOTDIR to allow relocation
let basedir = joinpath(Sys.BINDIR, DATAROOTDIR)
for i = 1:length(_compiler_require_dependencies)
    tup = _compiler_require_dependencies[i]
    _compiler_require_dependencies[i] = (tup[1], relpath(tup[2], basedir), tup[3:end]...)
end
end
@assert length(_compiler_require_dependencies) >= 15

end

# Ensure this file is also tracked
@assert !isassigned(_included_files, 1)
_included_files[1] = (parentmodule(Base), abspath(@__FILE__))
