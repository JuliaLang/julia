# This file is a part of Julia. License is MIT: https://julialang.org/license

baremodule Base

using Core.Intrinsics, Core.IR

# to start, we're going to use a very simple definition of `include`
# that doesn't require any function (except what we can get from the `Core` top-module)
const _included_files = Array{Tuple{Module,String},1}()
function include(mod::Module, path::String)
    ccall(:jl_array_grow_end, Cvoid, (Any, UInt), _included_files, UInt(1))
    Core.arrayset(true, _included_files, (mod, ccall(:jl_prepend_cwd, Any, (Any,), path)), arraylen(_included_files))
    Core.println(path)
    ccall(:jl_uv_flush, Nothing, (Ptr{Nothing},), Core.io_pointer(Core.stdout))
    Core.include(mod, path)
end
include(path::String) = include(Base, path)

# from now on, this is now a top-module for resolving syntax
const is_primary_base_module = ccall(:jl_module_parent, Ref{Module}, (Any,), Base) === Core.Main
ccall(:jl_set_istopmod, Cvoid, (Any, Bool), Base, is_primary_base_module)

# Try to help prevent users from shooting them-selves in the foot
# with ambiguities by defining a few common and critical operations
# (and these don't need the extra convert code)
getproperty(x::Module, f::Symbol) = getfield(x, f)
setproperty!(x::Module, f::Symbol, v) = setfield!(x, f, v)
getproperty(x::Type, f::Symbol) = getfield(x, f)
setproperty!(x::Type, f::Symbol, v) = setfield!(x, f, v)
getproperty(x::Tuple, f::Int) = getfield(x, f)
setproperty!(x::Tuple, f::Int, v) = setfield!(x, f, v) # to get a decent error

getproperty(x, f::Symbol) = getfield(x, f)
setproperty!(x, f::Symbol, v) = setfield!(x, f, convert(fieldtype(typeof(x), f), v))

include("coreio.jl")

eval(x) = Core.eval(Base, x)
eval(m::Module, x) = Core.eval(m, x)

# init core docsystem
import Core: @doc, @__doc__, WrappedException, @int128_str, @uint128_str, @big_str, @cmd
if isdefined(Core, :Compiler)
    import Core.Compiler.CoreDocs
    Core.atdoc!(CoreDocs.docm)
end

include("exports.jl")

if false
    # simple print definitions for debugging. enable these if something
    # goes wrong during bootstrap before printing code is available.
    # otherwise, they just just eventually get (noisily) overwritten later
    global show, print, println
    show(io::IO, x) = Core.show(io, x)
    print(io::IO, a...) = Core.print(io, a...)
    println(io::IO, x...) = Core.println(io, x...)
end

"""
    time_ns()

Get the time in nanoseconds. The time corresponding to 0 is undefined, and wraps every 5.8 years.
"""
time_ns() = ccall(:jl_hrtime, UInt64, ())

start_base_include = time_ns()

## Load essential files and libraries
include("essentials.jl")
include("ctypes.jl")
include("gcutils.jl")
include("generator.jl")
include("reflection.jl")
include("options.jl")

# core operations & types
include("promotion.jl")
include("tuple.jl")
include("expr.jl")
include("pair.jl")
include("traits.jl")
include("range.jl")
include("error.jl")

# core numeric operations & types
include("bool.jl")
include("number.jl")
include("int.jl")
include("operators.jl")
include("pointer.jl")
include("refvalue.jl")
include("refpointer.jl")
include("checked.jl")
using .Checked

# array structures
include("indices.jl")
include("array.jl")
include("abstractarray.jl")
include("subarray.jl")
include("views.jl")
include("baseext.jl")

include("ntuple.jl")

include("abstractdict.jl")
include("iddict.jl")
include("idset.jl")

include("iterators.jl")
using .Iterators: zip, enumerate, only
using .Iterators: Flatten, Filter, product  # for generators

include("namedtuple.jl")

# numeric operations
include("hashing.jl")
include("rounding.jl")
using .Rounding
include("div.jl")
include("float.jl")
include("twiceprecision.jl")
include("complex.jl")
include("rational.jl")
include("multinverses.jl")
using .MultiplicativeInverses
include("abstractarraymath.jl")
include("arraymath.jl")

# SIMD loops
@pure sizeof(s::String) = Core.sizeof(s)  # needed by gensym as called from simdloop
include("simdloop.jl")
using .SimdLoop

# map-reduce operators
include("reduce.jl")

## core structures
include("reshapedarray.jl")
include("reinterpretarray.jl")
include("bitarray.jl")
include("bitset.jl")

if !isdefined(Core, :Compiler)
    include("docs/core.jl")
    Core.atdoc!(CoreDocs.docm)
end

include("multimedia.jl")
using .Multimedia

# Some type
include("some.jl")

include("dict.jl")
include("abstractset.jl")
include("set.jl")

include("char.jl")
include("strings/basic.jl")
include("strings/string.jl")
include("strings/substring.jl")

# For OS specific stuff
include(string((length(Core.ARGS)>=2 ? Core.ARGS[2] : ""), "build_h.jl"))     # include($BUILDROOT/base/build_h.jl)
include(string((length(Core.ARGS)>=2 ? Core.ARGS[2] : ""), "version_git.jl")) # include($BUILDROOT/base/version_git.jl)

# Initialize DL_LOAD_PATH as early as possible.  We are defining things here in
# a slightly more verbose fashion than usual, because we're running so early.
const DL_LOAD_PATH = String[]
let os = ccall(:jl_get_UNAME, Any, ())
    if os === :Darwin || os === :Apple
        if Base.DARWIN_FRAMEWORK
            push!(DL_LOAD_PATH, "@loader_path/Frameworks")
        else
            push!(DL_LOAD_PATH, "@loader_path/julia")
        end
        push!(DL_LOAD_PATH, "@loader_path")
    end
end

include("osutils.jl")
include("c.jl")

# Core I/O
include("io.jl")
include("iobuffer.jl")

# strings & printing
include("intfuncs.jl")
include("strings/strings.jl")
include("parse.jl")
include("shell.jl")
include("regex.jl")
include("show.jl")
include("arrayshow.jl")
include("methodshow.jl")

# multidimensional arrays
include("cartesian.jl")
using .Cartesian
include("multidimensional.jl")
include("permuteddimsarray.jl")
using .PermutedDimsArrays

include("broadcast.jl")
using .Broadcast
using .Broadcast: broadcasted, broadcasted_kwsyntax, materialize, materialize!

# missing values
include("missing.jl")

# version
include("version.jl")

# system & environment
include("sysinfo.jl")
include("libc.jl")
using .Libc: getpid, gethostname, time

include("env.jl")

# Concurrency
include("linked_list.jl")
include("condition.jl")
include("threads.jl")
include("lock.jl")
include("channels.jl")
include("task.jl")
include("threads_overloads.jl")
include("weakkeydict.jl")

# Logging
include("logging.jl")
using .CoreLogging

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
include("ttyhascolor.jl")
include("grisu/grisu.jl")
include("secretbuffer.jl")

# core math functions
include("floatfuncs.jl")
include("math.jl")
using .Math
const (√)=sqrt
const (∛)=cbrt

# now switch to a simple, race-y TLS, relative include for the rest of Base
delete_method(which(include, (Module, String)))
let SOURCE_PATH = ""
    global function include(mod::Module, path::String)
        prev = SOURCE_PATH
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
include("reducedim.jl")  # macros in this file relies on string.jl
include("accumulate.jl")

# basic data structures
include("ordering.jl")
using .Order

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

# more hashing definitions
include("hashing2.jl")

# irrational mathematical constants
include("irrationals.jl")
include("mathconstants.jl")
using .MathConstants: ℯ, π, pi

# metaprogramming
include("meta.jl")

# utilities
include("deepcopy.jl")
include("download.jl")
include("summarysize.jl")
include("errorshow.jl")

# Stack frames and traces
include("stacktraces.jl")
using .StackTraces

include("initdefs.jl")

# worker threads
include("threadcall.jl")

# code loading
include("uuid.jl")
include("loading.jl")

# misc useful functions & macros
include("timing.jl")
include("util.jl")

include("asyncmap.jl")

# experimental API's
include("experimental.jl")

# deprecated functions
include("deprecated.jl")

# Some basic documentation
include("docs/basedocs.jl")

include("client.jl")

# Documentation -- should always be included last in sysimg.
include("docs/Docs.jl")
using .Docs
if isdefined(Core, :Compiler) && is_primary_base_module
    Docs.loaddocs(Core.Compiler.CoreDocs.DOCS)
end

# finally, now make `include` point to the full version
for m in methods(include)
    delete_method(m)
end
# These functions are duplicated in client.jl/include(::String) for
# nicer stacktraces. Modifications here have to be backported there
include(mod::Module, _path::AbstractString) = _include(identity, mod, _path)
include(mapexpr::Function, mod::Module, _path::AbstractString) = _include(mapexpr, mod, _path)

end_base_include = time_ns()

if is_primary_base_module
function __init__()
    # try to ensuremake sure OpenBLAS does not set CPU affinity (#1070, #9639)
    if !haskey(ENV, "OPENBLAS_MAIN_FREE") && !haskey(ENV, "GOTOBLAS_MAIN_FREE")
        ENV["OPENBLAS_MAIN_FREE"] = "1"
    end
    # And try to prevent openblas from starting too many threads, unless/until specifically requested
    if !haskey(ENV, "OPENBLAS_NUM_THREADS") && !haskey(ENV, "OMP_NUM_THREADS")
        cpu_threads = Sys.CPU_THREADS::Int
        if cpu_threads > 8 # always at most 8
            ENV["OPENBLAS_NUM_THREADS"] = "8"
        elseif haskey(ENV, "JULIA_CPU_THREADS") # or exactly as specified
            ENV["OPENBLAS_NUM_THREADS"] = cpu_threads
        end # otherwise, trust that openblas will pick CPU_THREADS anyways, without any intervention
    end
    # for the few uses of Libc.rand in Base:
    Libc.srand()
    # Base library init
    reinit_stdio()
    Multimedia.reinit_displays() # since Multimedia.displays uses stdout as fallback
    # initialize loading
    init_depot_path()
    init_load_path()
    nothing
end


end

const tot_time_stdlib = RefValue(0.0)

end # baremodule Base
