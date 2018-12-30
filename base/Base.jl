# This file is a part of Julia. License is MIT: https://julialang.org/license

baremodule Base

using Core.Intrinsics, Core.IR

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

getproperty(Core.@nospecialize(x), f::Symbol) = getfield(x, f)
setproperty!(x, f::Symbol, v) = setfield!(x, f, convert(fieldtype(typeof(x), f), v))

function include_relative end
function include(mod::Module, path::AbstractString)
    local result
    if INCLUDE_STATE === 1
        result = _include1(mod, path)
    elseif INCLUDE_STATE === 2
        result = _include(mod, path)
    elseif INCLUDE_STATE === 3
        result = include_relative(mod, path)
    end
    result
end
function include(path::AbstractString)
    local result
    if INCLUDE_STATE === 1
        result = _include1(Base, path)
    elseif INCLUDE_STATE === 2
        result = _include(Base, path)
    else
        # to help users avoid error (accidentally evaluating into Base), this is not allowed
        error("Base.include(string) is discontinued, use `include(fname)` or `Base.include(@__MODULE__, fname)` instead.")
    end
    result
end
const _included_files = Array{Tuple{Module,String},1}()
function _include1(mod::Module, path)
    Core.Compiler.push!(_included_files, (mod, ccall(:jl_prepend_cwd, Any, (Any,), path)))
    Core.include(mod, path)
end
let SOURCE_PATH = ""
    # simple, race-y TLS, relative include
    global _include
    function _include(mod::Module, path)
        prev = SOURCE_PATH
        path = normpath(joinpath(dirname(prev), path))
        push!(_included_files, (mod, abspath(path)))
        SOURCE_PATH = path
        result = Core.include(mod, path)
        SOURCE_PATH = prev
        result
    end
end
INCLUDE_STATE = 1 # include = Core.include

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

include("iterators.jl")
using .Iterators: zip, enumerate
using .Iterators: Flatten, Filter, product  # for generators

include("namedtuple.jl")

# numeric operations
include("hashing.jl")
include("rounding.jl")
using .Rounding
include("float.jl")
include("twiceprecision.jl")
include("complex.jl")
include("rational.jl")
include("multinverses.jl")
using .MultiplicativeInverses
include("abstractarraymath.jl")
include("arraymath.jl")

# SIMD loops
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

include("osutils.jl")
include("c.jl")

# Core I/O
include("io.jl")
include("iostream.jl")
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
using .Libc: getpid, gethostname, time, RawFD

const DL_LOAD_PATH = String[]
if Sys.isapple()
    if Base.DARWIN_FRAMEWORK
        push!(DL_LOAD_PATH, "@loader_path/Frameworks")
    else
        push!(DL_LOAD_PATH, "@loader_path/julia")
    end
    push!(DL_LOAD_PATH, "@loader_path")
end

include("env.jl")

# Scheduling
include("linked_list.jl")
include("condition.jl")
include("threads.jl")
include("lock.jl")
include("task.jl")
include("weakkeydict.jl")

# Logging
include("logging.jl")
using .CoreLogging

# functions defined in Random
function rand end
function randn end

# I/O
include("asyncevent.jl")
include("filesystem.jl")
using .Filesystem
include("cmd.jl")
if !DISABLE_LIBUV
include("uvevent.jl")
include("libuv.jl")
include("stream.jl")
include("process.jl")
end
include("grisu/grisu.jl")
include("secretbuffer.jl")

# core math functions
include("floatfuncs.jl")
include("math.jl")
using .Math
const (√)=sqrt
const (∛)=cbrt

INCLUDE_STATE = 2 # include = _include (from lines above)

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

# BigInts and BigFloats
include("gmp.jl")
using .GMP

include("mpfr.jl")
using .MPFR

include("combinatorics.jl")

# more hashing definitions
include("hashing2.jl")

# irrational mathematical constants
include("irrationals.jl")
include("mathconstants.jl")
using .MathConstants: ℯ, π, pi

# (s)printf macros
include("printf.jl")
# import .Printf

# metaprogramming
include("meta.jl")

# concurrency and parallelism
include("channels.jl")

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
if !DISABLE_LIBUV
include("threadcall.jl")
end

# code loading
include("uuid.jl")
include("loading.jl")

# misc useful functions & macros
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

INCLUDE_STATE = 3 # include = include_relative
end

const tot_time_stdlib = RefValue(0.0)

end # baremodule Base
