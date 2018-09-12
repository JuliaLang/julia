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

VecElement{T}(arg) where {T} = VecElement{T}(convert(T, arg))
convert(::Type{T}, arg)  where {T<:VecElement} = T(arg)
convert(::Type{T}, arg::T) where {T<:VecElement} = arg

# init core docsystem
import Core: @doc, @__doc__, WrappedException
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
include("pair.jl")
include("traits.jl")
include("range.jl")
include("expr.jl")
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

# vararg Symbol constructor
Symbol(x...) = Symbol(string(x...))

# array structures
include("indices.jl")
include("array.jl")
include("abstractarray.jl")
include("subarray.jl")
include("views.jl")

# ## dims-type-converting Array constructors for convenience
# type and dimensionality specified, accepting dims as series of Integers
Vector{T}(::UndefInitializer, m::Integer) where {T} = Vector{T}(undef, Int(m))
Matrix{T}(::UndefInitializer, m::Integer, n::Integer) where {T} = Matrix{T}(undef, Int(m), Int(n))
Array{T,N}(::UndefInitializer, d::Vararg{Integer,N}) where {T,N} = Array{T,N}(undef, convert(Tuple{Vararg{Int}}, d))
# type but not dimensionality specified, accepting dims as series of Integers
Array{T}(::UndefInitializer, m::Integer) where {T} = Array{T,1}(undef, Int(m))
Array{T}(::UndefInitializer, m::Integer, n::Integer) where {T} = Array{T,2}(undef, Int(m), Int(n))
Array{T}(::UndefInitializer, m::Integer, n::Integer, o::Integer) where {T} = Array{T,3}(undef, Int(m), Int(n), Int(o))
Array{T}(::UndefInitializer, d::Integer...) where {T} = Array{T}(undef, convert(Tuple{Vararg{Int}}, d))
# dimensionality but not type specified, accepting dims as series of Integers
Vector(::UndefInitializer, m::Integer) = Vector{Any}(undef, Int(m))
Matrix(::UndefInitializer, m::Integer, n::Integer) = Matrix{Any}(undef, Int(m), Int(n))
# Dimensions as a single tuple
Array{T}(::UndefInitializer, d::NTuple{N,Integer}) where {T,N} = Array{T,N}(undef, convert(Tuple{Vararg{Int}}, d))
Array{T,N}(::UndefInitializer, d::NTuple{N,Integer}) where {T,N} = Array{T,N}(undef, convert(Tuple{Vararg{Int}}, d))
# empty vector constructor
Vector() = Vector{Any}(undef, 0)

# Array constructors for nothing and missing
# type and dimensionality specified
Array{T,N}(::Nothing, d...) where {T,N} = fill!(Array{T,N}(undef, d...), nothing)
Array{T,N}(::Missing, d...) where {T,N} = fill!(Array{T,N}(undef, d...), missing)
# type but not dimensionality specified
Array{T}(::Nothing, d...) where {T} = fill!(Array{T}(undef, d...), nothing)
Array{T}(::Missing, d...) where {T} = fill!(Array{T}(undef, d...), missing)

include("abstractdict.jl")

include("iterators.jl")
using .Iterators: zip, enumerate
using .Iterators: Flatten, product  # for generators

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

# define MIME"foo/bar" early so that we can overload 3-arg show
struct MIME{mime} end
macro MIME_str(s)
    :(MIME{$(Expr(:quote, Symbol(s)))})
end
# fallback text/plain representation of any type:
show(io::IO, ::MIME"text/plain", x) = show(io, x)

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

# Some type
include("some.jl")

include("dict.jl")
include("abstractset.jl")
include("set.jl")

include("char.jl")
include("strings/basic.jl")
include("strings/string.jl")
include("strings/substring.jl")

# Definition of StridedArray
StridedFastContiguousSubArray{T,N,A<:DenseArray} = FastContiguousSubArray{T,N,A}
StridedReinterpretArray{T,N,A<:Union{DenseArray,StridedFastContiguousSubArray}} = ReinterpretArray{T,N,S,A} where S
StridedReshapedArray{T,N,A<:Union{DenseArray,StridedFastContiguousSubArray,StridedReinterpretArray}} = ReshapedArray{T,N,A}
StridedSubArray{T,N,A<:Union{DenseArray,StridedReshapedArray,StridedReinterpretArray},
    I<:Tuple{Vararg{Union{RangeIndex, AbstractCartesianIndex}}}} = SubArray{T,N,A,I}
StridedArray{T,N} = Union{DenseArray{T,N}, StridedSubArray{T,N}, StridedReshapedArray{T,N}, StridedReinterpretArray{T,N}}
StridedVector{T} = Union{DenseArray{T,1}, StridedSubArray{T,1}, StridedReshapedArray{T,1}, StridedReinterpretArray{T,1}}
StridedMatrix{T} = Union{DenseArray{T,2}, StridedSubArray{T,2}, StridedReshapedArray{T,2}, StridedReinterpretArray{T,2}}
StridedVecOrMat{T} = Union{StridedVector{T}, StridedMatrix{T}}

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

# multidimensional arrays
include("cartesian.jl")
using .Cartesian
include("multidimensional.jl")
include("permuteddimsarray.jl")
using .PermutedDimsArrays

include("broadcast.jl")
using .Broadcast

# define the real ntuple functions
@inline function ntuple(f::F, ::Val{N}) where {F,N}
    N::Int
    (N >= 0) || throw(ArgumentError(string("tuple length should be ≥0, got ", N)))
    if @generated
        quote
            @nexprs $N i -> t_i = f(i)
            @ncall $N tuple t
        end
    else
        Tuple(f(i) for i = 1:N)
    end
end
@inline function fill_to_length(t::Tuple, val, ::Val{N}) where {N}
    M = length(t)
    M > N && throw(ArgumentError("input tuple of length $M, requested $N"))
    if @generated
        quote
            (t..., $(fill(:val, N-length(t.parameters))...))
        end
    else
        (t..., fill(val, N-M)...)
    end
end

# missing values
include("missing.jl")

# version
include("version.jl")

# system & environment
include("sysinfo.jl")
include("libc.jl")
using .Libc: getpid, gethostname, time

const DL_LOAD_PATH = String[]
if Sys.isapple()
    push!(DL_LOAD_PATH, "@loader_path/julia")
    push!(DL_LOAD_PATH, "@loader_path")
end

include("env.jl")

# Scheduling
include("libuv.jl")
include("event.jl")
include("task.jl")
include("lock.jl")
include("threads.jl")
include("weakkeydict.jl")

# Logging
include("logging.jl")
using .CoreLogging

# functions defined in Random
function rand end
function randn end

# I/O
include("stream.jl")
include("filesystem.jl")
using .Filesystem
include("process.jl")
include("grisu/grisu.jl")
include("methodshow.jl")
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

for T in [Signed, Integer, BigInt, Float32, Float64, Real, Complex, Rational]
    @eval flipsign(x::$T, ::Unsigned) = +x
    @eval copysign(x::$T, ::Unsigned) = +x
end

include("mpfr.jl")
using .MPFR
big(n::Integer) = convert(BigInt,n)
big(x::AbstractFloat) = convert(BigFloat,x)
big(q::Rational) = big(numerator(q))//big(denominator(q))

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
include("threadcall.jl")

# code loading
include("uuid.jl")
include("loading.jl")

# misc useful functions & macros
include("util.jl")

creating_sysimg = true
# set up depot & load paths to be able to find stdlib packages
init_depot_path()
init_load_path()

include("asyncmap.jl")

include("multimedia.jl")
using .Multimedia

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

using .Base

# Ensure this file is also tracked
pushfirst!(Base._included_files, (@__MODULE__, joinpath(@__DIR__, "sysimg.jl")))

if Base.is_primary_base_module
# load some stdlib packages but don't put their names in Main
let
    # Stdlibs manually sorted in top down order
    stdlibs = [
            # No deps
            :Base64,
            :CRC32c,
            :SHA,
            :FileWatching,
            :Unicode,
            :Mmap,
            :Serialization,
            :Libdl,
            :Markdown,
            :LibGit2,
            :Logging,
            :Sockets,
            :Printf,
            :Profile,
            :Dates,
            :DelimitedFiles,
            :Random,
            :UUIDs,
            :Future,
            :LinearAlgebra,
            :SparseArrays,
            :SuiteSparse,
            :Distributed,
            :SharedArrays,
            :Pkg,
            :Test,
            :REPL,
            :Statistics,
        ]

    maxlen = maximum(textwidth.(string.(stdlibs)))

    print_time = (mod, t) -> (print(rpad(string(mod) * "  ", maxlen + 3, "─")); Base.time_print(t * 10^9); println())
    print_time(Base, (Base.end_base_include - Base.start_base_include) * 10^(-9))

    Base._track_dependencies[] = true
    Base.tot_time_stdlib[] = @elapsed for stdlib in stdlibs
        tt = @elapsed Base.require(Base, stdlib)
        print_time(stdlib, tt)
    end
    for dep in Base._require_dependencies
        dep[3] == 0.0 && continue
        push!(Base._included_files, dep[1:2])
    end
    empty!(Base._require_dependencies)
    Base._track_dependencies[] = false

    print_time("Stdlibs total", Base.tot_time_stdlib[])
end
end

# Clear global state
empty!(Core.ARGS)
empty!(Base.ARGS)
empty!(LOAD_PATH)
@eval Base creating_sysimg = false
Base.init_load_path() # want to be able to find external packages in userimg.jl

let
tot_time_userimg = @elapsed (Base.isfile("userimg.jl") && Base.include(Main, "userimg.jl"))


tot_time_base = (Base.end_base_include - Base.start_base_include) * 10.0^(-9)
tot_time = tot_time_base + Base.tot_time_stdlib[] + tot_time_userimg

println("Sysimage built. Summary:")
print("Total ─────── "); Base.time_print(tot_time               * 10^9); print(" \n");
print("Base: ─────── "); Base.time_print(tot_time_base          * 10^9); print(" "); show(IOContext(stdout, :compact=>true), (tot_time_base          / tot_time) * 100); println("%")
print("Stdlibs: ──── "); Base.time_print(Base.tot_time_stdlib[] * 10^9); print(" "); show(IOContext(stdout, :compact=>true), (Base.tot_time_stdlib[] / tot_time) * 100); println("%")
if isfile("userimg.jl")
print("Userimg: ──── "); Base.time_print(tot_time_userimg       * 10^9); print(" "); show(IOContext(stdout, :compact=>true), (tot_time_userimg       / tot_time) * 100); println("%")
end
end

empty!(LOAD_PATH)
empty!(DEPOT_PATH)
