# This file is a part of Julia. License is MIT: http://julialang.org/license

baremodule Base

using Core.Intrinsics
ccall(:jl_set_istopmod, Void, (Bool,), true)
function include(path::AbstractString)
    local result
    if INCLUDE_STATE === 1
        result = Core.include(path)
    elseif INCLUDE_STATE === 2
        result = _include(path)
    elseif INCLUDE_STATE === 3
        result = include_from_node1(path)
    end
    result
end
INCLUDE_STATE = 1 # include = Core.include

include("coreio.jl")

eval(x) = Core.eval(Base, x)
eval(m, x) = Core.eval(m, x)
(::Type{T}){T}(arg) = convert(T, arg)::T
(::Type{VecElement{T}}){T}(arg) = VecElement{T}(convert(T, arg))
convert{T<:VecElement}(::Type{T}, arg) = T(arg)
convert{T<:VecElement}(::Type{T}, arg::T) = arg

# init core docsystem
import Core: @doc, @__doc__, @doc_str
if isdefined(Core, :Inference)
    import Core.Inference.CoreDocs
    Core.atdoc!(CoreDocs.docm)
end

include("exports.jl")

if false
    # simple print definitions for debugging. enable these if something
    # goes wrong during bootstrap before printing code is available.
    # otherwise, they just just eventually get (noisily) overwritten later
    global show, print, println
    show(io::IO, x::ANY) = Core.show(io, x)
    print(io::IO, a::ANY...) = Core.print(io, a...)
    println(io::IO, x::ANY...) = Core.println(io, x...)
end

## Load essential files and libraries
include("essentials.jl")
include("ctypes.jl")
include("base.jl")
include("generator.jl")
include("reflection.jl")
include("options.jl")

# core operations & types
include("promotion.jl")
include("tuple.jl")
include("traits.jl")
include("range.jl")
include("twiceprecision.jl")
include("expr.jl")
include("error.jl")

# core numeric operations & types
include("bool.jl")
include("number.jl")
include("int.jl")
include("operators.jl")
include("pointer.jl")
include("refpointer.jl")
include("checked.jl")
importall .Checked

# vararg Symbol constructor
Symbol(x...) = Symbol(string(x...))

# Define the broadcast function, which is mostly implemented in
# broadcast.jl, so that we can overload broadcast methods for
# specific array types etc.
#  --Here, just define fallback routines for broadcasting with no arguments
broadcast(f) = f()
broadcast!(f, X::AbstractArray) = (@inbounds for I in eachindex(X); X[I] = f(); end; X)

# array structures
include("array.jl")
include("abstractarray.jl")
include("subarray.jl")

# Array convenience converting constructors
(::Type{Array{T}}){T}(m::Integer) = Array{T,1}(Int(m))
(::Type{Array{T}}){T}(m::Integer, n::Integer) = Array{T,2}(Int(m), Int(n))
(::Type{Array{T}}){T}(m::Integer, n::Integer, o::Integer) = Array{T,3}(Int(m), Int(n), Int(o))
(::Type{Array{T}}){T}(d::Integer...) = Array{T}(convert(Tuple{Vararg{Int}}, d))

(::Type{Vector})() = Array{Any,1}(0)
(::Type{Vector{T}}){T}(m::Integer) = Array{T,1}(Int(m))
(::Type{Vector})(m::Integer) = Array{Any,1}(Int(m))
(::Type{Matrix{T}}){T}(m::Integer, n::Integer) = Matrix{T}(Int(m), Int(n))
(::Type{Matrix})(m::Integer, n::Integer) = Matrix{Any}(Int(m), Int(n))

# numeric operations
include("hashing.jl")
include("rounding.jl")
importall .Rounding
include("float.jl")
include("complex.jl")
include("rational.jl")
include("multinverses.jl")
using .MultiplicativeInverses
include("abstractarraymath.jl")
include("arraymath.jl")

# define MIME"foo/bar" early so that we can overload 3-arg show
immutable MIME{mime} end
macro MIME_str(s)
    :(MIME{$(Expr(:quote, Symbol(s)))})
end

include("char.jl")
include("strings/string.jl")

# SIMD loops
include("simdloop.jl")
importall .SimdLoop

# map-reduce operators
include("reduce.jl")

## core structures
include("reshapedarray.jl")
include("bitarray.jl")
include("intset.jl")
include("associative.jl")
include("dict.jl")
include("set.jl")
include("iterators.jl")
using .Iterators: zip, enumerate
using .Iterators: Flatten, product  # for generators

# Definition of StridedArray
typealias StridedReshapedArray{T,N,A<:DenseArray} ReshapedArray{T,N,A}
typealias StridedArray{T,N,A<:Union{DenseArray,StridedReshapedArray},I<:Tuple{Vararg{Union{RangeIndex, AbstractCartesianIndex}}}} Union{DenseArray{T,N}, SubArray{T,N,A,I}, StridedReshapedArray{T,N}}
typealias StridedVector{T,A<:Union{DenseArray,StridedReshapedArray},I<:Tuple{Vararg{Union{RangeIndex, AbstractCartesianIndex}}}}  Union{DenseArray{T,1}, SubArray{T,1,A,I}, StridedReshapedArray{T,1}}
typealias StridedMatrix{T,A<:Union{DenseArray,StridedReshapedArray},I<:Tuple{Vararg{Union{RangeIndex, AbstractCartesianIndex}}}}  Union{DenseArray{T,2}, SubArray{T,2,A,I}, StridedReshapedArray{T,2}}
typealias StridedVecOrMat{T} Union{StridedVector{T}, StridedMatrix{T}}

# For OS specific stuff
include(string((length(Core.ARGS)>=2 ? Core.ARGS[2] : ""), "build_h.jl"))     # include($BUILDROOT/base/build_h.jl)
include(string((length(Core.ARGS)>=2 ? Core.ARGS[2] : ""), "version_git.jl")) # include($BUILDROOT/base/version_git.jl)

include("osutils.jl")
include("c.jl")
include("sysinfo.jl")

if !isdefined(Core, :Inference)
    include("docs/core.jl")
    Core.atdoc!(CoreDocs.docm)
end

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

# multidimensional arrays
include("cartesian.jl")
using .Cartesian
include("multidimensional.jl")
include("permuteddimsarray.jl")
using .PermutedDimsArrays

# nullable types
include("nullable.jl")

include("broadcast.jl")
importall .Broadcast

# base64 conversions (need broadcast)
include("base64.jl")
importall .Base64

# version
include("version.jl")

# system & environment
include("libc.jl")
using .Libc: getpid, gethostname, time
include("libdl.jl")
using .Libdl: DL_LOAD_PATH
include("env.jl")

# Scheduling
include("libuv.jl")
include("event.jl")
include("task.jl")
include("lock.jl")
include("threads.jl")
include("weakkeydict.jl")

# I/O
include("stream.jl")
include("socket.jl")
include("filesystem.jl")
importall .Filesystem
include("process.jl")
include("multimedia.jl")
importall .Multimedia
include("grisu/grisu.jl")
import .Grisu.print_shortest
include("methodshow.jl")

# core math functions
include("floatfuncs.jl")
include("math.jl")
importall .Math
const (√)=sqrt
const (∛)=cbrt

let SOURCE_PATH = ""
    global function _include(path)
        prev = SOURCE_PATH
        path = joinpath(dirname(prev),path)
        SOURCE_PATH = path
        Core.include(path)
        SOURCE_PATH = prev
    end
end
INCLUDE_STATE = 2 # include = _include (from lines above)

# reduction along dims
include("reducedim.jl")  # macros in this file relies on string.jl

# basic data structures
include("ordering.jl")
importall .Order

# Combinatorics
include("sort.jl")
importall .Sort

function deepcopy_internal end

# BigInts and BigFloats
include("gmp.jl")
importall .GMP
include("mpfr.jl")
importall .MPFR
big(n::Integer) = convert(BigInt,n)
big(x::AbstractFloat) = convert(BigFloat,x)
big(q::Rational) = big(numerator(q))//big(denominator(q))

include("combinatorics.jl")

# more hashing definitions
include("hashing2.jl")

# random number generation
include("dSFMT.jl")
include("random.jl")
importall .Random

# (s)printf macros
include("printf.jl")
importall .Printf

# metaprogramming
include("meta.jl")

# enums
include("Enums.jl")
importall .Enums

# concurrency and parallelism
include("serialize.jl")
importall .Serializer
include("channels.jl")

# memory-mapped and shared arrays
include("mmap.jl")
import .Mmap

# utilities - timing, help, edit
include("datafmt.jl")
importall .DataFmt
include("deepcopy.jl")
include("interactiveutil.jl")
include("summarysize.jl")
include("replutil.jl")
include("test.jl")
include("i18n.jl")
using .I18n

# frontend
include("initdefs.jl")
include("Terminals.jl")
include("LineEdit.jl")
include("REPLCompletions.jl")
include("REPL.jl")
include("client.jl")

# Stack frames and traces
include("stacktraces.jl")
importall .StackTraces

# misc useful functions & macros
include("util.jl")

# dense linear algebra
include("linalg/linalg.jl")
importall .LinAlg
const ⋅ = dot
const × = cross

# statistics
include("statistics.jl")

# irrational mathematical constants
include("irrationals.jl")

# signal processing
include("dft.jl")
importall .DFT
include("dsp.jl")
importall .DSP

# Fast math
include("fastmath.jl")
importall .FastMath

# libgit2 support
include("libgit2/libgit2.jl")

# package manager
include("pkg/pkg.jl")

# profiler
include("profile.jl")
importall .Profile

# dates
include("dates/Dates.jl")
import .Dates: Date, DateTime, DateFormat, @dateformat_str, now

# sparse matrices, vectors, and sparse linear algebra
include("sparse/sparse.jl")
importall .SparseArrays

include("asyncmap.jl")

include("parallel/Parallel.jl")
importall .Parallel
include("sharedarray.jl")

# code loading
include("loading.jl")

# worker threads
include("threadcall.jl")

# deprecated functions
include("deprecated.jl")

# Some basic documentation
include("docs/helpdb.jl")
include("docs/basedocs.jl")

# Documentation -- should always be included last in sysimg.
include("markdown/Markdown.jl")
include("docs/Docs.jl")
using .Docs, .Markdown
isdefined(Core, :Inference) && Docs.loaddocs(Core.Inference.CoreDocs.DOCS)

function __init__()
    # Base library init
    reinit_stdio()
    Multimedia.reinit_displays() # since Multimedia.displays uses STDOUT as fallback
    early_init()
    init_load_path()
    Parallel.init_parallel()
    init_threadcall()
end

INCLUDE_STATE = 3 # include = include_from_node1
include("precompile.jl")

end # baremodule Base

using Base
importall Base.Operators

Base.isfile("userimg.jl") && Base.include("userimg.jl")
