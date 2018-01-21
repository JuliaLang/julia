# This file is a part of Julia. License is MIT: https://julialang.org/license

baremodule Base

using Core.Intrinsics
ccall(:jl_set_istopmod, Cvoid, (Any, Bool), Base, true)

getproperty(x, f::Symbol) = getfield(x, f)
setproperty!(x, f::Symbol, v) = setfield!(x, f, convert(fieldtype(typeof(x), f), v))

# Try to help prevent users from shooting them-selves in the foot
# with ambiguities by defining a few common and critical operations
# (and these don't need the extra convert code)
getproperty(x::Module, f::Symbol) = getfield(x, f)
setproperty!(x::Module, f::Symbol, v) = setfield!(x, f, v)
getproperty(x::Type, f::Symbol) = getfield(x, f)
setproperty!(x::Type, f::Symbol, v) = setfield!(x, f, v)

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
        # to help users avoid error (accidentally evaluating into Base), this is deprecated
        depwarn("Base.include(string) is deprecated, use `include(fname)` or `Base.include(@__MODULE__, fname)` instead.", :include)
        result = include_relative(_current_module(), path)
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
        path = joinpath(dirname(prev), path)
        push!(_included_files, (mod, abspath(path)))
        SOURCE_PATH = path
        result = Core.include(mod, path)
        SOURCE_PATH = prev
        result
    end
end
INCLUDE_STATE = 1 # include = Core.include

baremodule MainInclude
export include
include(fname::AbstractString) = Main.Base.include(Main, fname)
end

include("coreio.jl")

eval(x) = Core.eval(Base, x)
eval(m, x) = Core.eval(m, x)
VecElement{T}(arg) where {T} = VecElement{T}(convert(T, arg))
convert(::Type{T}, arg)  where {T<:VecElement} = T(arg)
convert(::Type{T}, arg::T) where {T<:VecElement} = arg

# init core docsystem
import Core: @doc, @__doc__, @doc_str, WrappedException
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
include("refpointer.jl")
include("checked.jl")
using .Checked

# vararg Symbol constructor
Symbol(x...) = Symbol(string(x...))

# Define the broadcast function, which is mostly implemented in
# broadcast.jl, so that we can overload broadcast methods for
# specific array types etc.
#  --Here, just define fallback routines for broadcasting with no arguments
broadcast(f) = f()
broadcast!(f, X::AbstractArray) = (@inbounds for I in eachindex(X); X[I] = f(); end; X)

# array structures
include("indices.jl")
include("array.jl")
include("abstractarray.jl")
include("subarray.jl")
include("reinterpretarray.jl")


# ## dims-type-converting Array constructors for convenience
# type and dimensionality specified, accepting dims as series of Integers
Vector{T}(::Uninitialized, m::Integer) where {T} = Vector{T}(uninitialized, Int(m))
Matrix{T}(::Uninitialized, m::Integer, n::Integer) where {T} = Matrix{T}(uninitialized, Int(m), Int(n))
# type but not dimensionality specified, accepting dims as series of Integers
Array{T}(::Uninitialized, m::Integer) where {T} = Array{T,1}(uninitialized, Int(m))
Array{T}(::Uninitialized, m::Integer, n::Integer) where {T} = Array{T,2}(uninitialized, Int(m), Int(n))
Array{T}(::Uninitialized, m::Integer, n::Integer, o::Integer) where {T} = Array{T,3}(uninitialized, Int(m), Int(n), Int(o))
Array{T}(::Uninitialized, d::Integer...) where {T} = Array{T}(uninitialized, convert(Tuple{Vararg{Int}}, d))
# dimensionality but not type specified, accepting dims as series of Integers
Vector(::Uninitialized, m::Integer) = Vector{Any}(uninitialized, Int(m))
Matrix(::Uninitialized, m::Integer, n::Integer) = Matrix{Any}(uninitialized, Int(m), Int(n))
# empty vector constructor
Vector() = Vector{Any}(uninitialized, 0)

# Array constructors for nothing and missing
# type and dimensionality specified
Array{T,N}(::Nothing, d...) where {T,N} = fill!(Array{T,N}(uninitialized, d...), nothing)
Array{T,N}(::Missing, d...) where {T,N} = fill!(Array{T,N}(uninitialized, d...), missing)
# type but not dimensionality specified
Array{T}(::Nothing, d...) where {T} = fill!(Array{T}(uninitialized, d...), nothing)
Array{T}(::Missing, d...) where {T} = fill!(Array{T}(uninitialized, d...), missing)

include("abstractdict.jl")

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
include("bitarray.jl")
include("bitset.jl")

if !isdefined(Core, :Compiler)
    include("docs/core.jl")
    Core.atdoc!(CoreDocs.docm)
end

# Some type
include("some.jl")

include("dict.jl")
include("set.jl")
include("iterators.jl")
using .Iterators: zip, enumerate
using .Iterators: Flatten, product  # for generators

include("char.jl")
include("strings/basic.jl")
include("strings/string.jl")

# Definition of StridedArray
StridedReshapedArray{T,N,A<:Union{DenseArray,FastContiguousSubArray}} = ReshapedArray{T,N,A}
StridedReinterpretArray{T,N,A<:Union{DenseArray,FastContiguousSubArray}} = ReinterpretArray{T,N,S,A} where S
StridedArray{T,N,A<:Union{DenseArray,StridedReshapedArray},
    I<:Tuple{Vararg{Union{RangeIndex, AbstractCartesianIndex}}}} =
    Union{DenseArray{T,N}, SubArray{T,N,A,I}, StridedReshapedArray{T,N}, StridedReinterpretArray{T,N,A}}
StridedVector{T,A<:Union{DenseArray,StridedReshapedArray},
    I<:Tuple{Vararg{Union{RangeIndex, AbstractCartesianIndex}}}} =
    Union{DenseArray{T,1}, SubArray{T,1,A,I}, StridedReshapedArray{T,1}, StridedReinterpretArray{T,1,A}}
StridedMatrix{T,A<:Union{DenseArray,StridedReshapedArray},
    I<:Tuple{Vararg{Union{RangeIndex, AbstractCartesianIndex}}}} =
    Union{DenseArray{T,2}, SubArray{T,2,A,I}, StridedReshapedArray{T,2}, StridedReinterpretArray{T,2,A}}
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

# Logging
include("logging.jl")
using .CoreLogging

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

# To limit dependency on rand functionality (implemented in the Random
# module), Crand is used in file.jl, and could be used in error.jl
# (but it breaks a test)
"""
    Crand([T::Type])

Interface to the C `rand()` function. If `T` is provided, generate a value of type `T`
by composing two calls to `Crand()`. `T` can be `UInt32` or `Float64`.
"""
Crand() = ccall(:rand, Cuint, ())
# RAND_MAX at least 2^15-1 in theory, but we assume 2^16-1 (in practice, it's 2^31-1)
Crand(::Type{UInt32}) = ((Crand() % UInt32) << 16) ⊻ (Crand() % UInt32)
Crand(::Type{Float64}) = Crand(UInt32) / 2^32

"""
    Csrand([seed])

Interface the the C `srand(seed)` function.
"""
Csrand(seed=floor(time())) = ccall(:srand, Cvoid, (Cuint,), seed)

# functions defined in Random
function rand end
function randn end

# I/O
include("stream.jl")
include("socket.jl")
include("filesystem.jl")
using .Filesystem
include("process.jl")
include("grisu/grisu.jl")
import .Grisu.print_shortest
include("methodshow.jl")

# core math functions
include("floatfuncs.jl")
include("math.jl")
using .Math
import .Math: gamma
const (√)=sqrt
const (∛)=cbrt

INCLUDE_STATE = 2 # include = _include (from lines above)

# reduction along dims
include("reducedim.jl")  # macros in this file relies on string.jl

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

# enums
include("Enums.jl")
using .Enums

# concurrency and parallelism
include("channels.jl")

# utilities - timing, help, edit
include("deepcopy.jl")
include("interactiveutil.jl")
include("summarysize.jl")
include("errorshow.jl")
include("i18n.jl")
using .I18n

# Stack frames and traces
include("stacktraces.jl")
using .StackTraces

include("initdefs.jl")
include("client.jl")

# misc useful functions & macros
include("util.jl")

# statistics
include("statistics.jl")

# missing values
include("missing.jl")

# libgit2 support
include("libgit2/libgit2.jl")

# package manager
include("pkg/pkg.jl")

# worker threads
include("threadcall.jl")

# code loading
include("uuid.jl")
include("loading.jl")

# set up depot & load paths to be able to find stdlib packages
let BINDIR = ccall(:jl_get_julia_bindir, Any, ())
    init_depot_path(BINDIR)
    init_load_path(BINDIR)
end

INCLUDE_STATE = 3 # include = include_relative

import Base64

INCLUDE_STATE = 2

include("asyncmap.jl")

include("multimedia.jl")
using .Multimedia

# deprecated functions
include("deprecated.jl")

# Some basic documentation
include("docs/basedocs.jl")

# Documentation -- should always be included last in sysimg.
include("markdown/Markdown.jl")
include("docs/Docs.jl")
using .Docs, .Markdown
isdefined(Core, :Compiler) && Docs.loaddocs(Core.Compiler.CoreDocs.DOCS)

function __init__()
    # for the few uses of Crand in Base:
    Csrand()
    # Base library init
    reinit_stdio()
    global_logger(root_module(PkgId("Logging")).ConsoleLogger(STDERR))
    Multimedia.reinit_displays() # since Multimedia.displays uses STDOUT as fallback
    early_init()
    init_depot_path()
    init_load_path()
    init_threadcall()
end

INCLUDE_STATE = 3 # include = include_relative

end # baremodule Base

using Base

# Ensure this file is also tracked
pushfirst!(Base._included_files, (@__MODULE__, joinpath(@__DIR__, "sysimg.jl")))

# load some stdlib packages but don't put their names in Main
Base.require(Base, :Base64)
Base.require(Base, :CRC32c)
Base.require(Base, :Dates)
Base.require(Base, :DelimitedFiles)
Base.require(Base, :Serialization)
Base.require(Base, :Distributed)
Base.require(Base, :FileWatching)
Base.require(Base, :Future)
Base.require(Base, :IterativeEigensolvers)
Base.require(Base, :Libdl)
Base.require(Base, :LinearAlgebra)
Base.require(Base, :Logging)
Base.require(Base, :Mmap)
Base.require(Base, :Printf)
Base.require(Base, :Profile)
Base.require(Base, :Random)
Base.require(Base, :SharedArrays)
Base.require(Base, :SparseArrays)
Base.require(Base, :SuiteSparse)
Base.require(Base, :Test)
Base.require(Base, :Unicode)
Base.require(Base, :REPL)

@eval Base begin
    @deprecate_binding Test root_module(Base, :Test) true ", run `using Test` instead"
    @deprecate_binding Mmap root_module(Base, :Mmap) true ", run `using Mmap` instead"
    @deprecate_binding Profile root_module(Base, :Profile) true ", run `using Profile` instead"
    @deprecate_binding Dates root_module(Base, :Dates) true ", run `using Dates` instead"
    @deprecate_binding Distributed root_module(Base, :Distributed) true ", run `using Distributed` instead"
    @deprecate_binding Random root_module(Base, :Random) true ", run `using Random` instead"
    @deprecate_binding Serializer root_module(Base, :Serialization) true ", run `using Serialization` instead"
    @deprecate_binding Libdl root_module(Base, :Libdl) true ", run `using Libdl` instead"

    # PR #25249
    @deprecate_binding SparseArrays root_module(Base, :SparseArrays) true ", run `using SparseArrays` instead"
    @deprecate_binding(AbstractSparseArray, root_module(Base, :SparseArrays).AbstractSparseArray, true,
        ", run `using SparseArrays` to load sparse array functionality")
    @deprecate_binding(AbstractSparseMatrix, root_module(Base, :SparseArrays).AbstractSparseMatrix, true,
        ", run `using SparseArrays` to load sparse array functionality")
    @deprecate_binding(AbstractSparseVector, root_module(Base, :SparseArrays).AbstractSparseVector, true,
        ", run `using SparseArrays` to load sparse array functionality")
    @deprecate_binding(SparseMatrixCSC, root_module(Base, :SparseArrays).SparseMatrixCSC, true,
        ", run `using SparseArrays` to load sparse array functionality")
    @deprecate_binding(SparseVector, root_module(Base, :SparseArrays).SparseVector, true,
        ", run `using SparseArrays` to load sparse array functionality")

    # PR #25571
    @deprecate_binding LinAlg root_module(Base, :LinearAlgebra) true ", run `using LinearAlgebra` instead"
    @deprecate_binding(I, root_module(Base, :LinearAlgebra).I, true,
        ", run `using LinearAlgebra` to load linear algebra functionality.")

    # PR 25544
    @deprecate_binding REPL            root_module(Base, :REPL)                 true ", run `using REPL` instead"
    @deprecate_binding LineEdit        root_module(Base, :REPL).LineEdit        true ", use `REPL.LineEdit` instead"
    @deprecate_binding REPLCompletions root_module(Base, :REPL).REPLCompletions true ", use `REPL.REPLCompletions` instead"
    @deprecate_binding Terminals       root_module(Base, :REPL).Terminals       true ", use `REPL.Terminals` instead"
end

empty!(DEPOT_PATH)
empty!(LOAD_PATH)

Base.isfile("userimg.jl") && Base.include(Main, "userimg.jl")

Base.include(Base, "precompile.jl")
