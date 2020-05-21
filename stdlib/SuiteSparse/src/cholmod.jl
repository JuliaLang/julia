# This file is a part of Julia. License is MIT: https://julialang.org/license

# Theoretically CHOLMOD supports both Int32 and Int64 indices on 64-bit.
# However experience suggests that using both in the same session causes memory
# leaks, so we restrict indices to be SuiteSparse_long (see cholmod_h.jl).
# Ref: https://github.com/JuliaLang/julia/issues/12664

# Additionally, only Float64/ComplexF64 are supported in practice.
# Ref: https://github.com/JuliaLang/julia/issues/25986

module CHOLMOD

import Base: (*), convert, copy, eltype, getindex, getproperty, show, size,
             IndexStyle, IndexLinear, IndexCartesian, adjoint, axes
using Base: require_one_based_indexing

using LinearAlgebra
import LinearAlgebra: (\),
                 cholesky, cholesky!, det, diag, ishermitian, isposdef,
                 issuccess, issymmetric, ldlt, ldlt!, logdet

using SparseArrays
using SparseArrays: getcolptr
import Libdl

export
    Dense,
    Factor,
    Sparse

import SparseArrays: AbstractSparseMatrix, SparseMatrixCSC, indtype, sparse, spzeros, nnz

import ..increment, ..increment!, ..decrement, ..decrement!

#########
# Setup #
#########

include("cholmod_h.jl")

const CHOLMOD_MIN_VERSION = v"2.1.1"

const common_struct = Vector{Vector{UInt8}}()

const common_supernodal = Vector{Ptr{Cint}}()
const common_final_ll   = Vector{Ptr{Cint}}()
const common_print      = Vector{Ptr{Cint}}()
const common_itype      = Vector{Ptr{Cint}}()
const common_dtype      = Vector{Ptr{Cint}}()
const common_nmethods   = Vector{Ptr{Cint}}()
const common_postorder  = Vector{Ptr{Cint}}()

### These offsets are defined in SuiteSparse_wrapper.c
const common_size = ccall((:jl_cholmod_common_size,:libsuitesparse_wrapper),Int,())

const cholmod_com_offsets = Vector{Csize_t}(undef, 19)
ccall((:jl_cholmod_common_offsets, :libsuitesparse_wrapper),
    Nothing, (Ptr{Csize_t},), cholmod_com_offsets)

## macro to generate the name of the C function according to the integer type
macro cholmod_name(nm)
    string("cholmod_l_", nm)
end

function start(a::Vector{UInt8})
    @isok ccall((@cholmod_name("start"), :libcholmod),
        Cint, (Ptr{UInt8},), a)
    return a
end

function finish(a::Vector{UInt8})
    @isok ccall((@cholmod_name("finish"), :libcholmod),
        Cint, (Ptr{UInt8},), a)
    return a
end

function defaults(a::Vector{UInt8})
    @isok ccall((@cholmod_name("defaults"), :libcholmod),
        Cint, (Ptr{UInt8},), a)
    return a
end

const build_version_array = Vector{Cint}(undef, 3)
ccall((:jl_cholmod_version, :libsuitesparse_wrapper), Cint, (Ptr{Cint},), build_version_array)
const build_version = VersionNumber(build_version_array...)

function __init__()
    try
        ### Check if the linked library is compatible with the Julia code
        if Libdl.dlsym_e(Libdl.dlopen("libcholmod"), :cholmod_version) != C_NULL
            current_version_array = Vector{Cint}(undef, 3)
            ccall((:cholmod_version, :libcholmod), Cint, (Ptr{Cint},), current_version_array)
            current_version = VersionNumber(current_version_array...)
        else # CHOLMOD < 2.1.1 does not include cholmod_version()
            current_version = v"0.0.0"
        end


        if current_version < CHOLMOD_MIN_VERSION
            @warn """
                CHOLMOD version incompatibility

                Julia was compiled with CHOLMOD version $build_version. It is
                currently linked with a version older than
                $(CHOLMOD_MIN_VERSION). This might cause Julia to
                terminate when working with sparse matrix factorizations,
                e.g. solving systems of equations with \\.

                It is recommended that you use Julia with a recent version
                of CHOLMOD, or download the generic binaries
                from www.julialang.org, which ship with the correct
                versions of all dependencies.
                """
        elseif build_version_array[1] != current_version_array[1]
            @warn """
                CHOLMOD version incompatibility

                Julia was compiled with CHOLMOD version $build_version. It is
                currently linked with version $current_version.
                This might cause Julia to terminate when working with
                sparse matrix factorizations, e.g. solving systems of
                equations with \\.

                It is recommended that you use Julia with the same major
                version of CHOLMOD as the one used during the build, or
                download the generic binaries from www.julialang.org,
                which ship with the correct versions of all dependencies.
                """
        end

        intsize = Int(ccall((:jl_cholmod_sizeof_long,:libsuitesparse_wrapper),Csize_t,()))
        if intsize != 4length(IndexTypes)
            @error """
                 CHOLMOD integer size incompatibility

                 Julia was compiled with a version of CHOLMOD that
                 supported $(32length(IndexTypes)) bit integers. It is
                 currently linked with version that supports $(8intsize)
                 integers. This might cause Julia to terminate when
                 working with sparse matrix factorizations, e.g. solving
                 systems of equations with \\.

                 This problem can be fixed by modifying the Julia build
                 configuration or by downloading the OS X or generic
                 Linux binary from www.julialang.org, which include
                 the correct versions of all dependencies.
                 """
        end

        ### Initiate CHOLMOD
        ### common_struct controls the type of factorization and keeps pointers
        ### to temporary memory. We need to manage a copy for each thread.
        nt = Threads.nthreads()
        resize!(common_struct    , nt)
        resize!(common_supernodal, nt)
        resize!(common_final_ll  , nt)
        resize!(common_print     , nt)
        resize!(common_itype     , nt)
        resize!(common_dtype     , nt)
        resize!(common_nmethods  , nt)
        resize!(common_postorder , nt)
        for i in 1:nt
            common_struct[i] = fill(0xff, common_size)

            common_supernodal[i] = pointer(common_struct[i], cholmod_com_offsets[4] + 1)
            common_final_ll[i]   = pointer(common_struct[i], cholmod_com_offsets[7] + 1)
            common_print[i]      = pointer(common_struct[i], cholmod_com_offsets[13] + 1)
            common_itype[i]      = pointer(common_struct[i], cholmod_com_offsets[18] + 1)
            common_dtype[i]      = pointer(common_struct[i], cholmod_com_offsets[19] + 1)
            common_nmethods[i]   = pointer(common_struct[i], cholmod_com_offsets[15] + 1)
            common_postorder[i]  = pointer(common_struct[i], cholmod_com_offsets[17] + 1)

            start(common_struct[i])              # initializes CHOLMOD
            set_print_level(common_struct[i], 0) # no printing from CHOLMOD by default
        end

        # Register gc tracked allocator if CHOLMOD is new enough
        if current_version >= v"3.0.0"
            cnfg = cglobal((:SuiteSparse_config, :libsuitesparseconfig), Ptr{Cvoid})
            unsafe_store!(cnfg, cglobal(:jl_malloc, Ptr{Cvoid}), 1)
            unsafe_store!(cnfg, cglobal(:jl_calloc, Ptr{Cvoid}), 2)
            unsafe_store!(cnfg, cglobal(:jl_realloc, Ptr{Cvoid}), 3)
            unsafe_store!(cnfg, cglobal(:jl_free, Ptr{Cvoid}), 4)
        end

    catch ex
        @error "Error during initialization of module CHOLMOD" exception=ex,catch_backtrace()
    end
end

function set_print_level(cm::Vector{UInt8}, lev::Integer)
    unsafe_store!(common_print[Threads.threadid()], lev)
end

####################
# Type definitions #
####################

abstract type SuiteSparseStruct end

# The three core data types for CHOLMOD: Dense, Sparse and Factor.
# CHOLMOD manages the memory, so the Julia versions only wrap a
# pointer to a struct.  Therefore finalizers should be registered each
# time a pointer is returned from CHOLMOD.

# Dense
struct C_Dense{T<:VTypes} <: SuiteSparseStruct
    nrow::Csize_t
    ncol::Csize_t
    nzmax::Csize_t
    d::Csize_t
    x::Ptr{T}
    z::Ptr{Cvoid}
    xtype::Cint
    dtype::Cint
end

mutable struct Dense{Tv<:VTypes} <: DenseMatrix{Tv}
    ptr::Ptr{C_Dense{Tv}}
    function Dense{Tv}(ptr::Ptr{C_Dense{Tv}}) where Tv<:VTypes
        if ptr == C_NULL
            throw(ArgumentError("dense matrix construction failed for " *
                "unknown reasons. Please submit a bug report."))
        end
        s = unsafe_load(ptr)
        if s.xtype != xtyp(Tv)
            free!(ptr)
            throw(CHOLMODException("xtype=$(s.xtype) not supported"))
        elseif s.dtype != dtyp(Tv)
            free!(ptr)
            throw(CHOLMODException("dtype=$(s.dtype) not supported"))
        end
        A = new(ptr)
        finalizer(free!, A)
        return A
    end
end
Dense(p::Ptr{C_Dense{Tv}}) where {Tv<:VTypes} = Dense{Tv}(p)

# Sparse
# allow Cvoid pointer for reading matrices of unknown type from files as in
# cholmod_read_sparse
struct C_Sparse{Tv<:Union{Cvoid, VTypes}} <: SuiteSparseStruct
    nrow::Csize_t
    ncol::Csize_t
    nzmax::Csize_t
    p::Ptr{SuiteSparse_long}
    i::Ptr{SuiteSparse_long}
    nz::Ptr{SuiteSparse_long}
    x::Ptr{Tv}
    z::Ptr{Cvoid}
    stype::Cint
    itype::Cint
    xtype::Cint
    dtype::Cint
    sorted::Cint
    packed::Cint
end

mutable struct Sparse{Tv<:VTypes} <: AbstractSparseMatrix{Tv,SuiteSparse_long}
    ptr::Ptr{C_Sparse{Tv}}
    function Sparse{Tv}(ptr::Ptr{C_Sparse{Tv}}) where Tv<:VTypes
        if ptr == C_NULL
            throw(ArgumentError("sparse matrix construction failed for " *
                "unknown reasons. Please submit a bug report."))
        end
        s = unsafe_load(ptr)
        if s.itype != ityp(SuiteSparse_long)
            free!(ptr)
            throw(CHOLMODException("itype=$(s.itype) not supported"))
        elseif s.xtype != xtyp(Tv)
            free!(ptr)
            throw(CHOLMODException("xtype=$(s.xtype) not supported"))
        elseif s.dtype != dtyp(Tv)
            free!(ptr)
            throw(CHOLMODException("dtype=$(s.dtype) not supported"))
        end
        A = new(ptr)
        finalizer(free!, A)
        return A
    end
end
Sparse(p::Ptr{C_Sparse{Tv}}) where {Tv<:VTypes} = Sparse{Tv}(p)

# Useful when reading in files, but not type stable
function Sparse(p::Ptr{C_Sparse{Cvoid}})
    if p == C_NULL
        throw(ArgumentError("sparse matrix construction failed for " *
                            "unknown reasons. Please submit a bug report."))
    end
    s = unsafe_load(p)
    Tv = s.xtype == REAL ? Float64 : Complex{Float64}
    Sparse(convert(Ptr{C_Sparse{Tv}}, p))
end

Base.unsafe_convert(::Type{Ptr{Tv}}, A::Sparse{Tv}) where {Tv} = getfield(A, :ptr)

# Factor
# Cvoid is used for pattern-only factors
struct C_Factor{Tv<:VTypes} <: SuiteSparseStruct
    n::Csize_t
    minor::Csize_t
    Perm::Ptr{SuiteSparse_long}
    ColCount::Ptr{SuiteSparse_long}
    @static if build_version >= v"2.1.0"
        IPerm::Ptr{SuiteSparse_long}  # this pointer was added in version 2.1.0
    end
    nzmax::Csize_t
    p::Ptr{SuiteSparse_long}
    i::Ptr{SuiteSparse_long}
    x::Ptr{Tv}
    z::Ptr{Cvoid}
    nz::Ptr{SuiteSparse_long}
    next::Ptr{SuiteSparse_long}
    prev::Ptr{SuiteSparse_long}
    nsuper::Csize_t
    ssize::Csize_t
    xsize::Csize_t
    maxcsize::Csize_t
    maxesize::Csize_t
    super::Ptr{SuiteSparse_long}
    pi::Ptr{SuiteSparse_long}
    px::Ptr{SuiteSparse_long}
    s::Ptr{SuiteSparse_long}
    ordering::Cint
    is_ll::Cint
    is_super::Cint
    is_monotonic::Cint
    itype::Cint
    xtype::Cint
    dtype::Cint
end

mutable struct Factor{Tv<:VTypes} <: Factorization{Tv}
    ptr::Ptr{C_Factor{Tv}}
    function Factor{Tv}(ptr::Ptr{C_Factor{Tv}}, register_finalizer = true) where Tv
        if ptr == C_NULL
            throw(ArgumentError("factorization construction failed for " *
                "unknown reasons. Please submit a bug report."))
        end
        s = unsafe_load(ptr)
        if s.itype != ityp(SuiteSparse_long)
            free!(ptr)
            throw(CHOLMODException("itype=$(s.itype) not supported"))
        elseif s.xtype != xtyp(Tv) && s.xtype != PATTERN
            free!(ptr)
            throw(CHOLMODException("xtype=$(s.xtype) not supported"))
        elseif s.dtype != dtyp(Tv)
            free!(ptr)
            throw(CHOLMODException("dtype=$(s.dtype) not supported"))
        end
        F = new(ptr)
        if register_finalizer
            finalizer(free!, F)
        end
        return F
    end
end
Factor(ptr::Ptr{C_Factor{Tv}}) where {Tv<:VTypes} = Factor{Tv}(ptr)
Factor(x::Factor) = x

Base.adjoint(F::Factor) = Adjoint(F)
Base.transpose(F::Factor) = Transpose(F)

# All pointer loads should be checked to make sure that SuiteSparse is not called with
# a C_NULL pointer which could cause a segfault. Pointers are set to null
# when serialized so this can happen when multiple processes are in use.
function Base.unsafe_convert(::Type{Ptr{T}}, x::Union{Dense,Sparse,Factor}) where T<:SuiteSparseStruct
    xp = getfield(x, :ptr)
    if xp == C_NULL
        throw(ArgumentError("pointer to the $T object is null. This can " *
            "happen if the object has been serialized."))
    else
        return xp
    end
end
Base.pointer(x::Dense{Tv}) where {Tv}  = Base.unsafe_convert(Ptr{C_Dense{Tv}}, x)
Base.pointer(x::Sparse{Tv}) where {Tv} = Base.unsafe_convert(Ptr{C_Sparse{Tv}}, x)
Base.pointer(x::Factor{Tv}) where {Tv} = Base.unsafe_convert(Ptr{C_Factor{Tv}}, x)

# FactorComponent, for encoding particular factors from a factorization
mutable struct FactorComponent{Tv,S} <: AbstractMatrix{Tv}
    F::Factor{Tv}

    function FactorComponent{Tv,S}(F::Factor{Tv}) where {Tv,S}
        s = unsafe_load(pointer(F))
        if s.is_ll != 0
            if !(S === :L || S === :U || S === :PtL || S === :UP)
                throw(CHOLMODException(string(S, " not supported for sparse ",
                    "LLt matrices; try :L, :U, :PtL, or :UP")))
            end
        elseif !(S === :L || S === :U || S === :PtL || S === :UP ||
                S === :D || S === :LD || S === :DU || S === :PtLD || S === :DUP)
            throw(CHOLMODException(string(S, " not supported for sparse LDLt ",
                "matrices; try :L, :U, :PtL, :UP, :D, :LD, :DU, :PtLD, or :DUP")))
        end
        new(F)
    end
end
function FactorComponent(F::Factor{Tv}, sym::Symbol) where Tv
    FactorComponent{Tv,sym}(F)
end

Factor(FC::FactorComponent) = Factor(FC.F)

#################
# Thin wrappers #
#################

# Dense wrappers

### cholmod_core_h ###
function allocate_dense(m::Integer, n::Integer, d::Integer, ::Type{Tv}) where {Tv<:VTypes}
    Dense(ccall((@cholmod_name("allocate_dense"), :libcholmod), Ptr{C_Dense{Tv}},
                (Csize_t, Csize_t, Csize_t, Cint, Ptr{Cvoid}),
                m, n, d, xtyp(Tv), common_struct[Threads.threadid()]))
end

function free!(p::Ptr{C_Dense{Tv}}) where {Tv<:VTypes}
    @isok ccall((@cholmod_name("free_dense"), :libcholmod), Cint,
                (Ref{Ptr{C_Dense{Tv}}}, Ptr{Cvoid}),
                p, common_struct[Threads.threadid()])
end
function zeros(m::Integer, n::Integer, ::Type{Tv}) where Tv<:VTypes
    Dense(ccall((@cholmod_name("zeros"), :libcholmod), Ptr{C_Dense{Tv}},
                (Csize_t, Csize_t, Cint, Ptr{UInt8}),
                m, n, xtyp(Tv), common_struct[Threads.threadid()]))
end
zeros(m::Integer, n::Integer) = zeros(m, n, Float64)

function ones(m::Integer, n::Integer, ::Type{Tv}) where Tv<:VTypes
    Dense(ccall((@cholmod_name("ones"), :libcholmod), Ptr{C_Dense{Tv}},
                (Csize_t, Csize_t, Cint, Ptr{UInt8}),
                m, n, xtyp(Tv), common_struct[Threads.threadid()]))
end
ones(m::Integer, n::Integer) = ones(m, n, Float64)

function eye(m::Integer, n::Integer, ::Type{Tv}) where Tv<:VTypes
    Dense(ccall((@cholmod_name("eye"), :libcholmod), Ptr{C_Dense{Tv}},
                (Csize_t, Csize_t, Cint, Ptr{UInt8}),
                m, n, xtyp(Tv), common_struct[Threads.threadid()]))
end
eye(m::Integer, n::Integer) = eye(m, n, Float64)
eye(n::Integer) = eye(n, n, Float64)

function copy(A::Dense{Tv}) where Tv<:VTypes
    Dense(ccall((@cholmod_name("copy_dense"), :libcholmod), Ptr{C_Dense{Tv}},
                (Ptr{C_Dense{Tv}}, Ptr{UInt8}),
                A, common_struct[Threads.threadid()]))
end

function sort!(S::Sparse{Tv}) where Tv<:VTypes
    @isok ccall((@cholmod_name("sort"), :libcholmod), Cint,
                (Ptr{C_Sparse{Tv}}, Ptr{UInt8}),
                S, common_struct[Threads.threadid()])
    return S
end

### cholmod_matrixops.h ###
function norm_dense(D::Dense{Tv}, p::Integer) where Tv<:VTypes
    s = unsafe_load(pointer(D))
    if p == 2
        if s.ncol > 1
            throw(ArgumentError("2 norm only supported when matrix has one column"))
        end
    elseif p != 0 && p != 1
        throw(ArgumentError("second argument must be either 0 (Inf norm), 1, or 2"))
    end
    ccall((@cholmod_name("norm_dense"), :libcholmod), Cdouble,
        (Ptr{C_Dense{Tv}}, Cint, Ptr{UInt8}),
          D, p, common_struct[Threads.threadid()])
end

### cholmod_check.h ###
function check_dense(A::Dense{Tv}) where Tv<:VTypes
    ccall((@cholmod_name("check_dense"), :libcholmod), Cint,
          (Ptr{C_Dense{Tv}}, Ptr{UInt8}),
          pointer(A), common_struct[Threads.threadid()]) != 0
end

# Non-Dense wrappers
### cholmod_core.h ###
function allocate_sparse(nrow::Integer, ncol::Integer, nzmax::Integer,
        sorted::Bool, packed::Bool, stype::Integer, ::Type{Tv}) where {Tv<:VTypes}
    Sparse(ccall((@cholmod_name("allocate_sparse"), :libcholmod),
            Ptr{C_Sparse{Tv}},
                (Csize_t, Csize_t, Csize_t, Cint,
                 Cint, Cint, Cint, Ptr{Cvoid}),
                nrow, ncol, nzmax, sorted,
                packed, stype, xtyp(Tv), common_struct[Threads.threadid()]))
end

function free!(ptr::Ptr{C_Sparse{Tv}}) where Tv<:VTypes
    @isok ccall((@cholmod_name("free_sparse"), :libcholmod), Cint,
            (Ref{Ptr{C_Sparse{Tv}}}, Ptr{UInt8}),
                ptr, common_struct[Threads.threadid()])
end

function free!(ptr::Ptr{C_Factor{Tv}}) where Tv<:VTypes
    # Warning! Important that finalizer doesn't modify the global Common struct.
    @isok ccall((@cholmod_name("free_factor"), :libcholmod), Cint,
            (Ref{Ptr{C_Factor{Tv}}}, Ptr{Cvoid}),
                ptr, common_struct[Threads.threadid()])
end

function aat(A::Sparse{Tv}, fset::Vector{SuiteSparse_long}, mode::Integer) where Tv<:VRealTypes
    Sparse(ccall((@cholmod_name("aat"), :libcholmod),
        Ptr{C_Sparse{Tv}},
            (Ptr{C_Sparse{Tv}}, Ptr{SuiteSparse_long}, Csize_t, Cint, Ptr{UInt8}),
                A, fset, length(fset), mode, common_struct[Threads.threadid()]))
end

function sparse_to_dense(A::Sparse{Tv}) where Tv<:VTypes
    Dense(ccall((@cholmod_name("sparse_to_dense"),:libcholmod),
        Ptr{C_Dense{Tv}},
            (Ptr{C_Sparse{Tv}}, Ptr{UInt8}),
                A, common_struct[Threads.threadid()]))
end
function dense_to_sparse(D::Dense{Tv}, ::Type{SuiteSparse_long}) where Tv<:VTypes
    Sparse(ccall((@cholmod_name("dense_to_sparse"),:libcholmod),
        Ptr{C_Sparse{Tv}},
            (Ptr{C_Dense{Tv}}, Cint, Ptr{UInt8}),
                D, true, common_struct[Threads.threadid()]))
end

function factor_to_sparse!(F::Factor{Tv}) where Tv<:VTypes
    ss = unsafe_load(pointer(F))
    ss.xtype == PATTERN && throw(CHOLMODException("only numeric factors are supported"))
    Sparse(ccall((@cholmod_name("factor_to_sparse"),:libcholmod),
        Ptr{C_Sparse{Tv}},
            (Ptr{C_Factor{Tv}}, Ptr{UInt8}),
                F, common_struct[Threads.threadid()]))
end

function change_factor!(F::Factor{Tv}, to_ll::Bool, to_super::Bool, to_packed::Bool,
                        to_monotonic::Bool) where Tv<:VTypes
    @isok ccall((@cholmod_name("change_factor"),:libcholmod), Cint,
            (Cint, Cint, Cint, Cint, Cint, Ptr{C_Factor{Tv}}, Ptr{UInt8}),
                xtyp(Tv), to_ll, to_super, to_packed, to_monotonic, F, common_struct[Threads.threadid()])
end

function check_sparse(A::Sparse{Tv}) where Tv<:VTypes
    ccall((@cholmod_name("check_sparse"),:libcholmod), Cint,
          (Ptr{C_Sparse{Tv}}, Ptr{UInt8}),
           A, common_struct[Threads.threadid()]) != 0
end

function check_factor(F::Factor{Tv}) where Tv<:VTypes
    ccall((@cholmod_name("check_factor"),:libcholmod), Cint,
          (Ptr{C_Factor{Tv}}, Ptr{UInt8}),
           F, common_struct[Threads.threadid()]) != 0
end

function nnz(A::Sparse{Tv}) where Tv<:VTypes
    ccall((@cholmod_name("nnz"),:libcholmod), Int,
            (Ptr{C_Sparse{Tv}}, Ptr{UInt8}),
                A, common_struct[Threads.threadid()])
end

function speye(m::Integer, n::Integer, ::Type{Tv}) where Tv<:VTypes
    Sparse(ccall((@cholmod_name("speye"), :libcholmod),
        Ptr{C_Sparse{Tv}},
            (Csize_t, Csize_t, Cint, Ptr{UInt8}),
                m, n, xtyp(Tv), common_struct[Threads.threadid()]))
end

function spzeros(m::Integer, n::Integer, nzmax::Integer, ::Type{Tv}) where Tv<:VTypes
    Sparse(ccall((@cholmod_name("spzeros"), :libcholmod),
        Ptr{C_Sparse{Tv}},
            (Csize_t, Csize_t, Csize_t, Cint, Ptr{UInt8}),
             m, n, nzmax, xtyp(Tv), common_struct[Threads.threadid()]))
end

function transpose_(A::Sparse{Tv}, values::Integer) where Tv<:VTypes
    Sparse(ccall((@cholmod_name("transpose"),:libcholmod),
        Ptr{C_Sparse{Tv}},
            (Ptr{C_Sparse{Tv}}, Cint, Ptr{UInt8}),
                A, values, common_struct[Threads.threadid()]))
end

function copy(F::Factor{Tv}) where Tv<:VTypes
    Factor(ccall((@cholmod_name("copy_factor"),:libcholmod),
        Ptr{C_Factor{Tv}},
            (Ptr{C_Factor{Tv}}, Ptr{UInt8}),
                F, common_struct[Threads.threadid()]))
end
function copy(A::Sparse{Tv}) where Tv<:VTypes
    Sparse(ccall((@cholmod_name("copy_sparse"),:libcholmod),
        Ptr{C_Sparse{Tv}},
            (Ptr{C_Sparse{Tv}}, Ptr{UInt8}),
                A, common_struct[Threads.threadid()]))
end
function copy(A::Sparse{Tv}, stype::Integer, mode::Integer) where Tv<:VRealTypes
    Sparse(ccall((@cholmod_name("copy"),:libcholmod),
        Ptr{C_Sparse{Tv}},
            (Ptr{C_Sparse{Tv}}, Cint, Cint, Ptr{UInt8}),
                A, stype, mode, common_struct[Threads.threadid()]))
end

### cholmod_check.h ###
function print_sparse(A::Sparse{Tv}, name::String) where Tv<:VTypes
    isascii(name) || error("non-ASCII name: $name")
    set_print_level(common_struct[Threads.threadid()], 3)
    @isok ccall((@cholmod_name("print_sparse"),:libcholmod), Cint,
            (Ptr{C_Sparse{Tv}}, Ptr{UInt8}, Ptr{UInt8}),
                 A, name, common_struct[Threads.threadid()])
    nothing
end
function print_factor(F::Factor{Tv}, name::String) where Tv<:VTypes
    set_print_level(common_struct[Threads.threadid()], 3)
    @isok ccall((@cholmod_name("print_factor"),:libcholmod), Cint,
            (Ptr{C_Factor{Tv}}, Ptr{UInt8}, Ptr{UInt8}),
                F, name, common_struct[Threads.threadid()])
    nothing
end

### cholmod_matrixops.h ###
function ssmult(A::Sparse{Tv}, B::Sparse{Tv}, stype::Integer,
        values::Bool, sorted::Bool) where Tv<:VRealTypes
    lA = unsafe_load(pointer(A))
    lB = unsafe_load(pointer(B))
    if lA.ncol != lB.nrow
        throw(DimensionMismatch("inner matrix dimensions do not fit"))
    end
    Sparse(ccall((@cholmod_name("ssmult"),:libcholmod),
        Ptr{C_Sparse{Tv}},
            (Ptr{C_Sparse{Tv}}, Ptr{C_Sparse{Tv}}, Cint, Cint,
                Cint, Ptr{UInt8}),
             A, B, stype, values,
                sorted, common_struct[Threads.threadid()]))
end

function norm_sparse(A::Sparse{Tv}, norm::Integer) where Tv<:VTypes
    if norm != 0 && norm != 1
        throw(ArgumentError("norm argument must be either 0 or 1"))
    end
    ccall((@cholmod_name("norm_sparse"), :libcholmod), Cdouble,
            (Ptr{C_Sparse{Tv}}, Cint, Ptr{UInt8}),
                A, norm, common_struct[Threads.threadid()])
end

function horzcat(A::Sparse{Tv}, B::Sparse{Tv}, values::Bool) where Tv<:VRealTypes
    Sparse(ccall((@cholmod_name("horzcat"), :libcholmod),
        Ptr{C_Sparse{Tv}},
            (Ptr{C_Sparse{Tv}}, Ptr{C_Sparse{Tv}}, Cint, Ptr{UInt8}),
             A, B, values, common_struct[Threads.threadid()]))
end

function scale!(S::Dense{Tv}, scale::Integer, A::Sparse{Tv}) where Tv<:VRealTypes
    sS = unsafe_load(pointer(S))
    sA = unsafe_load(pointer(A))
    if sS.ncol != 1 && sS.nrow != 1
        throw(DimensionMismatch("first argument must be a vector"))
    end
    if scale == SCALAR && sS.nrow != 1
        throw(DimensionMismatch("scaling argument must have length one"))
    elseif scale == ROW && sS.nrow*sS.ncol != sA.nrow
        throw(DimensionMismatch("scaling vector has length $(sS.nrow*sS.ncol), " *
            "but matrix has $(sA.nrow) rows."))
    elseif scale == COL && sS.nrow*sS.ncol != sA.ncol
        throw(DimensionMismatch("scaling vector has length $(sS.nrow*sS.ncol), " *
            "but matrix has $(sA.ncol) columns"))
    elseif scale == SYM
        if sA.nrow != sA.ncol
            throw(DimensionMismatch("matrix must be square"))
        elseif sS.nrow*sS.ncol != sA.nrow
            throw(DimensionMismatch("scaling vector has length $(sS.nrow*sS.ncol), " *
                "but matrix has $(sA.ncol) columns and rows"))
        end
    end

    sA = unsafe_load(pointer(A))
    @isok ccall((@cholmod_name("scale"),:libcholmod), Cint,
            (Ptr{C_Dense{Tv}}, Cint, Ptr{C_Sparse{Tv}}, Ptr{UInt8}),
                S, scale, A, common_struct[Threads.threadid()])
    A
end

function sdmult!(A::Sparse{Tv}, transpose::Bool,
        α::Number, β::Number, X::Dense{Tv}, Y::Dense{Tv}) where Tv<:VTypes
    m, n = size(A)
    nc = transpose ? m : n
    nr = transpose ? n : m
    if nc != size(X, 1)
        throw(DimensionMismatch("incompatible dimensions, $nc and $(size(X,1))"))
    end
    @isok ccall((@cholmod_name("sdmult"),:libcholmod), Cint,
            (Ptr{C_Sparse{Tv}}, Cint,
             Ref{ComplexF64}, Ref{ComplexF64},
             Ptr{C_Dense{Tv}}, Ptr{C_Dense{Tv}}, Ptr{UInt8}),
                A, transpose, α, β, X, Y, common_struct[Threads.threadid()])
    Y
end

function vertcat(A::Sparse{Tv}, B::Sparse{Tv}, values::Bool) where Tv<:VRealTypes
    Sparse(ccall((@cholmod_name("vertcat"), :libcholmod),
            Ptr{C_Sparse{Tv}},
            (Ptr{C_Sparse{Tv}}, Ptr{C_Sparse{Tv}}, Cint, Ptr{UInt8}),
                A, B, values, common_struct[Threads.threadid()]))
end

function symmetry(A::Sparse{Tv}, option::Integer) where Tv<:VTypes
    xmatched = Ref{SuiteSparse_long}()
    pmatched = Ref{SuiteSparse_long}()
    nzoffdiag = Ref{SuiteSparse_long}()
    nzdiag = Ref{SuiteSparse_long}()
    rv = ccall((@cholmod_name("symmetry"), :libcholmod), Cint,
            (Ptr{C_Sparse{Tv}}, Cint, Ptr{SuiteSparse_long}, Ptr{SuiteSparse_long},
                Ptr{SuiteSparse_long}, Ptr{SuiteSparse_long}, Ptr{UInt8}),
                    A, option, xmatched, pmatched,
                        nzoffdiag, nzdiag, common_struct[Threads.threadid()])
    rv, xmatched[], pmatched[], nzoffdiag[], nzdiag[]
end

# cholmod_cholesky.h
# For analyze, analyze_p, and factorize_p!, the Common argument must be
# supplied in order to control if the factorization is LLt or LDLt
function analyze(A::Sparse{Tv}, cmmn::Vector{UInt8}) where Tv<:VTypes
    Factor(ccall((@cholmod_name("analyze"),:libcholmod),
        Ptr{C_Factor{Tv}},
            (Ptr{C_Sparse{Tv}}, Ptr{UInt8}),
                A, cmmn))
end
function analyze_p(A::Sparse{Tv}, perm::Vector{SuiteSparse_long},
                   cmmn::Vector{UInt8}) where Tv<:VTypes
    length(perm) != size(A,1) && throw(BoundsError())
    Factor(ccall((@cholmod_name("analyze_p"),:libcholmod),
            Ptr{C_Factor{Tv}},
            (Ptr{C_Sparse{Tv}}, Ptr{SuiteSparse_long}, Ptr{SuiteSparse_long},
                Csize_t, Ptr{UInt8}),
                A, perm, C_NULL, 0, cmmn))
end
function factorize!(A::Sparse{Tv}, F::Factor{Tv}, cmmn::Vector{UInt8}) where Tv<:VTypes
    @isok ccall((@cholmod_name("factorize"),:libcholmod), Cint,
        (Ptr{C_Sparse{Tv}}, Ptr{C_Factor{Tv}}, Ptr{UInt8}),
            A, F, cmmn)
    F
end
function factorize_p!(A::Sparse{Tv}, β::Real, F::Factor{Tv}, cmmn::Vector{UInt8}) where Tv<:VTypes
    # note that β is passed as a complex number (double beta[2]),
    # but the CHOLMOD manual says that only beta[0] (real part) is used
    @isok ccall((@cholmod_name("factorize_p"),:libcholmod), Cint,
        (Ptr{C_Sparse{Tv}}, Ref{ComplexF64}, Ptr{SuiteSparse_long}, Csize_t,
         Ptr{C_Factor{Tv}}, Ptr{UInt8}),
            A, β, C_NULL, 0, F, cmmn)
    F
end

function solve(sys::Integer, F::Factor{Tv}, B::Dense{Tv}) where Tv<:VTypes
    if size(F,1) != size(B,1)
        throw(DimensionMismatch("LHS and RHS should have the same number of rows. " *
            "LHS has $(size(F,1)) rows, but RHS has $(size(B,1)) rows."))
    end
    if !issuccess(F)
        s = unsafe_load(pointer(F))
        if s.is_ll == 1
            throw(LinearAlgebra.PosDefException(s.minor))
        else
            throw(LinearAlgebra.ZeroPivotException(s.minor))
        end
    end
    Dense(ccall((@cholmod_name("solve"),:libcholmod), Ptr{C_Dense{Tv}},
            (Cint, Ptr{C_Factor{Tv}}, Ptr{C_Dense{Tv}}, Ptr{UInt8}),
                sys, F, B, common_struct[Threads.threadid()]))
end

function spsolve(sys::Integer, F::Factor{Tv}, B::Sparse{Tv}) where Tv<:VTypes
    if size(F,1) != size(B,1)
        throw(DimensionMismatch("LHS and RHS should have the same number of rows. " *
            "LHS has $(size(F,1)) rows, but RHS has $(size(B,1)) rows."))
    end
    Sparse(ccall((@cholmod_name("spsolve"),:libcholmod),
        Ptr{C_Sparse{Tv}},
            (Cint, Ptr{C_Factor{Tv}}, Ptr{C_Sparse{Tv}}, Ptr{UInt8}),
                sys, F, B, common_struct[Threads.threadid()]))
end

# Autodetects the types
function read_sparse(file::Libc.FILE, ::Type{SuiteSparse_long})
    ptr = ccall((@cholmod_name("read_sparse"), :libcholmod),
        Ptr{C_Sparse{Cvoid}},
            (Ptr{Cvoid}, Ptr{UInt8}),
                file.ptr, common_struct[Threads.threadid()])
    if ptr == C_NULL
        throw(ArgumentError("sparse matrix construction failed. Check that input file is valid."))
    end
    Sparse(ptr)
end

function read_sparse(file::IO, T)
    cfile = Libc.FILE(file)
    try return read_sparse(cfile, T)
    finally close(cfile)
    end
end

function get_perm(F::Factor)
    s = unsafe_load(pointer(F))
    p = unsafe_wrap(Array, s.Perm, s.n, own = false)
    p .+ 1
end
get_perm(FC::FactorComponent) = get_perm(Factor(FC))

#########################
# High level interfaces #
#########################

# Conversion/construction
function Dense{T}(A::StridedVecOrMat) where T<:VTypes
    d = allocate_dense(size(A, 1), size(A, 2), stride(A, 2), T)
    GC.@preserve d begin
        s = unsafe_load(pointer(d))
        for (i, c) in enumerate(eachindex(A))
            unsafe_store!(s.x, A[c], i)
        end
    end
    d
end
function Dense{T}(A::Union{Adjoint{<:Any, <:StridedVecOrMat}, Transpose{<:Any, <:StridedVecOrMat}}) where T<:VTypes
    d = allocate_dense(size(A, 1), size(A, 2), size(A, 1), T)
    GC.@preserve d begin
        s = unsafe_load(pointer(d))
        for (i, c) in enumerate(eachindex(A))
            unsafe_store!(s.x, A[c], i)
        end
    end
    d
end
function Dense(A::Union{StridedVecOrMat, Adjoint{<:Any, <:StridedVecOrMat}, Transpose{<:Any, <:StridedVecOrMat}})
    T = promote_type(eltype(A), Float64)
    return Dense{T}(A)
end
Dense(A::Sparse) = sparse_to_dense(A)

# This constructior assumes zero based colptr and rowval
function Sparse(m::Integer, n::Integer,
        colptr0::Vector{SuiteSparse_long}, rowval0::Vector{SuiteSparse_long},
        nzval::Vector{Tv}, stype) where Tv<:VTypes
    # checks
    ## length of input
    if length(colptr0) <= n
        throw(ArgumentError("length of colptr0 must be at least n + 1 = $(n + 1) but was $(length(colptr0))"))
    end
    if colptr0[n + 1] > length(rowval0)
        throw(ArgumentError("length of rowval0 is $(length(rowval0)) but value of colptr0 requires length to be at least $(colptr0[n + 1])"))
    end
    if colptr0[n + 1] > length(nzval)
        throw(ArgumentError("length of nzval is $(length(nzval)) but value of colptr0 requires length to be at least $(colptr0[n + 1])"))
    end
    ## columns are sorted
    iss = true
    for i = 2:length(colptr0)
        if !issorted(view(rowval0, colptr0[i - 1] + 1:colptr0[i]))
            iss = false
            break
        end
    end

    o = allocate_sparse(m, n, colptr0[n + 1], iss, true, stype, Tv)
    s = unsafe_load(pointer(o))

    unsafe_copyto!(s.p, pointer(colptr0), n + 1)
    unsafe_copyto!(s.i, pointer(rowval0), colptr0[n + 1])
    unsafe_copyto!(s.x, pointer(nzval) , colptr0[n + 1])

    @isok check_sparse(o)

    return o
end

function Sparse(m::Integer, n::Integer,
        colptr0::Vector{SuiteSparse_long},
        rowval0::Vector{SuiteSparse_long},
        nzval::Vector{<:VTypes})
    o = Sparse(m, n, colptr0, rowval0, nzval, 0)

    # sort indices
    sort!(o)

    # check if array is symmetric and change stype if it is
    if ishermitian(o)
        change_stype!(o, -1)
    end
    o
end

function Sparse{Tv}(A::SparseMatrixCSC, stype::Integer) where Tv<:VTypes
    ## Check length of input. This should never fail but see #20024
    if length(getcolptr(A)) <= size(A, 2)
        throw(ArgumentError("length of colptr must be at least size(A,2) + 1 = $(size(A, 2) + 1) but was $(length(getcolptr(A)))"))
    end
    if nnz(A) > length(rowvals(A))
        throw(ArgumentError("length of rowval is $(length(rowvals(A))) but value of colptr requires length to be at least $(nnz(A))"))
    end
    if nnz(A) > length(nonzeros(A))
        throw(ArgumentError("length of nzval is $(length(nonzeros(A))) but value of colptr requires length to be at least $(nnz(A))"))
    end

    o = allocate_sparse(size(A, 1), size(A, 2), nnz(A), true, true, stype, Tv)
    s = unsafe_load(pointer(o))
    for i = 1:(size(A, 2) + 1)
        unsafe_store!(s.p, getcolptr(A)[i] - 1, i)
    end
    for i = 1:nnz(A)
        unsafe_store!(s.i, rowvals(A)[i] - 1, i)
    end
    if Tv <: Complex && stype != 0
        # Need to remove any non real elements in the diagonal because, in contrast to
        # BLAS/LAPACK these are not ignored by CHOLMOD. If even tiny imaginary parts are
        # present CHOLMOD will fail with a non-positive definite/zero pivot error.
        for j = 1:size(A, 2)
            for ip = getcolptr(A)[j]:getcolptr(A)[j + 1] - 1
                v = nonzeros(A)[ip]
                unsafe_store!(s.x, rowvals(A)[ip] == j ? Complex(real(v)) : v, ip)
            end
        end
    elseif Tv == eltype(nonzeros(A))
        unsafe_copyto!(s.x, pointer(nonzeros(A)), nnz(A))
    else
        for i = 1:nnz(A)
            unsafe_store!(s.x, nonzeros(A)[i], i)
        end
    end

    @isok check_sparse(o)

    return o
end

# handle promotion
function Sparse(A::SparseMatrixCSC{Tv,SuiteSparse_long}, stype::Integer) where {Tv}
    T = promote_type(Tv, Float64)
    return Sparse{T}(A, stype)
end

# convert SparseVectors into CHOLMOD Sparse types through a mx1 CSC matrix
Sparse(A::SparseVector) = Sparse(SparseMatrixCSC(A))
function Sparse(A::SparseMatrixCSC)
    o = Sparse(A, 0)
    # check if array is symmetric and change stype if it is
    if ishermitian(o)
        change_stype!(o, -1)
    end
    o
end

Sparse(A::Symmetric{Tv, SparseMatrixCSC{Tv,Ti}}) where {Tv<:Real, Ti} =
    Sparse(A.data, A.uplo == 'L' ? -1 : 1)
Sparse(A::Hermitian{Tv,SparseMatrixCSC{Tv,Ti}}) where {Tv, Ti} =
    Sparse(A.data, A.uplo == 'L' ? -1 : 1)

Sparse(A::Dense) = dense_to_sparse(A, SuiteSparse_long)
Sparse(L::Factor) = factor_to_sparse!(copy(L))
function Sparse(filename::String)
    open(filename) do f
        return read_sparse(f, SuiteSparse_long)
    end
end

## conversion back to base Julia types
function Matrix{T}(D::Dense{T}) where T
    s = unsafe_load(pointer(D))
    a = Matrix{T}(undef, s.nrow, s.ncol)
    copyto!(a, D)
end

Base.copyto!(dest::Base.PermutedDimsArrays.PermutedDimsArray, src::Dense) = _copy!(dest, src) # ambig
Base.copyto!(dest::Dense{T}, D::Dense{T}) where {T<:VTypes} = _copy!(dest, D)
Base.copyto!(dest::AbstractArray{T}, D::Dense{T}) where {T<:VTypes} = _copy!(dest, D)
Base.copyto!(dest::AbstractArray{T,2}, D::Dense{T}) where {T<:VTypes} = _copy!(dest, D)
Base.copyto!(dest::AbstractArray, D::Dense) = _copy!(dest, D)

function _copy!(dest::AbstractArray, D::Dense)
    require_one_based_indexing(dest)
    s = unsafe_load(pointer(D))
    n = s.nrow*s.ncol
    n <= length(dest) || throw(BoundsError(dest, n))
    if s.d == s.nrow && isa(dest, Array)
        unsafe_copyto!(pointer(dest), s.x, s.d*s.ncol)
    else
        k = 0
        for j = 1:s.ncol
            for i = 1:s.nrow
                dest[k+=1] = unsafe_load(s.x, i + (j - 1)*s.d)
            end
        end
    end
    dest
end
Matrix(D::Dense{T}) where {T} = Matrix{T}(D)
function Vector{T}(D::Dense{T}) where T
    if size(D, 2) > 1
        throw(DimensionMismatch("input must be a vector but had $(size(D, 2)) columns"))
    end
    copyto!(Vector{T}(undef, size(D, 1)), D)
end
Vector(D::Dense{T}) where {T} = Vector{T}(D)

function SparseMatrixCSC{Tv,SuiteSparse_long}(A::Sparse{Tv}) where Tv
    s = unsafe_load(pointer(A))
    if s.stype != 0
        throw(ArgumentError("matrix has stype != 0. Convert to matrix " *
            "with stype == 0 before converting to SparseMatrixCSC"))
    end

    B = SparseMatrixCSC(s.nrow, s.ncol,
        increment(unsafe_wrap(Array, s.p, (s.ncol + 1,), own = false)),
        increment(unsafe_wrap(Array, s.i, (s.nzmax,), own = false)),
        copy(unsafe_wrap(Array, s.x, (s.nzmax,), own = false)))

    if s.sorted == 0
        return SparseArrays.sortSparseMatrixCSC!(B)
    else
        return B
    end
end

function Symmetric{Float64,SparseMatrixCSC{Float64,SuiteSparse_long}}(A::Sparse{Float64})
    s = unsafe_load(pointer(A))
    if !issymmetric(A)
        throw(ArgumentError("matrix is not symmetric"))
    end

    B = Symmetric(SparseMatrixCSC(s.nrow, s.ncol,
        increment(unsafe_wrap(Array, s.p, (s.ncol + 1,), own = false)),
        increment(unsafe_wrap(Array, s.i, (s.nzmax,), own = false)),
        copy(unsafe_wrap(Array, s.x, (s.nzmax,), own = false))), s.stype > 0 ? :U : :L)

    if s.sorted == 0
        return SparseArrays.sortSparseMatrixCSC!(B.data)
    else
        return B
    end
end
convert(T::Type{Symmetric{Float64,SparseMatrixCSC{Float64,SuiteSparse_long}}}, A::Sparse{Float64}) = T(A)

function Hermitian{Tv,SparseMatrixCSC{Tv,SuiteSparse_long}}(A::Sparse{Tv}) where Tv<:VTypes
    s = unsafe_load(pointer(A))
    if !ishermitian(A)
        throw(ArgumentError("matrix is not Hermitian"))
    end

    B = Hermitian(SparseMatrixCSC(s.nrow, s.ncol,
        increment(unsafe_wrap(Array, s.p, (s.ncol + 1,), own = false)),
        increment(unsafe_wrap(Array, s.i, (s.nzmax,), own = false)),
        copy(unsafe_wrap(Array, s.x, (s.nzmax,), own = false))), s.stype > 0 ? :U : :L)

    if s.sorted == 0
        return SparseArrays.sortSparseMatrixCSC!(B.data)
    else
        return B
    end
end
convert(T::Type{Hermitian{Tv,SparseMatrixCSC{Tv,SuiteSparse_long}}}, A::Sparse{Tv}) where {Tv<:VTypes} = T(A)

function sparse(A::Sparse{Float64}) # Notice! Cannot be type stable because of stype
    s = unsafe_load(pointer(A))
    if s.stype == 0
        return SparseMatrixCSC{Float64,SuiteSparse_long}(A)
    end
    return Symmetric{Float64,SparseMatrixCSC{Float64,SuiteSparse_long}}(A)
end
function sparse(A::Sparse{Complex{Float64}}) # Notice! Cannot be type stable because of stype
    s = unsafe_load(pointer(A))
    if s.stype == 0
        return SparseMatrixCSC{Complex{Float64},SuiteSparse_long}(A)
    end
    return Hermitian{Complex{Float64},SparseMatrixCSC{Complex{Float64},SuiteSparse_long}}(A)
end
function sparse(F::Factor)
    s = unsafe_load(pointer(F))
    if s.is_ll != 0
        L = Sparse(F)
        A = sparse(L*L')
    else
        LD = sparse(F.LD)
        L, d = getLd!(LD)
        A = (L * Diagonal(d)) * L'
    end
    SparseArrays.sortSparseMatrixCSC!(A)
    p = get_perm(F)
    if p != [1:s.n;]
        pinv = Vector{Int}(undef, length(p))
        for k = 1:length(p)
            pinv[p[k]] = k
        end
        A = A[pinv,pinv]
    end
    A
end

sparse(D::Dense) = sparse(Sparse(D))

function sparse(FC::FactorComponent{Tv,:L}) where Tv
    F = Factor(FC)
    s = unsafe_load(pointer(F))
    if s.is_ll == 0
        throw(CHOLMODException("sparse: supported only for :LD on LDLt factorizations"))
    end
    sparse(Sparse(F))
end
sparse(FC::FactorComponent{Tv,:LD}) where {Tv} = sparse(Sparse(Factor(FC)))

# Calculate the offset into the stype field of the cholmod_sparse_struct and
# change the value
let offset = fieldoffset(C_Sparse{Float64}, findfirst(name -> name === :stype, fieldnames(C_Sparse{Float64}))::Int)
    global change_stype!
    function change_stype!(A::Sparse, i::Integer)
        unsafe_store!(convert(Ptr{Cint}, pointer(A)), i, div(offset, 4) + 1)
        return A
    end
end

free!(A::Dense)  = free!(pointer(A))
free!(A::Sparse) = free!(pointer(A))
free!(F::Factor) = free!(pointer(F))

eltype(::Type{Dense{T}}) where {T<:VTypes} = T
eltype(::Type{Factor{T}}) where {T<:VTypes} = T
eltype(::Type{Sparse{T}}) where {T<:VTypes} = T

nnz(F::Factor) = nnz(Sparse(F))

function show(io::IO, F::Factor)
    println(io, typeof(F))
    showfactor(io, F)
end

function show(io::IO, FC::FactorComponent)
    println(io, typeof(FC))
    showfactor(io, Factor(FC))
end

function showfactor(io::IO, F::Factor)
    s = unsafe_load(pointer(F))
    print(io, """
        type:    $(s.is_ll!=0 ? "LLt" : "LDLt")
        method:  $(s.is_super!=0 ? "supernodal" : "simplicial")
        maxnnz:  $(Int(s.nzmax))
        nnz:     $(nnz(F))
        success: $(s.minor == size(F, 1))
        """)
end

# getindex not defined for these, so don't use the normal array printer
show(io::IO, ::MIME"text/plain", FC::FactorComponent) = show(io, FC)
show(io::IO, ::MIME"text/plain", F::Factor) = show(io, F)

isvalid(A::Dense) = check_dense(A)
isvalid(A::Sparse) = check_sparse(A)
isvalid(A::Factor) = check_factor(A)

function size(A::Union{Dense,Sparse})
    s = unsafe_load(pointer(A))
    return (Int(s.nrow), Int(s.ncol))
end
function size(F::Factor, i::Integer)
    if i < 1
        throw(ArgumentError("dimension must be positive"))
    end
    s = unsafe_load(pointer(F))
    if i <= 2
        return Int(s.n)
    end
    return 1
end
size(F::Factor) = (size(F, 1), size(F, 2))
axes(A::Union{Dense,Sparse,Factor}) = map(Base.OneTo, size(A))

IndexStyle(::Dense) = IndexLinear()

size(FC::FactorComponent, i::Integer) = size(FC.F, i)
size(FC::FactorComponent) = size(FC.F)

adjoint(FC::FactorComponent{Tv,:L}) where {Tv} = FactorComponent{Tv,:U}(FC.F)
adjoint(FC::FactorComponent{Tv,:U}) where {Tv} = FactorComponent{Tv,:L}(FC.F)
adjoint(FC::FactorComponent{Tv,:PtL}) where {Tv} = FactorComponent{Tv,:UP}(FC.F)
adjoint(FC::FactorComponent{Tv,:UP}) where {Tv} = FactorComponent{Tv,:PtL}(FC.F)
adjoint(FC::FactorComponent{Tv,:D}) where {Tv} = FC
adjoint(FC::FactorComponent{Tv,:LD}) where {Tv} = FactorComponent{Tv,:DU}(FC.F)
adjoint(FC::FactorComponent{Tv,:DU}) where {Tv} = FactorComponent{Tv,:LD}(FC.F)
adjoint(FC::FactorComponent{Tv,:PtLD}) where {Tv} = FactorComponent{Tv,:DUP}(FC.F)
adjoint(FC::FactorComponent{Tv,:DUP}) where {Tv} = FactorComponent{Tv,:PtLD}(FC.F)

function getindex(A::Dense, i::Integer)
    s = unsafe_load(pointer(A))
    0 < i <= s.nrow*s.ncol || throw(BoundsError())
    unsafe_load(s.x, i)
end

IndexStyle(::Sparse) = IndexCartesian()
function getindex(A::Sparse{T}, i0::Integer, i1::Integer) where T
    s = unsafe_load(pointer(A))
    !(1 <= i0 <= s.nrow && 1 <= i1 <= s.ncol) && throw(BoundsError())
    s.stype < 0 && i0 < i1 && return conj(A[i1,i0])
    s.stype > 0 && i0 > i1 && return conj(A[i1,i0])

    r1 = Int(unsafe_load(s.p, i1) + 1)
    r2 = Int(unsafe_load(s.p, i1 + 1))
    (r1 > r2) && return zero(T)
    r1 = Int(searchsortedfirst(unsafe_wrap(Array, s.i, (s.nzmax,), own = false),
        i0 - 1, r1, r2, Base.Order.Forward))
    ((r1 > r2) || (unsafe_load(s.i, r1) + 1 != i0)) ? zero(T) : unsafe_load(s.x, r1)
end

@inline function getproperty(F::Factor, sym::Symbol)
    if sym === :p
        return get_perm(F)
    elseif sym === :ptr
        return getfield(F, :ptr)
    else
        return FactorComponent(F, sym)
    end
end

function getLd!(S::SparseMatrixCSC)
    d = Vector{eltype(S)}(undef, size(S, 1))
    fill!(d, 0)
    col = 1
    for k = 1:nnz(S)
        while k >= getcolptr(S)[col+1]
            col += 1
        end
        if rowvals(S)[k] == col
            d[col] = nonzeros(S)[k]
            nonzeros(S)[k] = 1
        end
    end
    S, d
end

## Multiplication
(*)(A::Sparse, B::Sparse) = ssmult(A, B, 0, true, true)
(*)(A::Sparse, B::Dense) = sdmult!(A, false, 1., 0., B, zeros(size(A, 1), size(B, 2)))
(*)(A::Sparse, B::VecOrMat) = (*)(A, Dense(B))

function *(A::Sparse{Tv}, adjB::Adjoint{Tv,Sparse{Tv}}) where Tv<:VRealTypes
    B = adjB.parent
    if A !== B
        aa1 = transpose_(B, 2)
        ## result of ssmult will have stype==0, contain numerical values and be sorted
        return ssmult(A, aa1, 0, true, true)
    end

    ## The A*A' case is handled by cholmod_aat. This routine requires
    ## A->stype == 0 (storage of upper and lower parts). If necessary
    ## the matrix A is first converted to stype == 0
    s = unsafe_load(pointer(A))
    fset = s.ncol == 0 ? SuiteSparse_long[] : SuiteSparse_long[0:s.ncol-1;]
    if s.stype != 0
        aa1 = copy(A, 0, 1)
        return aat(aa1, fset, 1)
    else
        return aat(A, fset, 1)
    end
end

function *(adjA::Adjoint{<:Any,<:Sparse}, B::Sparse)
    A = adjA.parent
    aa1 = transpose_(A, 2)
    if A === B
        return *(aa1, adjoint(aa1))
    end
    ## result of ssmult will have stype==0, contain numerical values and be sorted
    return ssmult(aa1, B, 0, true, true)
end

*(adjA::Adjoint{<:Any,<:Sparse}, B::Dense) =
    (A = adjA.parent; sdmult!(A, true, 1., 0., B, zeros(size(A, 2), size(B, 2))))
*(adjA::Adjoint{<:Any,<:Sparse}, B::VecOrMat) =
    (A = adjA.parent; *(adjoint(A), Dense(B)))


## Factorization methods

## Compute that symbolic factorization only
function fact_(A::Sparse{<:VTypes}, cm::Array{UInt8};
    perm::Union{Nothing,AbstractVector{SuiteSparse_long}}=nothing,
    postorder::Bool=true, userperm_only::Bool=true)

    sA = unsafe_load(pointer(A))
    sA.stype == 0 && throw(ArgumentError("sparse matrix is not symmetric/Hermitian"))

    if !postorder
        unsafe_store!(common_postorder[Threads.threadid()], 0)
    end

    if perm === nothing || isempty(perm) # TODO: deprecate empty perm
        F = analyze(A, cm)
    else # user permutation provided
        if userperm_only # use perm even if it is worse than AMD
            unsafe_store!(common_nmethods[Threads.threadid()], 1)
        end
        F = analyze_p(A, SuiteSparse_long[p-1 for p in perm], cm)
    end

    return F
end

function cholesky!(F::Factor{Tv}, A::Sparse{Tv};
                   shift::Real=0.0, check::Bool = true) where Tv
    # Makes it an LLt
    unsafe_store!(common_final_ll[Threads.threadid()], 1)

    # Compute the numerical factorization
    factorize_p!(A, shift, F, common_struct[Threads.threadid()])

    check && (issuccess(F) || throw(LinearAlgebra.PosDefException(1)))
    return F
end

"""
    cholesky!(F::Factor, A; shift = 0.0, check = true) -> CHOLMOD.Factor

Compute the Cholesky (``LL'``) factorization of `A`, reusing the symbolic
factorization `F`. `A` must be a [`SparseMatrixCSC`](@ref) or a [`Symmetric`](@ref)/
[`Hermitian`](@ref) view of a `SparseMatrixCSC`. Note that even if `A` doesn't
have the type tag, it must still be symmetric or Hermitian.

See also [`cholesky`](@ref).

!!! note
    This method uses the CHOLMOD library from SuiteSparse, which only supports
    doubles or complex doubles. Input matrices not of those element types will
    be converted to `SparseMatrixCSC{Float64}` or `SparseMatrixCSC{ComplexF64}`
    as appropriate.
"""
cholesky!(F::Factor, A::Union{SparseMatrixCSC{T},
          SparseMatrixCSC{Complex{T}},
          Symmetric{T,SparseMatrixCSC{T,SuiteSparse_long}},
          Hermitian{Complex{T},SparseMatrixCSC{Complex{T},SuiteSparse_long}},
          Hermitian{T,SparseMatrixCSC{T,SuiteSparse_long}}};
          shift = 0.0, check::Bool = true) where {T<:Real} =
    cholesky!(F, Sparse(A); shift = shift, check = check)

function cholesky(A::Sparse; shift::Real=0.0, check::Bool = true,
    perm::Union{Nothing,AbstractVector{SuiteSparse_long}}=nothing)

    cm = defaults(common_struct[Threads.threadid()])
    set_print_level(cm, 0)

    # Compute the symbolic factorization
    F = fact_(A, cm; perm = perm)

    # Compute the numerical factorization
    cholesky!(F, A; shift = shift, check = check)

    return F
end

"""
    cholesky(A; shift = 0.0, check = true, perm = nothing) -> CHOLMOD.Factor

Compute the Cholesky factorization of a sparse positive definite matrix `A`.
`A` must be a [`SparseMatrixCSC`](@ref) or a [`Symmetric`](@ref)/[`Hermitian`](@ref)
view of a `SparseMatrixCSC`. Note that even if `A` doesn't
have the type tag, it must still be symmetric or Hermitian.
If `perm` is not given, a fill-reducing permutation is used.
`F = cholesky(A)` is most frequently used to solve systems of equations with `F\\b`,
but also the methods [`diag`](@ref), [`det`](@ref), and
[`logdet`](@ref) are defined for `F`.
You can also extract individual factors from `F`, using `F.L`.
However, since pivoting is on by default, the factorization is internally
represented as `A == P'*L*L'*P` with a permutation matrix `P`;
using just `L` without accounting for `P` will give incorrect answers.
To include the effects of permutation,
it's typically preferable to extract "combined" factors like `PtL = F.PtL`
(the equivalent of `P'*L`) and `LtP = F.UP` (the equivalent of `L'*P`).

When `check = true`, an error is thrown if the decomposition fails.
When `check = false`, responsibility for checking the decomposition's
validity (via [`issuccess`](@ref)) lies with the user.

Setting the optional `shift` keyword argument computes the factorization of
`A+shift*I` instead of `A`. If the `perm` argument is provided,
it should be a permutation of `1:size(A,1)` giving the ordering to use
(instead of CHOLMOD's default AMD ordering).

# Examples

In the following example, the fill-reducing permutation used is `[3, 2, 1]`.
If `perm` is set to `1:3` to enforce no permutation, the number of nonzero
elements in the factor is 6.
```jldoctest
julia> A = [2 1 1; 1 2 0; 1 0 2]
3×3 Array{Int64,2}:
 2  1  1
 1  2  0
 1  0  2

julia> C = cholesky(sparse(A))
SuiteSparse.CHOLMOD.Factor{Float64}
type:    LLt
method:  simplicial
maxnnz:  5
nnz:     5
success: true

julia> C.p
3-element Array{Int64,1}:
 3
 2
 1

julia> L = sparse(C.L);

julia> Matrix(L)
3×3 Array{Float64,2}:
 1.41421   0.0       0.0
 0.0       1.41421   0.0
 0.707107  0.707107  1.0

julia> L * L' ≈ A[C.p, C.p]
true

julia> P = sparse(1:3, C.p, ones(3))
3×3 SparseMatrixCSC{Float64,Int64} with 3 stored entries:
 ⋅    ⋅   1.0
 ⋅   1.0   ⋅
 1.0   ⋅    ⋅

julia> P' * L * L' * P ≈ A
true

julia> C = cholesky(sparse(A), perm=1:3)
SuiteSparse.CHOLMOD.Factor{Float64}
type:    LLt
method:  simplicial
maxnnz:  6
nnz:     6
success: true

julia> L = sparse(C.L);

julia> Matrix(L)
3×3 Array{Float64,2}:
 1.41421    0.0       0.0
 0.707107   1.22474   0.0
 0.707107  -0.408248  1.1547

julia> L * L' ≈ A
true
```

!!! note
    This method uses the CHOLMOD library from SuiteSparse, which only supports
    doubles or complex doubles. Input matrices not of those element types will
    be converted to `SparseMatrixCSC{Float64}` or `SparseMatrixCSC{ComplexF64}`
    as appropriate.

    Many other functions from CHOLMOD are wrapped but not exported from the
    `Base.SparseArrays.CHOLMOD` module.
"""
cholesky(A::Union{SparseMatrixCSC{T}, SparseMatrixCSC{Complex{T}},
    Symmetric{T,SparseMatrixCSC{T,SuiteSparse_long}},
    Hermitian{Complex{T},SparseMatrixCSC{Complex{T},SuiteSparse_long}},
    Hermitian{T,SparseMatrixCSC{T,SuiteSparse_long}}};
    kws...) where {T<:Real} = cholesky(Sparse(A); kws...)


function ldlt!(F::Factor{Tv}, A::Sparse{Tv};
               shift::Real=0.0, check::Bool = true) where Tv
    cm = defaults(common_struct[Threads.threadid()])
    set_print_level(cm, 0)

    # Makes it an LDLt
    change_factor!(F, false, false, true, false)

    # Compute the numerical factorization
    factorize_p!(A, shift, F, cm)

    check && (issuccess(F) || throw(LinearAlgebra.ZeroPivotException(1)))
    return F
end

"""
    ldlt!(F::Factor, A; shift = 0.0, check = true) -> CHOLMOD.Factor

Compute the ``LDL'`` factorization of `A`, reusing the symbolic factorization `F`.
`A` must be a [`SparseMatrixCSC`](@ref) or a [`Symmetric`](@ref)/[`Hermitian`](@ref)
view of a `SparseMatrixCSC`. Note that even if `A` doesn't
have the type tag, it must still be symmetric or Hermitian.

See also [`ldlt`](@ref).

!!! note
    This method uses the CHOLMOD library from SuiteSparse, which only supports
    doubles or complex doubles. Input matrices not of those element types will
    be converted to `SparseMatrixCSC{Float64}` or `SparseMatrixCSC{ComplexF64}`
    as appropriate.
"""
ldlt!(F::Factor, A::Union{SparseMatrixCSC{T},
    SparseMatrixCSC{Complex{T}},
    Symmetric{T,SparseMatrixCSC{T,SuiteSparse_long}},
    Hermitian{Complex{T},SparseMatrixCSC{Complex{T},SuiteSparse_long}},
    Hermitian{T,SparseMatrixCSC{T,SuiteSparse_long}}};
    shift = 0.0, check::Bool = true) where {T<:Real} =
    ldlt!(F, Sparse(A), shift = shift, check = check)

function ldlt(A::Sparse; shift::Real=0.0, check::Bool = true,
    perm::Union{Nothing,AbstractVector{SuiteSparse_long}}=nothing)

    cm = defaults(common_struct[Threads.threadid()])
    set_print_level(cm, 0)

    # Makes it an LDLt
    unsafe_store!(common_final_ll[Threads.threadid()], 0)
    # Really make sure it's an LDLt by avoiding supernodal factorization
    unsafe_store!(common_supernodal[Threads.threadid()], 0)

    # Compute the symbolic factorization
    F = fact_(A, cm; perm = perm)

    # Compute the numerical factorization
    ldlt!(F, A; shift = shift, check = check)

    return F
end

"""
    ldlt(A; shift = 0.0, check = true, perm=nothing) -> CHOLMOD.Factor

Compute the ``LDL'`` factorization of a sparse matrix `A`.
`A` must be a [`SparseMatrixCSC`](@ref) or a [`Symmetric`](@ref)/[`Hermitian`](@ref)
view of a `SparseMatrixCSC`. Note that even if `A` doesn't
have the type tag, it must still be symmetric or Hermitian.
A fill-reducing permutation is used. `F = ldlt(A)` is most frequently
used to solve systems of equations `A*x = b` with `F\\b`. The returned
factorization object `F` also supports the methods [`diag`](@ref),
[`det`](@ref), [`logdet`](@ref), and [`inv`](@ref).
You can extract individual factors from `F` using `F.L`.
However, since pivoting is on by default, the factorization is internally
represented as `A == P'*L*D*L'*P` with a permutation matrix `P`;
using just `L` without accounting for `P` will give incorrect answers.
To include the effects of permutation, it is typically preferable to extract
"combined" factors like `PtL = F.PtL` (the equivalent of
`P'*L`) and `LtP = F.UP` (the equivalent of `L'*P`).
The complete list of supported factors is `:L, :PtL, :D, :UP, :U, :LD, :DU, :PtLD, :DUP`.

When `check = true`, an error is thrown if the decomposition fails.
When `check = false`, responsibility for checking the decomposition's
validity (via [`issuccess`](@ref)) lies with the user.

Setting the optional `shift` keyword argument computes the factorization of
`A+shift*I` instead of `A`. If the `perm` argument is provided,
it should be a permutation of `1:size(A,1)` giving the ordering to use
(instead of CHOLMOD's default AMD ordering).

!!! note
    This method uses the CHOLMOD library from SuiteSparse, which only supports
    doubles or complex doubles. Input matrices not of those element types will
    be converted to `SparseMatrixCSC{Float64}` or `SparseMatrixCSC{ComplexF64}`
    as appropriate.

    Many other functions from CHOLMOD are wrapped but not exported from the
    `Base.SparseArrays.CHOLMOD` module.
"""
ldlt(A::Union{SparseMatrixCSC{T},SparseMatrixCSC{Complex{T}},
    Symmetric{T,SparseMatrixCSC{T,SuiteSparse_long}},
    Hermitian{Complex{T},SparseMatrixCSC{Complex{T},SuiteSparse_long}},
    Hermitian{T,SparseMatrixCSC{T,SuiteSparse_long}}};
    kws...) where {T<:Real} = ldlt(Sparse(A); kws...)

## Rank updates

"""
    lowrankupdowndate!(F::Factor, C::Sparse, update::Cint)

Update an `LDLt` or `LLt` Factorization `F` of `A` to a factorization of `A ± C*C'`.

If sparsity preserving factorization is used, i.e. `L*L' == P*A*P'` then the new
factor will be `L*L' == P*A*P' + C'*C`

update: `Cint(1)` for `A + CC'`, `Cint(0)` for `A - CC'`
"""
function lowrankupdowndate!(F::Factor{Tv}, C::Sparse{Tv}, update::Cint) where Tv<:VTypes
    lF = unsafe_load(pointer(F))
    lC = unsafe_load(pointer(C))
    if lF.n != lC.nrow
        throw(DimensionMismatch("matrix dimensions do not fit"))
    end
    @isok ccall((@cholmod_name("updown"), :libcholmod), Cint,
        (Cint, Ptr{C_Sparse{Tv}}, Ptr{C_Factor{Tv}}, Ptr{Cvoid}),
        update, C, F, common_struct[Threads.threadid()])
    F
end

#Helper functions for rank updates
lowrank_reorder(V::AbstractArray,p) = Sparse(sparse(V[p,:]))
lowrank_reorder(V::AbstractSparseArray,p) = Sparse(V[p,:])

"""
    lowrankupdate!(F::Factor, C)

Update an `LDLt` or `LLt` Factorization `F` of `A` to a factorization of `A + C*C'`.

`LLt` factorizations are converted to `LDLt`.

See also [`lowrankupdate`](@ref), [`lowrankdowndate`](@ref), [`lowrankdowndate!`](@ref).
"""
function lowrankupdate!(F::Factor{Tv}, V::AbstractArray{Tv}) where Tv<:VTypes
    #Reorder and copy V to account for permutation
    C = lowrank_reorder(V, get_perm(F))
    lowrankupdowndate!(F, C, Cint(1))
end

"""
    lowrankdowndate!(F::Factor, C)

Update an `LDLt` or `LLt` Factorization `F` of `A` to a factorization of `A - C*C'`.

`LLt` factorizations are converted to `LDLt`.

See also [`lowrankdowndate`](@ref), [`lowrankupdate`](@ref), [`lowrankupdate!`](@ref).
"""
function lowrankdowndate!(F::Factor{Tv}, V::AbstractArray{Tv}) where Tv<:VTypes
    #Reorder and copy V to account for permutation
    C = lowrank_reorder(V, get_perm(F))
    lowrankupdowndate!(F, C, Cint(0))
end

"""
    lowrankupdate(F::Factor, C) -> FF::Factor

Get an `LDLt` Factorization of `A + C*C'` given an `LDLt` or `LLt` factorization `F` of `A`.

The returned factor is always an `LDLt` factorization.

See also [`lowrankupdate!`](@ref), [`lowrankdowndate`](@ref), [`lowrankdowndate!`](@ref).
"""
lowrankupdate(F::Factor{Tv}, V::AbstractArray{Tv}) where {Tv<:VTypes} =
    lowrankupdate!(copy(F), V)

"""
    lowrankupdate(F::Factor, C) -> FF::Factor

Get an `LDLt` Factorization of `A + C*C'` given an `LDLt` or `LLt` factorization `F` of `A`.

The returned factor is always an `LDLt` factorization.

See also [`lowrankdowndate!`](@ref), [`lowrankupdate`](@ref), [`lowrankupdate!`](@ref).
"""
lowrankdowndate(F::Factor{Tv}, V::AbstractArray{Tv}) where {Tv<:VTypes} =
    lowrankdowndate!(copy(F), V)

## Solvers

for (T, f) in ((:Dense, :solve), (:Sparse, :spsolve))
    @eval begin
        # Solve Lx = b and L'x=b where A = L*L'
        function (\)(L::FactorComponent{T,:L}, B::$T) where T
            ($f)(CHOLMOD_L, Factor(L), B)
        end
        function (\)(L::FactorComponent{T,:U}, B::$T) where T
            ($f)(CHOLMOD_Lt, Factor(L), B)
        end
        # Solve PLx = b and L'P'x=b where A = P*L*L'*P'
        function (\)(L::FactorComponent{T,:PtL}, B::$T) where T
            F = Factor(L)
            ($f)(CHOLMOD_L, F, ($f)(CHOLMOD_P, F, B)) # Confusingly, CHOLMOD_P solves P'x = b
        end
        function (\)(L::FactorComponent{T,:UP}, B::$T) where T
            F = Factor(L)
            ($f)(CHOLMOD_Pt, F, ($f)(CHOLMOD_Lt, F, B))
        end
        # Solve various equations for A = L*D*L' and A = P*L*D*L'*P'
        function (\)(L::FactorComponent{T,:D}, B::$T) where T
            ($f)(CHOLMOD_D, Factor(L), B)
        end
        function (\)(L::FactorComponent{T,:LD}, B::$T) where T
            ($f)(CHOLMOD_LD, Factor(L), B)
        end
        function (\)(L::FactorComponent{T,:DU}, B::$T) where T
            ($f)(CHOLMOD_DLt, Factor(L), B)
        end
        function (\)(L::FactorComponent{T,:PtLD}, B::$T) where T
            F = Factor(L)
            ($f)(CHOLMOD_LD, F, ($f)(CHOLMOD_P, F, B))
        end
        function (\)(L::FactorComponent{T,:DUP}, B::$T) where T
            F = Factor(L)
            ($f)(CHOLMOD_Pt, F, ($f)(CHOLMOD_DLt, F, B))
        end
    end
end

SparseVecOrMat{Tv,Ti} = Union{SparseVector{Tv,Ti}, SparseMatrixCSC{Tv,Ti}}

function (\)(L::FactorComponent, b::Vector)
    reshape(Matrix(L\Dense(b)), length(b))
end
function (\)(L::FactorComponent, B::Matrix)
    Matrix(L\Dense(B))
end
function (\)(L::FactorComponent, B::SparseVecOrMat)
    sparse(L\Sparse(B,0))
end

\(adjL::Adjoint{<:Any,<:FactorComponent}, B::Union{VecOrMat,SparseVecOrMat}) = (L = adjL.parent; adjoint(L)\B)

(\)(L::Factor{T}, B::Dense{T}) where {T<:VTypes} = solve(CHOLMOD_A, L, B)
# Explicit typevars are necessary to avoid ambiguities with defs in linalg/factorizations.jl
# Likewise the two following explicit Vector and Matrix defs (rather than a single VecOrMat)
(\)(L::Factor{T}, B::Vector{Complex{T}}) where {T<:Float64} = complex.(L\real(B), L\imag(B))
(\)(L::Factor{T}, B::Matrix{Complex{T}}) where {T<:Float64} = complex.(L\real(B), L\imag(B))
(\)(L::Factor{T}, B::Adjoint{<:Any, <:Matrix{Complex{T}}}) where {T<:Float64} = complex.(L\real(B), L\imag(B))
(\)(L::Factor{T}, B::Transpose{<:Any, <:Matrix{Complex{T}}}) where {T<:Float64} = complex.(L\real(B), L\imag(B))

(\)(L::Factor{T}, b::StridedVector) where {T<:VTypes} = Vector(L\Dense{T}(b))
(\)(L::Factor{T}, B::StridedMatrix) where {T<:VTypes} = Matrix(L\Dense{T}(B))
(\)(L::Factor{T}, B::Adjoint{<:Any, <:StridedMatrix}) where {T<:VTypes} = Matrix(L\Dense{T}(B))
(\)(L::Factor{T}, B::Transpose{<:Any, <:StridedMatrix}) where {T<:VTypes} = Matrix(L\Dense{T}(B))

(\)(L::Factor, B::Sparse) = spsolve(CHOLMOD_A, L, B)
# When right hand side is sparse, we have to ensure that the rhs is not marked as symmetric.
(\)(L::Factor, B::SparseMatrixCSC) = sparse(spsolve(CHOLMOD_A, L, Sparse(B, 0)))
(\)(L::Factor, B::SparseVector) = sparse(spsolve(CHOLMOD_A, L, Sparse(B)))

\(adjL::Adjoint{<:Any,<:Factor}, B::Dense) = (L = adjL.parent; solve(CHOLMOD_A, L, B))
\(adjL::Adjoint{<:Any,<:Factor}, B::Sparse) = (L = adjL.parent; spsolve(CHOLMOD_A, L, B))
\(adjL::Adjoint{<:Any,<:Factor}, B::SparseVecOrMat) = (L = adjL.parent; \(adjoint(L), Sparse(B)))

function \(adjL::Adjoint{<:Any,<:Factor}, b::StridedVector)
    L = adjL.parent
    return Vector(solve(CHOLMOD_A, L, Dense(b)))
end
function \(adjL::Adjoint{<:Any,<:Factor}, B::StridedMatrix)
    L = adjL.parent
    return Matrix(solve(CHOLMOD_A, L, Dense(B)))
end

const RealHermSymComplexHermF64SSL = Union{
    Symmetric{Float64,SparseMatrixCSC{Float64,SuiteSparse_long}},
    Hermitian{Float64,SparseMatrixCSC{Float64,SuiteSparse_long}},
    Hermitian{Complex{Float64},SparseMatrixCSC{Complex{Float64},SuiteSparse_long}}}
const StridedVecOrMatInclAdjAndTrans = Union{StridedVecOrMat, Adjoint{<:Any, <:StridedVecOrMat}, Transpose{<:Any, <:StridedVecOrMat}}
function \(A::RealHermSymComplexHermF64SSL, B::StridedVecOrMatInclAdjAndTrans)
    F = cholesky(A; check = false)
    if issuccess(F)
        return \(F, B)
    else
        ldlt!(F, A; check = false)
        if issuccess(F)
            return \(F, B)
        else
            return \(lu(SparseMatrixCSC{eltype(A), SuiteSparse_long}(A)), B)
        end
    end
end
function \(adjA::Adjoint{<:Any,<:RealHermSymComplexHermF64SSL}, B::StridedVecOrMatInclAdjAndTrans)
    A = adjA.parent
    F = cholesky(A; check = false)
    if issuccess(F)
        return \(adjoint(F), B)
    else
        ldlt!(F, A; check = false)
        if issuccess(F)
            return \(adjoint(F), B)
        else
            return \(adjoint(lu(SparseMatrixCSC{eltype(A), SuiteSparse_long}(A))), B)
        end
    end
end

## Other convenience methods
function diag(F::Factor{Tv}) where Tv
    f = unsafe_load(pointer(F))
    fsuper = f.super
    fpi = f.pi
    res = Base.zeros(Tv, Int(f.n))
    xv  = f.x
    if f.is_super!=0
        px = f.px
        pos = 1
        for i in 1:f.nsuper
            base = unsafe_load(px, i) + 1
            res[pos] = unsafe_load(xv, base)
            pos += 1
            for j in 1:unsafe_load(fsuper, i + 1) - unsafe_load(fsuper, i) - 1
                res[pos] = unsafe_load(xv, base + j*(unsafe_load(fpi, i + 1) -
                    unsafe_load(fpi, i) + 1))
                pos += 1
            end
        end
    else
        c0 = f.p
        r0 = f.i
        xv = f.x
        for j in 1:f.n
            jj = unsafe_load(c0, j) + 1
            @assert(unsafe_load(r0, jj) == j - 1)
            res[j] = unsafe_load(xv, jj)
        end
    end
    res
end

function logdet(F::Factor{Tv}) where Tv<:VTypes
    f = unsafe_load(pointer(F))
    res = zero(Tv)
    for d in diag(F); res += log(abs(d)) end
    f.is_ll != 0 ? 2res : res
end

det(L::Factor) = exp(logdet(L))

function issuccess(F::Factor)
    s = unsafe_load(pointer(F))
    return s.minor == size(F, 1)
end

function isposdef(F::Factor)
    if issuccess(F)
        s = unsafe_load(pointer(F))
        if s.is_ll == 1
            return true
        else
            # try conversion to LLt
            change_factor!(F, true, s.is_super, true, s.is_monotonic)
            b = issuccess(F)
            # convert back
            change_factor!(F, false, s.is_super, true, s.is_monotonic)
            return b
        end
    else
        return false
    end
end

function ishermitian(A::Sparse{Float64})
    s = unsafe_load(pointer(A))
    if s.stype != 0
        return true
    else
        i = symmetry(A, 1)[1]
        if i < 0
            throw(CHOLMODException("negative value returned from CHOLMOD's symmetry function. This
                is either because the indices are not sorted or because of a memory error"))
        end
        return i == MM_SYMMETRIC || i == MM_SYMMETRIC_POSDIAG
    end
end
function ishermitian(A::Sparse{Complex{Float64}})
    s = unsafe_load(pointer(A))
    if s.stype != 0
        return true
    else
        i = symmetry(A, 1)[1]
        if i < 0
            throw(CHOLMODException("negative value returned from CHOLMOD's symmetry function. This
                is either because the indices are not sorted or because of a memory error"))
        end
        return i == MM_HERMITIAN || i == MM_HERMITIAN_POSDIAG
    end
end

(*)(A::Symmetric{Float64,SparseMatrixCSC{Float64,Ti}},
    B::SparseVecOrMat{Float64,Ti}) where {Ti} = sparse(Sparse(A)*Sparse(B))
(*)(A::Hermitian{Complex{Float64},SparseMatrixCSC{Complex{Float64},Ti}},
    B::SparseVecOrMat{Complex{Float64},Ti}) where {Ti} = sparse(Sparse(A)*Sparse(B))
(*)(A::Hermitian{Float64,SparseMatrixCSC{Float64,Ti}},
    B::SparseVecOrMat{Float64,Ti}) where {Ti} = sparse(Sparse(A)*Sparse(B))

(*)(A::SparseVecOrMat{Float64,Ti},
    B::Symmetric{Float64,SparseMatrixCSC{Float64,Ti}}) where {Ti} = sparse(Sparse(A)*Sparse(B))
(*)(A::SparseVecOrMat{Complex{Float64},Ti},
    B::Hermitian{Complex{Float64},SparseMatrixCSC{Complex{Float64},Ti}}) where {Ti} = sparse(Sparse(A)*Sparse(B))
(*)(A::SparseVecOrMat{Float64,Ti},
    B::Hermitian{Float64,SparseMatrixCSC{Float64,Ti}}) where {Ti} = sparse(Sparse(A)*Sparse(B))

end #module
