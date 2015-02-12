module CHOLMOD

import Base: (*), convert, copy, eltype, getindex, show, size

import Base.LinAlg: (\), A_mul_Bc, A_mul_Bt, Ac_ldiv_B, Ac_mul_B, At_ldiv_B, At_mul_B,
                 cholfact, cholfact!, det, diag, ishermitian, isposdef,
                 issym, ldltfact, logdet

import Base.SparseMatrix: sparse

export
    Dense,
    Factor,
    Sparse

using Base.SparseMatrix: AbstractSparseMatrix, SparseMatrixCSC, increment, increment!, indtype, decrement, decrement!

#########
# Setup #
#########

include("cholmod_h.jl")

## macro to generate the name of the C function according to the integer type
macro cholmod_name(nm,typ) string("cholmod_", eval(typ) == Int64 ? "l_" : "", nm) end

for Ti in IndexTypes
    @eval begin
        function common(::Type{$Ti})
            a = fill(0xff, cholmod_com_sz)
            @isok ccall((@cholmod_name "start" $Ti
                , :libcholmod), Cint, (Ptr{UInt8},), a)
            set_print_level(a, 0) # no printing from CHOLMOD by default
            a
        end
    end
end

### These offsets are defined in SuiteSparse_wrapper.c
const cholmod_com_offsets = Array(Csize_t, 19)
ccall((:jl_cholmod_common_offsets, :libsuitesparse_wrapper),
      Void, (Ptr{Csize_t},), cholmod_com_offsets)
const common_final_ll = (1:4) + cholmod_com_offsets[7]
const common_print = (1:4) + cholmod_com_offsets[13]
const common_itype = (1:4) + cholmod_com_offsets[18]
const common_dtype = (1:4) + cholmod_com_offsets[19]

function set_print_level(cm::Array{UInt8}, lev::Integer)
    cm[common_print] = reinterpret(UInt8, [int32(lev)])
end

####################
# Type definitions #
####################

# The three core data types for CHOLMOD: Dense, Sparse and Factor. CHOLMOD
# manages the memory, so the Julia versions only wrap a pointer to a struct.
# Therefore finalizers should be registret each time a pointer is returned from
# CHOLMOD.

# Dense
immutable C_Dense{T<:VTypes}
    nrow::Csize_t
    ncol::Csize_t
    nzmax::Csize_t
    d::Csize_t
    x::Ptr{T}
    z::Ptr{Void}
    xtype::Cint
    dtype::Cint
end

type Dense{T<:VTypes} <: DenseMatrix{T}
    p::Ptr{C_Dense{T}}
end

# Sparse
immutable C_Sparse{Tv<:VTypes,Ti<:ITypes}
    nrow::Csize_t
    ncol::Csize_t
    nzmax::Csize_t
    p::Ptr{Ti}
    i::Ptr{Ti}
    nz::Ptr{Ti}
    x::Ptr{Tv}
    z::Ptr{Void}
    stype::Cint
    itype::Cint
    xtype::Cint
    dtype::Cint
    sorted::Cint
    packed::Cint
end

# Corresponds to the exact definition of cholmod_sparse_struct in the library.
# Useful when reading matrices of unknown type from files as in
# cholmod_read_sparse
immutable C_SparseVoid
    nrow::Csize_t
    ncol::Csize_t
    nzmax::Csize_t
    p::Ptr{Void}
    i::Ptr{Void}
    nz::Ptr{Void}
    x::Ptr{Void}
    z::Ptr{Void}
    stype::Cint
    itype::Cint
    xtype::Cint
    dtype::Cint
    sorted::Cint
    packed::Cint
end

type Sparse{Tv<:VTypes,Ti<:ITypes} <: AbstractSparseMatrix{Tv,Ti}
    p::Ptr{C_Sparse{Tv,Ti}}
end

# Factor

if version >= v"2.1.0" # CHOLMOD version 2.1.0 or later
    immutable C_Factor{Tv<:VTypes,Ti<:ITypes}
        n::Csize_t
        minor::Csize_t
        Perm::Ptr{Ti}
        ColCount::Ptr{Ti}
        IPerm::Ptr{Ti}        # this pointer was added in verison 2.1.0
        nzmax::Csize_t
        p::Ptr{Ti}
        i::Ptr{Ti}
        x::Ptr{Tv}
        z::Ptr{Void}
        nz::Ptr{Ti}
        next::Ptr{Ti}
        prev::Ptr{Ti}
        nsuper::Csize_t
        ssize::Csize_t
        xsize::Csize_t
        maxcsize::Csize_t
        maxesize::Csize_t
        super::Ptr{Ti}
        pi::Ptr{Ti}
        px::Ptr{Ti}
        s::Ptr{Ti}
        ordering::Cint
        is_ll::Cint
        is_super::Cint
        is_monotonic::Cint
        itype::Cint
        xtype::Cint
        dtype::Cint
    end
else
    immutable C_Factor{Tv<:VTypes,Ti<:ITypes}
        n::Csize_t
        minor::Csize_t
        Perm::Ptr{Ti}
        ColCount::Ptr{Ti}
        nzmax::Csize_t
        p::Ptr{Ti}
        i::Ptr{Ti}
        x::Ptr{Tv}
        z::Ptr{Void}
        nz::Ptr{Ti}
        next::Ptr{Ti}
        prev::Ptr{Ti}
        nsuper::Csize_t
        ssize::Csize_t
        xsize::Csize_t
        maxcsize::Csize_t
        maxesize::Csize_t
        super::Ptr{Ti}
        pi::Ptr{Ti}
        px::Ptr{Ti}
        s::Ptr{Ti}
        ordering::Cint
        is_ll::Cint
        is_super::Cint
        is_monotonic::Cint
        itype::Cint
        xtype::Cint
        dtype::Cint
    end
end

type Factor{Tv,Ti} <: Factorization{Tv}
    p::Ptr{C_Factor{Tv,Ti}}
end

#################
# Thin wrappers #
#################

# Dense wrappers
## Note! Integer type defaults to Cint, but this is actually not necessary, but
## making this a choice would require another type parameter in the Dense type

### cholmod_core_h ###
function allocate_dense(nrow::Integer, ncol::Integer, d::Integer, ::Type{Float64})
    d = Dense(ccall((:cholmod_allocate_dense, :libcholmod), Ptr{C_Dense{Float64}},
        (Csize_t, Csize_t, Csize_t, Cint, Ptr{Void}),
        nrow, ncol, d, REAL, common(Cint)))
    finalizer(d, free!)
    d
end
function allocate_dense(nrow::Integer, ncol::Integer, d::Integer, ::Type{Complex{Float64}})
    d = Dense(ccall((:cholmod_allocate_dense, :libcholmod), Ptr{C_Dense{Complex{Float64}}},
        (Csize_t, Csize_t, Csize_t, Cint, Ptr{Void}),
        nrow, ncol, d, COMPLEX, common(Cint)))
    finalizer(d, free!)
    d
end

free_dense!{T}(p::Ptr{C_Dense{T}}) = ccall((:cholmod_free_dense, :libcholmod), Cint, (Ptr{Ptr{C_Dense{T}}}, Ptr{Void}), &p, common(Cint))

function zeros{T<:VTypes}(m::Integer, n::Integer, ::Type{T})
    d = Dense(ccall((:cholmod_zeros, :libcholmod), Ptr{C_Dense{T}},
        (Csize_t, Csize_t, Cint, Ptr{UInt8}),
         m, n, xtyp(T), common(Cint)))
    finalizer(d, free!)
    d
end
zeros(m::Integer, n::Integer) = zeros(m, n, Float64)

function ones{T<:VTypes}(m::Integer, n::Integer, ::Type{T})
    d = Dense(ccall((:cholmod_ones, :libcholmod), Ptr{C_Dense{T}},
        (Csize_t, Csize_t, Cint, Ptr{UInt8}),
         m, n, xtyp(T), common(Cint)))
    finalizer(d, free!)
    d
end
ones(m::Integer, n::Integer) = ones(m, n, Float64)

function eye{T<:VTypes}(m::Integer, n::Integer, ::Type{T})
    d = Dense(ccall((:cholmod_eye, :libcholmod), Ptr{C_Dense{T}},
        (Csize_t, Csize_t, Cint, Ptr{UInt8}),
         m, n, xtyp(T), common(Cint)))
    finalizer(d, free!)
    d
end
eye(m::Integer, n::Integer) = eye(m, n, Float64)
eye(n::Integer) = eye(n, n, Float64)

function copy_dense{Tv<:VTypes}(A::Dense{Tv})
    d = Dense(ccall((:cholmod_copy_dense, :libcholmod), Ptr{C_Dense{Tv}},
        (Ptr{C_Dense{Tv}}, Ptr{UInt8}),
         A.p, common(Cint)))
    finalizer(d, free!)
    d
end

### cholmod_matrixops.h ###
function norm_dense{Tv<:VTypes}(D::Dense{Tv}, p::Integer)
    s = unsafe_load(D.p)
    if p == 2
        if s.ncol > 1
            throw(ArgumentError("2 norm only supported when matrix has one column"))
        end
    elseif p != 0 && p != 1
        throw(ArgumentError("second argument must be either 0 (Inf norm), 1, or 2"))
    end
    ccall((:cholmod_norm_dense, :libcholmod), Cdouble,
        (Ptr{C_Dense{Tv}}, Cint, Ptr{UInt8}),
          D.p, p, common(Cint))
end

### cholmod_check.h ###
function check_dense{T<:VTypes}(A::Dense{T})
    bool(ccall((:cholmod_check_dense, :libcholmod), Cint,
        (Ptr{C_Dense{T}}, Ptr{UInt8}),
         A.p, common(Cint)))
end

# Non-Dense wrappers (which all depend on IType)
for Ti in IndexTypes
    @eval begin
        ### cholmod_core.h ###
        function allocate_sparse(nrow::Integer, ncol::Integer, nzmax::Integer, sorted::Bool, packed::Bool, stype::Integer, ::Type{Float64}, ::Type{$Ti})
            s = Sparse(ccall((@cholmod_name("allocate_sparse", $Ti), :libcholmod), Ptr{C_Sparse{Float64,$Ti}},
                    (Csize_t, Csize_t, Csize_t, Cint,
                        Cint, Cint, Cint, Ptr{Void}),
                    nrow, ncol, nzmax, sorted,
                        packed, stype, REAL, common($Ti)))
            finalizer(s, free!)
            s
        end
        function allocate_sparse(nrow::Integer, ncol::Integer, nzmax::Integer, sorted::Bool, packed::Bool, stype::Integer, ::Type{Complex{Float64}}, ::Type{$Ti})
            s = Sparse(ccall((@cholmod_name("allocate_sparse", $Ti), :libcholmod), Ptr{C_Sparse{Complex{Float64},$Ti}},
                    (Csize_t, Csize_t, Csize_t, Cint,
                        Cint, Cint, Cint, Ptr{Void}),
                    nrow, ncol, nzmax, sorted,
                        packed, stype, COMPLEX, common($Ti)))
            finalizer(s, free!)
            s
        end
        function free_sparse!{Tv<:VTypes}(ptr::Ptr{C_Sparse{Tv,$Ti}})
            @isok ccall((@cholmod_name("free_sparse", $Ti), :libcholmod), Cint,
                    (Ptr{Ptr{C_Sparse{Tv,$Ti}}}, Ptr{UInt8}),
                        &ptr, common($Ti))
        end

        function free_sparse!(ptr::Ptr{C_SparseVoid})
            @isok ccall((@cholmod_name("free_sparse", $Ti), :libcholmod), Cint,
                    (Ptr{Ptr{C_SparseVoid}}, Ptr{UInt8}),
                        &ptr, common($Ti))
        end

        function free_factor!{Tv<:VTypes}(ptr::Ptr{C_Factor{Tv,$Ti}})
            @isok ccall((@cholmod_name("free_factor", $Ti), :libcholmod), Cint,
                    (Ptr{Ptr{C_Factor{Tv,$Ti}}}, Ptr{Void}),
                        &ptr, common($Ti))
        end

        function aat{Tv<:VRealTypes}(A::Sparse{Tv,$Ti}, fset::Vector{$Ti}, mode::Integer)
            s = Sparse(ccall((@cholmod_name("aat", $Ti), :libcholmod), Ptr{C_Sparse{Tv,$Ti}},
                    (Ptr{C_Sparse{Tv,$Ti}}, Ptr{$Ti}, Csize_t, Cint, Ptr{UInt8}),
                        A.p, fset, length(fset), mode, common($Ti)))
            finalizer(s, free!)
            s
        end

        function sparse_to_dense{Tv<:VTypes}(A::Sparse{Tv,$Ti})
            d = Dense(ccall((@cholmod_name("sparse_to_dense", $Ti),:libcholmod), Ptr{C_Dense{Tv}},
                    (Ptr{C_Sparse{Tv,$Ti}}, Ptr{UInt8}),
                        A.p, common($Ti)))
            finalizer(d, free!)
            d
        end
        function dense_to_sparse{Tv<:VTypes}(D::Dense{Tv}, ::Type{$Ti})
            s = Sparse(ccall((@cholmod_name("dense_to_sparse", $Ti),:libcholmod), Ptr{C_Sparse{Tv,$Ti}},
                    (Ptr{C_Dense{Tv}}, Cint, Ptr{UInt8}),
                        D.p, true, common($Ti)))
            finalizer(s, free!)
            s
        end

        function factor_to_sparse!{Tv<:VTypes}(F::Factor{Tv,$Ti})
            ss = unsafe_load(F.p)
            ss.xtype > PATTERN || throw(CHOLMODException("only numeric factors are supported"))
            s = Sparse(ccall((@cholmod_name("factor_to_sparse", $Ti),:libcholmod), Ptr{C_Sparse{Tv,$Ti}},
                    (Ptr{C_Factor{Tv,$Ti}}, Ptr{UInt8}),
                        F.p, common($Ti)))
            finalizer(s, free!)
            s
        end

        function change_factor{Tv<:VTypes}(::Type{Float64}, to_ll::Bool, to_super::Bool, to_packed::Bool, to_monotonic::Bool, F::Factor{Tv,$Ti})
            @isok ccall((@cholmod_name("change_factor", $Ti),:libcholmod), Cint,
                    (Cint, Cint, Cint, Cint, Cint, Ptr{C_Factor{Tv,$Ti}}, Ptr{UInt8}),
                        REAL, to_ll, to_super, to_packed, to_monotonic, F.p, common($Ti))
            Factor{Float64,$Ti}(F.p)
        end

        function change_factor{Tv<:VTypes}(::Type{Complex{Float64}}, to_ll::Bool, to_super::Bool, to_packed::Bool, to_monotonic::Bool, F::Factor{Tv,$Ti})
            @isok ccall((@cholmod_name("change_factor", $Ti),:libcholmod), Cint,
                    (Cint, Cint, Cint, Cint, Cint, Ptr{C_Factor{Tv,$Ti}}, Ptr{UInt8}),
                        COMPLEX, to_ll, to_super, to_packed, to_monotonic, F.p, common($Ti))
            Factor{Complex{Float64},$Ti}(F.p)
        end

        function check_sparse{Tv<:VTypes}(A::Sparse{Tv,$Ti})
            bool(ccall((@cholmod_name("check_sparse", $Ti),:libcholmod), Cint,
                    (Ptr{C_Sparse{Tv,$Ti}}, Ptr{UInt8}),
                        A.p, common($Ti)))
        end

        function check_factor{Tv<:VTypes}(F::Factor{Tv,$Ti})
            bool(ccall((@cholmod_name("check_factor", $Ti),:libcholmod), Cint,
                    (Ptr{C_Factor{Tv,$Ti}}, Ptr{UInt8}),
                        F.p, common($Ti)))
        end

        function nnz{Tv<:VTypes}(A::Sparse{Tv,$Ti})
            ccall((@cholmod_name("nnz", $Ti),:libcholmod), Int,
                    (Ptr{C_Sparse{Tv,$Ti}}, Ptr{UInt8}),
                        A.p, common($Ti))
        end

        function speye{Tv<:VTypes}(m::Integer, n::Integer, ::Type{Tv})
            s = Sparse(ccall((@cholmod_name("speye", $Ti), :libcholmod), Ptr{C_Sparse{Tv,$Ti}},
                    (Csize_t, Csize_t, Cint, Ptr{UInt8}),
                        m, n, xtyp(Tv), common($Ti)))
            finalizer(s, free!)
            s
        end

        function spzeros{Tv<:VTypes}(m::Integer, n::Integer, nzmax::Integer, ::Type{Tv})
            s = Sparse(ccall((@cholmod_name("spzeros", $Ti), :libcholmod), Ptr{C_Sparse{Tv,$Ti}},
                    (Csize_t, Csize_t, Csize_t, Cint, Ptr{UInt8}),
                        m, n, nzmax, xtyp(Tv), common($Ti)))
            finalizer(s, free!)
            s
        end

        function transpose{Tv<:VTypes}(A::Sparse{Tv,$Ti}, values::Integer)
            s = Sparse(ccall((@cholmod_name("transpose", $Ti),:libcholmod), Ptr{C_Sparse{Tv,$Ti}},
                    (Ptr{C_Sparse{Tv,$Ti}}, Cint, Ptr{UInt8}),
                        A.p, values, common($Ti)))
            finalizer(s, free!)
            s
        end

        function copy_factor{Tv<:VTypes}(F::Factor{Tv,$Ti})
            f = Factor(ccall((@cholmod_name("copy_factor", $Ti),:libcholmod), Ptr{C_Factor{Tv,$Ti}},
                    (Ptr{C_Factor{Tv,$Ti}}, Ptr{UInt8}),
                        F.p, common($Ti)))
            finalizer(f, free!)
            f
        end
        function copy_sparse{Tv<:VTypes}(A::Sparse{Tv,$Ti})
            s = Sparse(ccall((@cholmod_name("copy_sparse", $Ti),:libcholmod), Ptr{C_Sparse{Tv,$Ti}},
                    (Ptr{C_Sparse{Tv,$Ti}}, Ptr{UInt8}),
                        A.p, common($Ti)))
            finalizer(s, free!)
            s
        end
        function copy{Tv<:VRealTypes}(A::Sparse{Tv,$Ti}, stype::Integer, mode::Integer)
            s = Sparse(ccall((@cholmod_name("copy", $Ti),:libcholmod), Ptr{C_Sparse{Tv,$Ti}},
                    (Ptr{C_Sparse{Tv,$Ti}}, Cint, Cint, Ptr{UInt8}),
                        A.p, stype, mode, common($Ti)))
            finalizer(s, free!)
            s
        end

        ### cholmod_check.h ###
        function print_sparse{Tv<:VTypes}(A::Sparse{Tv,$Ti}, name::ASCIIString)
            cm = common($Ti)
            set_print_level(cm, 3)
            @isok ccall((@cholmod_name("print_sparse", $Ti),:libcholmod), Cint,
                    (Ptr{C_Sparse{Tv,$Ti}}, Ptr{UInt8}, Ptr{UInt8}),
                         A.p, name, cm)
            nothing
        end
        function print_factor{Tv<:VTypes}(F::Factor{Tv,$Ti}, name::ASCIIString)
            cm = common($Ti)
            set_print_level(cm, 3)
            @isok ccall((@cholmod_name("print_factor", $Ti),:libcholmod), Cint,
                    (Ptr{C_Factor{Tv,$Ti}}, Ptr{UInt8}, Ptr{UInt8}),
                        F.p, name, cm)
            nothing
        end

        ### cholmod_matrixops.h ###
        function ssmult{Tv<:VRealTypes}(A::Sparse{Tv,$Ti}, B::Sparse{Tv,$Ti}, stype::Integer, values::Bool, sorted::Bool)
            lA = unsafe_load(A.p)
            lB = unsafe_load(B.p)
            if lA.ncol != lB.nrow
                throw(DimensionMismatch("inner matrix dimensions do not fit"))
            end
            s = Sparse(ccall((@cholmod_name("ssmult", $Ti),:libcholmod), Ptr{C_Sparse{Tv,$Ti}},
                    (Ptr{C_Sparse{Tv,$Ti}}, Ptr{C_Sparse{Tv,$Ti}}, Cint, Cint, Cint, Ptr{UInt8}),
                        A.p, B.p, stype, values, sorted, common($Ti)))
            finalizer(s, free!)
            s
        end

        function norm_sparse{Tv<:VTypes}(A::Sparse{Tv,$Ti}, norm::Integer)
            if norm != 0 && norm != 1
                throw(ArgumentError("norm argument must be either 0 or 1"))
            end
            ccall((@cholmod_name("norm_sparse", $Ti), :libcholmod), Cdouble,
                    (Ptr{C_Sparse{Tv,$Ti}}, Cint, Ptr{UInt8}),
                        A.p, norm, common($Ti))
        end

        function horzcat{Tv<:VRealTypes}(A::Sparse{Tv,$Ti}, B::Sparse{Tv,$Ti}, values::Bool)
            s = Sparse(ccall((@cholmod_name("horzcat", $Ti), :libcholmod), Ptr{C_Sparse{Tv,$Ti}},
                    (Ptr{C_Sparse{Tv,$Ti}}, Ptr{C_Sparse{Tv,$Ti}}, Cint, Ptr{UInt8}),
                        A.p, B.p, values, common($Ti)))
            finalizer(s, free!)
            s
        end

        function scale!{Tv<:VRealTypes}(S::Dense{Tv}, scale::Integer, A::Sparse{Tv,$Ti})
            sS = unsafe_load(S.p)
            sA = unsafe_load(A.p)
            sS.ncol == 1 || sS.nrow == 1 || throw(DimensionMismatch("first argument must be a vector"))
            if scale == SCALAR && sS.nrow != 1
                throw(DimensionMismatch("scaling argument must have length one"))
            elseif scale == ROW && sS.nrow*sS.ncol != sA.nrow
                throw(DimensionMismatch("scaling vector has length $(sS.nrow*sS.ncol), but matrix has $(sA.nrow) rows."))
            elseif scale == COL && sS.nrow*sS.ncol != sA.ncol
                throw(DimensionMismatch("scaling vector has length $(sS.nrow*sS.ncol), but matrix has $(sA.ncol) columns"))
            elseif scale == SYM
                if sA.nrow != sA.ncol
                    throw(DimensionMismatch("matrix must be square"))
                elseif sS.nrow*sS.ncol != sA.nrow
                    throw(DimensionMismatch("scaling vector has length $(sS.nrow*sS.ncol), but matrix has $(sA.ncol) columns and rows"))
                end
            end

            sA = unsafe_load(A.p)
            @isok ccall((@cholmod_name("scale",$Ti),:libcholmod), Cint,
                    (Ptr{C_Dense{Tv}}, Cint, Ptr{C_Sparse{Tv,$Ti}}, Ptr{UInt8}),
                        S.p, scale, A.p, common($Ti))
            A
        end

        function sdmult!{Tv<:VTypes}(A::Sparse{Tv,$Ti}, transpose::Bool, α::Tv, β::Tv, X::Dense{Tv}, Y::Dense{Tv})
            m, n = size(A)
            nc = transpose ? m : n
            nr = transpose ? n : m
            if nc != size(X, 1)
                throw(DimensionMismatch("incompatible dimensions, $nc and $(size(X,1))"))
            end
            @isok ccall((@cholmod_name("sdmult", $Ti),:libcholmod), Cint,
                    (Ptr{C_Sparse{Tv,$Ti}}, Cint, Ptr{Cdouble}, Ptr{Cdouble},
                        Ptr{C_Dense{Tv}}, Ptr{C_Dense{Tv}}, Ptr{UInt8}),
                            A.p, transpose, &α, &β,
                                X.p, Y.p, common($Ti))
            Y
        end

        function vertcat{Tv<:VRealTypes}(A::Sparse{Tv,$Ti}, B::Sparse{Tv,$Ti}, values::Bool)
            s = Sparse(ccall((@cholmod_name("vertcat", $Ti), :libcholmod), Ptr{C_Sparse{Tv,$Ti}},
                    (Ptr{C_Sparse{Tv,$Ti}}, Ptr{C_Sparse{Tv,$Ti}}, Cint, Ptr{UInt8}),
                        A.p, B.p, values, common($Ti)))
            finalizer(s, free!)
            s
        end

        function symmetry{Tv<:VTypes}(A::Sparse{Tv,$Ti}, option::Integer)
            xmatched = Array($Ti, 1)
            pmatched = Array($Ti, 1)
            nzoffdiag = Array($Ti, 1)
            nzdiag = Array($Ti, 1)
            rv = ccall((@cholmod_name("symmetry", $Ti), :libcholmod), Cint,
                    (Ptr{C_Sparse{Tv,$Ti}}, Cint, Ptr{$Ti}, Ptr{$Ti},
                        Ptr{$Ti}, Ptr{$Ti}, Ptr{UInt8}),
                            A.p, option, xmatched, pmatched,
                                nzoffdiag, nzdiag, common($Ti))
            rv, xmatched[1], pmatched[1], nzoffdiag[1], nzdiag[1]
        end

        # cholmod_cholesky.h
        # For analyze, factorize and factorize_p, the Common argument must be
        # supplied in order to control if the factorization is LLt or LDLt
        function analyze{Tv<:VTypes}(A::Sparse{Tv,$Ti}, cmmn::Vector{UInt8})
            f = Factor(ccall((@cholmod_name("analyze", $Ti),:libcholmod), Ptr{C_Factor{Tv,$Ti}},
                    (Ptr{C_Sparse{Tv,$Ti}}, Ptr{UInt8}),
                        A.p, cmmn))
            finalizer(f, free!)
            f
        end
        function factorize!{Tv<:VTypes}(A::Sparse{Tv,$Ti}, F::Factor{Tv,$Ti}, cmmn::Vector{UInt8})
            @isok ccall((@cholmod_name("factorize", $Ti),:libcholmod), Cint,
                    (Ptr{C_Sparse{Tv,$Ti}}, Ptr{C_Factor{Tv,$Ti}}, Ptr{UInt8}),
                        A.p, F.p, cmmn)
            F
        end
        function factorize_p!{Tv<:VTypes}(A::Sparse{Tv,$Ti}, β::Tv, fset::Vector{$Ti}, F::Factor{Tv,$Ti}, cmmn::Vector{UInt8})
            @isok ccall((@cholmod_name("factorize_p", $Ti),:libcholmod), Cint,
                    (Ptr{C_Sparse{Tv,$Ti}}, Ptr{Cdouble}, Ptr{$Ti}, Csize_t,
                        Ptr{C_Factor{Tv,$Ti}}, Ptr{UInt8}),
                            A.p, &β, fset, length(fset),
                                F.p, cmmn)
            F
        end

        function solve{Tv<:VTypes}(sys::Integer, F::Factor{Tv,$Ti}, B::Dense{Tv})
            if size(F,1) != size(B,1)
                throw(DimensionMismatch("LHS and RHS should have the same number of rows. LHS has $(size(F,1)) rows, but RHS has $(size(B,1)) rows."))
            end
            d = Dense(ccall((@cholmod_name("solve", $Ti),:libcholmod), Ptr{C_Dense{Tv}},
                    (Cint, Ptr{C_Factor{Tv,$Ti}}, Ptr{C_Dense{Tv}}, Ptr{UInt8}),
                        sys, F.p, B.p, common($Ti)))
            finalizer(d, free!)
            d
        end

        function spsolve{Tv<:VTypes}(sys::Integer, F::Factor{Tv,$Ti}, B::Sparse{Tv,$Ti})
            if size(F,1) != size(B,1)
                throw(DimensionMismatch("LHS and RHS should have the same number of rows. LHS has $(size(F,1)) rows, but RHS has $(size(B,1)) rows."))
            end
            s = Sparse(ccall((@cholmod_name("spsolve", $Ti),:libcholmod), Ptr{C_Sparse{Tv,$Ti}},
                    (Cint, Ptr{C_Factor{Tv,$Ti}}, Ptr{C_Sparse{Tv,$Ti}}, Ptr{UInt8}),
                        sys, F.p, B.p, common($Ti)))
            finalizer(s, free!)
            s
        end

        # Autodetects the types
        function read_sparse(file::CFILE, ::Type{$Ti})
            s = Sparse(ccall((@cholmod_name("read_sparse", $Ti), :libcholmod), Ptr{C_SparseVoid},
                (Ptr{Void}, Ptr{UInt8}),
                    file.ptr, common($Ti)))
            finalizer(s, free!)
            s
        end
    end
end

#########################
# High level interfaces #
#########################

# Convertion/construction
function Dense{T<:VTypes}(A::VecOrMat{T})
    d = allocate_dense(size(A, 1), size(A, 2), stride(A, 2), T)
    s = unsafe_load(d.p)
    unsafe_copy!(s.x, pointer(A), length(A))
    d
end
Dense(A::Sparse) = sparse_to_dense(A)

# This constructior assumes zero based colptr and rowval
function Sparse{Tv<:VTypes,Ti<:ITypes}(m::Integer, n::Integer, colptr::Vector{Ti}, rowval::Vector{Ti}, nzval::Vector{Tv}, stype)

    # check if columns are sorted
    iss = true
    for i = 2:length(colptr)
        if !issorted(sub(rowval, colptr[i - 1] + 1:colptr[i]))
            iss = false
            break
        end
    end

    o = allocate_sparse(m, n, length(nzval), iss, true, stype, Tv, Ti)
    s = unsafe_load(o.p)

    unsafe_copy!(s.p, pointer(colptr), length(colptr))
    unsafe_copy!(s.i, pointer(rowval), length(rowval))
    unsafe_copy!(s.x, pointer(nzval), length(nzval))

    @isok check_sparse(o)

    return o

end
function Sparse{Tv<:VTypes,Ti<:ITypes}(m::Integer, n::Integer, colptr::Vector{Ti}, rowval::Vector{Ti}, nzval::Vector{Tv})
    o = Sparse(m, n, colptr, rowval, nzval, 0)

    # check if array is symmetric and change stype if it is
    if ishermitian(o)
        change_stype!(o, -1)
    end
    o
end

function Sparse{Tv<:VTypes,Ti<:ITypes}(A::SparseMatrixCSC{Tv,Ti}, stype::Integer)
    o = allocate_sparse(A.m, A.n, length(A.nzval), true, true, stype, Tv, Ti)
    s = unsafe_load(o.p)
    for i = 1:length(A.colptr)
        unsafe_store!(s.p, A.colptr[i] - 1, i)
    end
    for i = 1:length(A.rowval)
        unsafe_store!(s.i, A.rowval[i] - 1, i)
    end
    unsafe_copy!(s.x, pointer(A.nzval), length(A.nzval))

    @isok check_sparse(o)

    return o
end
function Sparse{Tv<:VTypes,Ti<:ITypes}(A::SparseMatrixCSC{Tv,Ti})
    o = Sparse(A, 0)
    # check if array is symmetric and change stype if it is
    if ishermitian(o)
        change_stype!(o, -1)
    end
    o
end

Sparse{Ti<:ITypes}(A::Symmetric{Float64,SparseMatrixCSC{Float64,Ti}}) = Sparse(A.data, A.uplo == 'L' ? -1 : 1)
Sparse{Tv<:VTypes,Ti<:ITypes}(A::Hermitian{Tv,SparseMatrixCSC{Tv,Ti}}) = Sparse(A.data, A.uplo == 'L' ? -1 : 1)

# Useful when reading in files, but not type stable
function Sparse(p::Ptr{C_SparseVoid})
    s = unsafe_load(p)

    # Check integer type
    if s.itype == INT
        Ti = Cint
    elseif s.itype == INTLONG
        free_sparse!(p)
        throw(CHOLMODException("the value of itype was $s.itype. This combination of integer types shouldn't happen. Please submit a bug report."))
    elseif s.itype == LONG
        Ti = SuiteSparse_long
    else
        free_sparse!(p)
        throw(CHOLMODException("illegal value of itype: $s.itype"))
    end

    # Check for double or single precision
    if s.dtype == DOUBLE
        Tv = Float64
    elseif s.dtype == SINGLE
        # Tv = Float32 # this should be supported at some point
        free_sparse!(p)
        throw(CHOLMODException("single precision not supported yet"))
    else
        free_sparse!(p)
        throw(CHOLMODException("illegal value of dtype: $s.dtype"))
    end

    # Check for real or complex
    if s.xtype == COMPLEX
        Tv = Complex{Tv}
    elseif s.xtype != REAL
        free_sparse!(p)
        throw(CHOLMODException("illegal value of xtype: $s.xtype"))
    end

    return Sparse(convert(Ptr{C_Sparse{Tv,Ti}}, p))

end

# default is Cint which is probably sufficient when converted from dense matrix
Sparse(A::Dense) = dense_to_sparse(A, Cint)
Sparse(L::Factor) = factor_to_sparse!(copy(L))
function Sparse(filename::ASCIIString)
    f = open(filename)
    A = read_sparse(CFILE(f), Int)
    close(f)
    A
end

## convertion back to base Julia types
function convert{T<:VTypes}(::Type{Matrix}, D::Dense{T})
    s = unsafe_load(D.p)
    a = Array(T, s.nrow, s.ncol)
    if s.d == s.nrow
        unsafe_copy!(pointer(a), s.x, s.d*s.ncol)
    else
        for j = 1:s.ncol
            for i = 1:s.nrow
                a[i,j] = unsafe_load(s.x, i + (j - 1)*s.d)
            end
        end
    end
    a
end

function convert{Tv,Ti}(::Type{SparseMatrixCSC{Tv,Ti}}, A::Sparse{Tv,Ti})
    s = unsafe_load(A.p)
    if s.stype != 0
        throw(ArgumentError("matrix has stype != 0. Convert to matrix with stype == 0 before converting to SparseMatrixCSC"))
    end
    return SparseMatrixCSC(s.nrow, s.ncol, increment(pointer_to_array(s.p, (s.ncol + 1,), false)), increment(pointer_to_array(s.i, (s.nzmax,), false)), copy(pointer_to_array(s.x, (s.nzmax,), false)))
end
function convert{Ti<:ITypes}(::Type{Symmetric{Float64,SparseMatrixCSC{Float64,Ti}}}, A::Sparse{Float64,Ti})
    s = unsafe_load(A.p)
    if !issym(A)
        throw(ArgumentError("matrix is not symmetric"))
    end
    return Symmetric(SparseMatrixCSC(s.nrow, s.ncol, increment(pointer_to_array(s.p, (s.ncol + 1,), false)), increment(pointer_to_array(s.i, (s.nzmax,), false)), copy(pointer_to_array(s.x, (s.nzmax,), false))), s.stype > 0 ? :U : :L)
end
function convert{Tv<:VTypes,Ti<:ITypes}(::Type{Hermitian{Tv,SparseMatrixCSC{Tv,Ti}}}, A::Sparse{Tv,Ti})
    s = unsafe_load(A.p)
    if !ishermitian(A)
        throw(ArgumentError("matrix is not Hermitian"))
    end
    return Hermitian(SparseMatrixCSC(s.nrow, s.ncol, increment(pointer_to_array(s.p, (s.ncol + 1,), false)), increment(pointer_to_array(s.i, (s.nzmax,), false)), copy(pointer_to_array(s.x, (s.nzmax,), false))), s.stype > 0 ? :U : :L)
end
function sparse{Ti}(A::Sparse{Float64,Ti}) # Notice! Cannot be type stable because of stype
    s = unsafe_load(A.p)
    if s.stype == 0
        return convert(SparseMatrixCSC{Float64,Ti}, A)
    end
    return convert(Symmetric{Float64,SparseMatrixCSC{Float64,Ti}}, A)
end
function sparse{Ti}(A::Sparse{Complex{Float64},Ti}) # Notice! Cannot be type stable because of stype
    s = unsafe_load(A.p)
    if s.stype == 0
        return convert(SparseMatrixCSC{Complex{Float64},Ti}, A)
    end
    return convert(Hermitian{Complex{Float64},SparseMatrixCSC{Complex{Float64},Ti}}, A)
end
sparse(L::Factor)   = sparse(Sparse(L))
sparse(D::Dense)    = sparse(Sparse(D))

# Calculate the offset into the stype field of the cholmod_sparse_struct and
# change the value
function change_stype!(A::Sparse, i::Integer)
    offset = fieldoffsets(C_Sparse)[names(C_Sparse) .== :stype][1]
    unsafe_store!(convert(Ptr{Cint}, A.p), i, div(offset, 4) + 1)
    A
end

free!(A::Dense) = free_dense!(A.p)
free!(A::Sparse) = free_sparse!(A.p)
free!(F::Factor) = free_factor!(F.p)

eltype{T<:VTypes}(A::Dense{T}) = T
eltype{T<:VTypes}(A::Factor{T}) = T
eltype{T<:VTypes}(A::Sparse{T}) = T

function show(io::IO, F::Factor)
    s = unsafe_load(F.p)
    println(io, typeof(F))
    @printf(io, "type: %12s\n", bool(s.is_ll) ? "LLt" : "LDLt")
    @printf(io, "method: %10s\n", bool(s.is_super) ? "supernodal" : "simplicial")
    @printf(io, "maxnnz: %10d\n", int(s.nzmax))
    @printf(io, "nnz: %13d\n", nnz(Sparse(F)))
end

isvalid(A::Dense) = check_dense(A)
isvalid(A::Sparse) = check_sparse(A)
isvalid(A::Factor) = check_factor(A)

copy(A::Dense) = copy_dense(A)
copy(A::Sparse) = copy_sparse(A)
copy(A::Factor) = copy_factor(A)

function size(A::Union(Dense,Sparse))
    s = unsafe_load(A.p)
    return (int(s.nrow), int(s.ncol))
end
function size(F::Factor, i::Integer)
    if i < 1
        throw(ArgumentError("dimension must be positive"))
    end
    s = unsafe_load(F.p)
    if i <= 2
        return int(s.n)
    end
    return 1
end

function getindex(A::Dense, i::Integer)
    s = unsafe_load(A.p)
    0 < i <= s.nrow*s.ncol || throw(BoundsError())
    unsafe_load(s.x, i)
end
function getindex(A::Dense, i::Integer, j::Integer)
    s = unsafe_load(A.p)
    0 < i <= s.nrow || throw(BoundsError())
    0 < j <= s.ncol || throw(BoundsError())
    unsafe_load(s.x, i + (j - 1)*s.d)
end

getindex(A::Sparse, i::Integer) = getindex(A, ind2sub(size(A),i)...)
function getindex{T}(A::Sparse{T}, i0::Integer, i1::Integer)
    s = unsafe_load(A.p)
    !(1 <= i0 <= s.nrow && 1 <= i1 <= s.ncol) && throw(BoundsError())
    s.stype < 0 && i0 < i1 && return conj(A[i1,i0])
    s.stype > 0 && i0 > i1 && return conj(A[i1,i0])

    r1 = int(unsafe_load(s.p, i1) + 1)
    r2 = int(unsafe_load(s.p, i1 + 1))
    (r1 > r2) && return zero(T)
    r1 = int(searchsortedfirst(pointer_to_array(s.i, (s.nzmax,), false), i0 - 1, r1, r2, Base.Order.Forward))
    ((r1 > r2) || (unsafe_load(s.i, r1) + 1 != i0)) ? zero(T) : unsafe_load(s.x, r1)
end

## Multiplication
(*)(A::Sparse, B::Sparse) = ssmult(A, B, 0, true, true)
(*)(A::Sparse, B::Dense) = sdmult!(A, false, 1., 0., B, zeros(size(A, 1), size(B, 2)))
(*)(A::Sparse, B::VecOrMat) = (*)(A, Dense(B))

function A_mul_Bc{Tv<:VRealTypes,Ti<:ITypes}(A::Sparse{Tv,Ti}, B::Sparse{Tv,Ti})
    cm = common(Ti)

    if !is(A,B)
        aa1 = transpose(B, 2)
        ## result of ssmult will have stype==0, contain numerical values and be sorted
        return ssmult(A, aa1, 0, true, true)
    end

    ## The A*A' case is handled by cholmod_aat. This routine requires
    ## A->stype == 0 (storage of upper and lower parts). If neccesary
    ## the matrix A is first converted to stype == 0
    s = unsafe_load(A.p)
    if s.stype != 0
        aa1 = copy(A, 0, 1)
        return aat(aa1, Ti[0:s.ncol-1;], 1)
    else
        return aat(A, Ti[0:s.ncol-1;], 1)
    end
end

function Ac_mul_B(A::Sparse, B::Sparse)
    aa1 = transpose(A, 2)
    if is(A,B)
        return A_mul_Bc(aa1, aa1)
    end
    ## result of ssmult will have stype==0, contain numerical values and be sorted
    return ssmult(aa1, B, 0, true, true)
end

Ac_mul_B(A::Sparse, B::Dense) = sdmult!(A, true, 1., 0., B, zeros(size(A, 2), size(B, 2)))
Ac_mul_B(A::Sparse, B::VecOrMat) =  Ac_mul_B(A, Dense(B))


## Factorization methods
function cholfact(A::Sparse)
    sA = unsafe_load(A.p)
    sA.stype == 0 && throw(ArgumentError("sparse matrix is not symmetric/Hermitian"))
    cm = common(indtype(A))
    ## may need to change final_asis as well as final_ll
    cm[common_final_ll] = reinterpret(UInt8, [one(Cint)]) # Hack! makes it a llt
    F = analyze(A, cm)
    factorize!(A, F, cm)
    s = unsafe_load(F.p)
    s.minor < size(A, 1) && throw(Base.LinAlg.PosDefException(s.minor))
    return F
end

function cholfact{Tv<:VTypes,Ti<:ITypes}(A::Sparse{Tv,Ti}, β::Tv)
    cm = common(Ti)
    ## may need to change final_asis as well as final_ll
    cm[common_final_ll] = reinterpret(UInt8, [one(Cint)]) # Hack! makes it a llt
    F = analyze(A, cm)
    factorize_p!(A, β, Ti[], F, cm)
    s = unsafe_load(F.p)
    s.minor < size(A, 1) && throw(Base.LinAlg.PosDefException(s.minor))
    return F
end

function ldltfact(A::Sparse)
    cm = common(indtype(A))
    ## may need to change final_asis as well as final_ll
    cm[common_final_ll] = reinterpret(UInt8, [zero(Cint)]) # Hack! makes it a ldlt
    F = analyze(A, cm)
    factorize!(A, F, cm)
    return F
end

function ldltfact{Tv<:VTypes,Ti<:ITypes}(A::Sparse{Tv,Ti}, β::Tv)
    cm = common(Ti)
    ## may need to change final_asis as well as final_ll
    cm[common_final_ll] = reinterpret(UInt8, [zero(Cint)]) # Hack! makes it a ldlt
    F = analyze(A, cm)
    factorize_p!(A, β, Ti[], F, cm)
    s = unsafe_load(F.p)
    s.minor < size(A, 1) && throw(Base.LinAlg.PosDefException(s.minor))
    return F
end

cholfact{T<:VTypes}(A::Sparse{T}, β::Number) = cholfact(A, convert(T, β))
cholfact(A::SparseMatrixCSC) = cholfact(Sparse(A))
cholfact(A::SparseMatrixCSC, β::Number) = cholfact(Sparse(A), β)
cholfact{Ti}(A::Symmetric{Float64,SparseMatrixCSC{Float64,Ti}}) = cholfact(Sparse(A))
cholfact{Ti}(A::Symmetric{Float64,SparseMatrixCSC{Float64,Ti}}, β::Number) = cholfact(Sparse(A), β)
cholfact{Ti}(A::Hermitian{Complex{Float64},SparseMatrixCSC{Complex{Float64},Ti}}) = cholfact(Sparse(A))
cholfact{Ti}(A::Hermitian{Complex{Float64},SparseMatrixCSC{Complex{Float64},Ti}}, β::Number) = cholfact(Sparse(A), β)

ldltfact{T<:VTypes}(A::Sparse{T}, β::Number) = ldltfact(A, convert(T, β))
ldltfact(A::SparseMatrixCSC) = ldltfact(Sparse(A))
ldltfact(A::SparseMatrixCSC, β::Number) = ldltfact(Sparse(A), β)
ldltfact{Ti}(A::Symmetric{Float64,SparseMatrixCSC{Float64,Ti}}) = ldltfact(Sparse(A))
ldltfact{Ti}(A::Symmetric{Float64,SparseMatrixCSC{Float64,Ti}}, β::Number) = ldltfact(Sparse(A), β)
ldltfact{Ti}(A::Hermitian{Complex{Float64},SparseMatrixCSC{Complex{Float64},Ti}}) = ldltfact(Sparse(A))
ldltfact{Ti}(A::Hermitian{Complex{Float64},SparseMatrixCSC{Complex{Float64},Ti}}, β::Number) = ldltfact(Sparse(A), β)

function update!{Tv<:VTypes,Ti<:ITypes}(F::Factor{Tv,Ti}, A::Sparse{Tv,Ti})
    cm = common(Ti)
    s = unsafe_load(F.p)
    if bool(s.is_ll)
        cm[common_final_ll] = reinterpret(UInt8, [one(Cint)]) # Hack! makes it a llt
    end
    factorize!(A, F, cm)
end
function update!{Tv<:VTypes,Ti<:ITypes}(F::Factor{Tv,Ti}, A::Sparse{Tv,Ti}, β::Tv)
    cm = common(Ti)
    s = unsafe_load(F.p)
    if bool(s.is_ll)
        cm[common_final_ll] = reinterpret(UInt8, [one(Cint)]) # Hack! makes it a llt
    end
    factorize_p!(A, β, Ti[0:size(F, 1) - 1;], F, cm)
end
update!{T<:VTypes}(F::Factor{T}, A::SparseMatrixCSC{T}) = update!(F, Sparse(A))
update!{T<:VTypes}(F::Factor{T}, A::SparseMatrixCSC{T}, β::Number) = update!(F, Sparse(A), convert(T, β))

## Solvers

(\)(L::Factor, B::Dense) = solve(CHOLMOD_A, L, B)
(\)(L::Factor, b::Vector) = reshape(convert(Matrix, solve(CHOLMOD_A, L, Dense(b))), length(b))
(\)(L::Factor, B::Matrix) = convert(Matrix, solve(CHOLMOD_A, L, Dense(B)))
(\)(L::Factor, B::Sparse) = spsolve(CHOLMOD_A, L, B)
# When right hand side is sparse, we have to ensure that the rhs is not marked as symmetric.
(\)(L::Factor, B::SparseMatrixCSC) = sparse(spsolve(CHOLMOD_A, L, Sparse(B, 0)))

Ac_ldiv_B(L::Factor, B::Dense) = solve(CHOLMOD_A, L, B)
Ac_ldiv_B(L::Factor, B::VecOrMat) = convert(Matrix, solve(CHOLMOD_A, L, Dense(B)))
Ac_ldiv_B(L::Factor, B::Sparse) = spsolve(CHOLMOD_A, L, B)
Ac_ldiv_B(L::Factor, B::SparseMatrixCSC) = Ac_ldiv_B(L, Sparse(B))

## Other convenience methods
function diag{Tv}(F::Factor{Tv})
    f = unsafe_load(F.p)
    res = Base.zeros(Tv, int(f.n))
    xv  = f.x
    if bool(f.is_super)
        px = f.px
        pos = 1
        for i in 1:f.nsuper
            base = unsafe_load(px, i) + 1
            res[pos] = unsafe_load(xv, base)
            pos += 1
            for j in 1:unsafe_load(f.super, i + 1) - unsafe_load(f.super, i) - 1
                res[pos] = unsafe_load(xv, base + j*(unsafe_load(f.pi, i + 1) - unsafe_load(f.pi, i) + 1))
                pos += 1
            end
        end
    else
        c0 = f.p
        r0 = f.i
        xv = f.x
        for j in 1:f.n
            jj = unsafe_load(c0, j) + 1
            assert(unsafe_load(r0, jj) == j - 1)
            res[j] = unsafe_load(xv, jj)
        end
    end
    res
end

function logdet{Tv<:VTypes,Ti<:ITypes}(F::Factor{Tv,Ti})
    f = unsafe_load(F.p)
    res = zero(Tv)
    for d in diag(F) res += log(abs(d)) end
    bool(f.is_ll) ? 2res : res
end

det(L::Factor) = exp(logdet(L))

function isposdef{Tv<:VTypes,Ti}(A::SparseMatrixCSC{Tv,Ti})
    if !ishermitian(A)
        return false
    end
    f = cholfact(A)
    s = unsafe_load(f.p)
    return s.minor == size(A,1)
end

function issym(A::Sparse)
    s = unsafe_load(A.p)
    if s.stype != 0
        return isreal(A)
    end
    i = symmetry(A, ifelse(version >= v"3.0.5", 0, 1))[1] # 0 is faster, but had a bug before 3.0.5
    return i == MM_SYMMETRIC || i == MM_SYMMETRIC_POSDIAG
end

function ishermitian(A::Sparse)
    s = unsafe_load(A.p)
    if isreal(A)
        if s.stype != 0
            return true
        else
            i = symmetry(A, ifelse(version >= v"3.0.5", 0, 1))[1] # 0 is faster, but had a bug before 3.0.5
            return i == MM_SYMMETRIC || i == MM_SYMMETRIC_POSDIAG
        end
    else
        if s.stype != 0
            return true
        else
            i = symmetry(A, ifelse(version >= v"3.0.5", 0, 1))[1] # 0 is faster, but had a bug before 3.0.5
            return i == MM_HERMITIAN || i == MM_HERMITIAN_POSDIAG
        end
    end
end

end #module
