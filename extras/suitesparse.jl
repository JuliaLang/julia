module SuiteSparse

import Base.SparseMatrixCSC, Base.size, Base.nnz, Base.eltype, Base.show
import Base.triu, Base.norm, Base.solve, Base.(\), Base.ctranspose, Base.transpose
import Base.convert

import Base.BlasInt
import Base.blas_int

export                                  # types
    CholmodPtr,
    CholmodCommon,
    CholmodSparse,
    CholmodFactor,
    CholmodDense,
    CholmodSparseOut,
    UmfpackPtr,
    UmfpackLU,
    UmfpackLU!,
    UmfpackLUTrans,
                                        # methods
    chm_aat, # drop prefix?
    eltype,  #? maybe not
    indtype, #? maybe not
    nnz,
    show,
    size,
    solve,
    \,
    At_ldiv_B,
    Ac_ldiv_B

include("suitesparse_h.jl")

const libsuitesparse_wrapper = "libsuitesparse_wrapper"
const libcholmod = "libcholmod"
const libumfpack = "libumfpack"
const libspqr = "libspqr"

const _chm_aat       = (:cholmod_aat, libcholmod)
const _chm_amd       = (:cholmod_amd, libcholmod)
const _chm_analyze   = (:cholmod_analyze, libcholmod)
const _chm_colamd    = (:cholmod_colamd, libcholmod)
const _chm_copy      = (:cholmod_copy, libcholmod)
const _chm_factorize = (:cholmod_factorize, libcholmod)
const _chm_free_dn   = (:cholmod_free_dense, libcholmod)
const _chm_free_fa   = (:cholmod_free_factor, libcholmod)
const _chm_free_sp   = (:cholmod_free_sparse, libcholmod)
const _chm_print_dn  = (:cholmod_print_dense, libcholmod)
const _chm_print_fa  = (:cholmod_print_factor, libcholmod)
const _chm_print_sp  = (:cholmod_print_sparse, libcholmod)
const _chm_solve     = (:cholmod_solve, libcholmod)
const _chm_sort      = (:cholmod_sort, libcholmod)
const _chm_submatrix = (:cholmod_submatrix, libcholmod)

const _spqr_C_QR                = (:SuiteSparseQR_C_QR, libspqr)
const _spqr_C_backslash         = (:SuiteSparseQR_C_backslash, libspqr)
const _spqr_C_backslash_default = (:SuiteSparseQR_C_backslash_default, libspqr)
const _spqr_C_backslash_sparse  = (:SuiteSparseQR_C_backslash_sparse, libspqr)
const _spqr_C_factorize         = (:SuiteSparseQR_C_factorize, libspqr)
const _spqr_C_symbolic          = (:SuiteSparseQR_C_symbolic, libspqr)
const _spqr_C_numeric           = (:SuiteSparseQR_C_numeric, libspqr)
const _spqr_C_free              = (:SuiteSparseQR_C_free, libspqr)
const _spqr_C_solve             = (:SuiteSparseQR_C_solve, libspqr)
const _spqr_C_qmult             = (:SuiteSparseQR_C_qmult, libspqr)

type MatrixIllConditionedException <: Exception end

function convert_to_0_based_indexing!(S::SparseMatrixCSC)
    for i=1:(S.colptr[end]-1); S.rowval[i] -= 1; end
    for i=1:length(S.colptr); S.colptr[i] -= 1; end
    return S
end

function convert_to_1_based_indexing!(S::SparseMatrixCSC)
    for i=1:length(S.colptr); S.colptr[i] += 1; end
    for i=1:(S.colptr[end]-1); S.rowval[i] += 1; end
    return S
end

convert_to_0_based_indexing(S) = convert_to_0_based_indexing!(copy(S))
convert_to_1_based_indexing(S) = convert_to_1_based_indexing!(copy(S))

## CHOLMOD

typealias CHMVTypes Union(Complex64, Complex128, Float32, Float64)
typealias CHMITypes Union(Int32, Int64)

function chm_itype{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti})
    if !(Ti<:CHMITypes) error("chm_itype: indtype(S) must be in CHMITypes") end
    Ti == Int32 ? CHOLMOD_INT : CHOLMOD_LONG
end

function chm_xtype{T}(S::SparseMatrixCSC{T})
    if !(T<:CHMVTypes) error("chm_xtype: eltype(S) must be in CHMVTypes") end
    T <: Complex ? CHOLMOD_COMPLEX : CHOLMOD_REAL
end

function chm_dtype{T}(S::SparseMatrixCSC{T})
    if !(T<:CHMVTypes) error("chm_dtype: eltype(S) must be in CHMVTypes") end
    T <: Union(Float32, Complex64) ? CHOLMOD_SINGLE : CHOLMOD_DOUBLE
end

# Wrapper for memory allocated by CHOLMOD. Carry along the value and index types.
## FIXME: CholmodPtr and UmfpackPtr should be amalgamated
type CholmodPtr{Tv<:CHMVTypes,Ti<:CHMITypes}
    val::Vector{Ptr{Void}} 
end

eltype{Tv,Ti}(P::CholmodPtr{Tv,Ti}) = Tv
indtype{Tv,Ti}(P::CholmodPtr{Tv,Ti}) = Ti

function cholmod_common_finalizer(x::Vector{Ptr{Void}})
    st = ccall((:cholmod_finish, libcholmod), BlasInt, (Ptr{Void},), x[1])
    if st != CHOLMOD_TRUE error("Error calling cholmod_finish") end
    c_free(x[1])
end

type CholmodCommon
    pt::Vector{Ptr{Void}}
    function CholmodCommon()
        pt = Array(Ptr{Void}, 1)
        ccall((:jl_cholmod_common, libsuitesparse_wrapper), Void,
              (Ptr{Void},), pt)
        st = ccall((:cholmod_start, libcholmod), BlasInt, (Ptr{Void}, ), pt[1])
        if st != CHOLMOD_TRUE error("Error calling cholmod_start") end
        finalizer(pt, cholmod_common_finalizer)
        new(pt)
    end
end

function show(io::IO, cm::CholmodCommon)
    st = ccall((:cholmod_print_common, libcholmod), BlasInt,
               (Ptr{Uint8},Ptr{Void}), "", cm.pt[1])
    if st != CHOLMOD_TRUE error("Error calling cholmod_print_common") end
end

type CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}
    pt::CholmodPtr{Tv,Ti}
    ## cp contains a copy of the original matrix but with 0-based indices
    cp::SparseMatrixCSC{Tv,Ti}
    stype::Int
    cm::CholmodCommon
    function CholmodSparse(S::SparseMatrixCSC{Tv,Ti}, stype::BlasInt, cm::CholmodCommon)
        pt = CholmodPtr{Tv,Ti}(Array(Ptr{Void}, 1))
        cp = convert_to_0_based_indexing(S)
        
        ccall((:jl_cholmod_sparse, libsuitesparse_wrapper), Void,
              (Ptr{Void}, Uint, Uint, Uint, Ptr{Void}, Ptr{Void}, Ptr{Void},
               Ptr{Void}, Ptr{Void}, BlasInt, BlasInt, BlasInt, BlasInt, BlasInt, Int),
              pt.val, S.m, S.n, nnz(S), cp.colptr, cp.rowval, C_NULL,
              cp.nzval, C_NULL, stype, chm_itype(S), chm_xtype(S), chm_dtype(S),
              CHOLMOD_TRUE, CHOLMOD_TRUE)
        finalizer(pt, x->c_free(x.val[1]))
        new(pt, cp, blas_int(stype), cm)
    end
end

CholmodSparse{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}, stype::Int) = CholmodSparse{Tv,Ti}(S, stype, CholmodCommon())

function CholmodSparse{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}, cm::CholmodCommon)
    stype = S.m == S.n && ishermitian(S)
    CholmodSparse{Tv,Ti}(stype ? triu(S) : S, blas_int(stype), cm)
end

CholmodSparse(S::SparseMatrixCSC) = CholmodSparse(S, CholmodCommon())

function show(io::IO, cs::CholmodSparse)
    ccall(_chm_print_sp,
          BlasInt, (Ptr{Void}, Ptr{Uint8},Ptr{Void}), cs.pt.val[1], "", cs.cm.pt[1])
end

size(cs::CholmodSparse) = size(cs.cp)
nnz(cs::CholmodSparse) = cs.cp.colptr[end]
eltype{T}(cs::CholmodSparse{T}) = T
indtype{Tv,Ti}(cs::CholmodSparse{Tv,Ti}) = Ti

SparseMatrixCSC(cs::CholmodSparse) = convert_to_1_based_indexing(cs.cp)

## For testing only.  The infinity and 1 norms of a sparse matrix are simply
## the same norm applied to its nzval field.
function norm(cs::CholmodSparse, p::Number)
    ccall((:cholmod_norm_sparse, libcholmod), Float64,
          (Ptr{Void}, BlasInt, Ptr{Void}), cs.pt.val[1], p == Inf ? 0 : 1, cs.cm.pt[1])
end

norm(cs::CholmodSparse) = norm(cs, Inf)

## Approximate minimal degree ordering
function chm_amd(cs::CholmodSparse)
    aa = Array(BlasInt, cs.cp.m)
    st = cs.stype == 0 ? ccall(_chm_colamd, BlasInt,
                               (Ptr{Void}, Ptr{Void}, Uint, BlasInt, Ptr{BlasInt}, Ptr{Void}),
                               cs.pt.val[1], C_NULL, 0, 1, aa, cs.cm.pt[1]) :
                         ccall(_chm_amd, BlasInt, (Ptr{Void}, Ptr{Void}, Uint, Ptr{BlasInt}, Ptr{Void}),
                               cs.pt.val[1], C_NULL, 0, aa, cs.cm.pt[1])
    if st != CHOLMOD_TRUE error("Error in cholmod_amd") end
    aa
end

type CholmodFactor{Tv<:CHMVTypes,Ti<:CHMITypes} <: Factorization{Tv}
    pt::CholmodPtr{Tv,Ti}
    cs::CholmodSparse{Tv,Ti}
    function CholmodFactor(pt::CholmodPtr{Tv,Ti}, cs::CholmodSparse{Tv,Ti})
        ff = new(pt, cs)
        finalizer(ff, cholmod_factor_finalizer)
        ff
    end
end

function cholmod_factor_finalizer(x::CholmodFactor)
    if ccall(_chm_free_fa, BlasInt, (Ptr{Void}, Ptr{Void}), x.pt.val, x.cs.cm[1]) != CHOLMOD_TRUE
        error("CHOLMOD error in cholmod_free_factor")
    end
end

function size(F::CholmodFactor)
    n = size(F.cs,1)
    (n, n)
end

eltype{T}(F::CholmodFactor{T}) = T
indtype{Tv,Ti}(F::CholmodFactor{Tv,Ti}) = Ti

function CholmodFactor{Tv,Ti}(cs::CholmodSparse{Tv,Ti})
    pt = CholmodPtr{Tv,Ti}(Array(Ptr{Void}, 1))
    pt.val[1] = ccall(_chm_analyze, Ptr{Void},
                      (Ptr{Void}, Ptr{Void}), cs.pt.val[1], cs.cm.pt[1])
    st = ccall(_chm_factorize, BlasInt,
               (Ptr{Void}, Ptr{Void}, Ptr{Void}), cs.pt.val[1], pt.val[1], cs.cm.pt[1])
    if st != CHOLMOD_TRUE error("CHOLMOD failure in factorize") end
    CholmodFactor{Tv,Ti}(pt, cs)
end

function show(io::IO, cf::CholmodFactor)
    st = ccall(_chm_print_fa, BlasInt, (Ptr{Void}, Ptr{Uint8}, Ptr{Void}), cf.pt.val[1], "", cf.cs.cm.pt[1])
    if st != CHOLMOD_TRUE error("Cholmod error in print_factor") end
end

type CholmodDense{T<:CHMVTypes}
    pt::Vector{Ptr{Void}}
    m::Int
    n::Int
    aa::VecOrMat{T}                     # original array
    cm::CholmodCommon
end

function CholmodDense{T<:CHMVTypes}(b::VecOrMat{T}, cm::CholmodCommon)
    m = size(b, 1)
    n = isa(b, Matrix) ? size(b, 2) : 1

    xtype = T <: Complex ? CHOLMOD_COMPLEX : CHOLMOD_REAL
    dtype = T <: Float32 || T == Complex64 ? CHOLMOD_SINGLE : CHOLMOD_DOUBLE

    pt = Array(Ptr{Void}, 1)

    ccall((:jl_cholmod_dense, libsuitesparse_wrapper), Void,
          (Ptr{Void}, Uint, Uint, Uint, Uint, Ptr{Void}, Ptr{Void}, BlasInt, Int),
          pt, m, n, length(b), m, b, C_NULL, xtype, dtype)
    finalizer(pt, x->c_free(pt[1]))
    CholmodDense{T}(pt, m, n, copy(b), cm)
end

CholmodDense{T<:Integer}(B::VecOrMat{T}, cm::CholmodCommon) = CholmodDense(float64(B), cm)
    
size(cd::CholmodDense) = (cd.m, cd.n)

function show(io::IO, cd::CholmodDense)
    st = ccall(_chm_print_dn, BlasInt, (Ptr{Void},Ptr{Uint8},Ptr{Void}), cd.pt[1], "", cd.cm.pt[1])
    if st != CHOLMOD_TRUE error("Cholmod error in print_dense") end
end

type CholmodDenseOut{Tv<:CHMVTypes,Ti<:CHMITypes}
    pt::CholmodPtr{Tv,Ti}
    m::Int
    n::Int
    cm::CholmodCommon
    function CholmodDenseOut(pt::CholmodPtr{Tv,Ti}, m::BlasInt, n::BlasInt, cm::CholmodCommon)
        dd = new(pt, m, n, cm)
        finalizer(dd, cholmod_denseout_finalizer)
        dd
    end
end

function cholmod_denseout_finalizer(cd::CholmodDenseOut)
    st = ccall(_chm_free_dn, BlasInt, (Ptr{Void}, Ptr{Void}), cd.pt.val, cd.cm.pt[1])
    if st != CHOLMOD_TRUE error("Error in cholmod_free_dense") end
end

eltype{T}(cdo::CholmodDenseOut{T}) = T
indtype{Tv,Ti}(cdo::CholmodDenseOut{Tv,Ti}) = Ti
size(cd::CholmodDenseOut) = (cd.m, cd.n)

function convert{T}(::Type{Array{T}}, cdo::CholmodDenseOut{T})
    mm = Array(T, size(cdo))
    ccall((:jl_cholmod_dense_copy_out, libsuitesparse_wrapper), Void,
          (Ptr{Void}, Ptr{T}), cdo.pt.val[1], mm)
    mm
end

function solve{Tv,Ti}(cf::CholmodFactor{Tv,Ti}, B::CholmodDense{Tv}, solv::Integer)
    m, n = size(B)
    cdo = CholmodPtr{Tv,Ti}(Array(Ptr{Void},1))
    cdo.val[1] = ccall(_chm_solve, Ptr{Void},
                       (BlasInt, Ptr{Void}, Ptr{Void}, Ptr{Void}),
                       solv, cf.pt.val[1], B.pt[1], cf.cs.cm.pt[1])
    return cdo, m, n, cf.cs.cm
    CholmodDenseOut(cdo, m, n, cf.cs.cm)
end

solve(cf::CholmodFactor, B::CholmodDense) = solve(cf, B, CHOLMOD_A)

(\){Tf,Tb}(cf::CholmodFactor{Tf}, b::VecOrMat{Tb}) = solve(cf, CholmodDense{Tf}(convert(Array{Tf},b), cf.cs.cm), CHOLMOD_A)

type CholmodSparseOut{Tv<:CHMVTypes,Ti<:CHMITypes}
    pt::CholmodPtr{Tv,Ti}
    m::Int
    n::Int
    cm::CholmodCommon
    function CholmodSparseOut(pt::CholmodPtr{Tv,Ti}, m::BlasInt, n::BlasInt, cm::CholmodCommon)
        cso = new(pt, m, n, cm)
        finalizer(cso, cholmod_sparseout_finalizer)
        cso
    end
end

function cholmod_sparseout_finalizer(cso::CholmodSparseOut)
    st = ccall(_chm_free_sp, BlasInt,
               (Ptr{Void}, Ptr{Void}), cso.pt.val, cso.cm.pt[1])
    if st != CHOLMOD_TRUE error("Error in cholmod_free_sparse") end
end

function nnz(cso::CholmodSparseOut)
    ccall((:cholmod_nnz, libcholmod), BlasInt,
          (Ptr{Void}, Ptr{Void}), cso.pt.val[1], cso.cm.pt[1])
end
size(cso::CholmodSparseOut) = (cso.m, cso.n)
eltype{T}(cso::CholmodSparseOut{T}) = T
indtype{Tv,Ti}(cso::CholmodSparseOut{Tv,Ti}) = Ti

function solve{Tv,Ti}(cf::CholmodFactor{Tv,Ti}, B::CholmodSparse{Tv,Ti}, solv::Integer)
    m, n = size(B)
    cso = CholmodPtr{Tv,Ti}(Array(Ptr{Void},1))
    cso.val[1] = ccall((:cholmod_spsolve, libcholmod), Ptr{Void},
                       (BlasInt, Ptr{Void}, Ptr{Void}, Ptr{Void}),
                       solv, cf.pt.val[1], B.pt[1], B.cm.pt[1])
    CholmodSparseOut{Tv,Ti}(cso, m, n, cf.cs.cm)
end

function CholmodSparseOut{Tv,Ti}(cf::CholmodFactor{Tv,Ti})
    n = size(cf.cs)[1]
    cso = CholmodPtr{Tv,Ti}(Array(Ptr{Void},1))
    cso.val[1] = ccall((:cholmod_factor_to_sparse, libcholmod), Ptr{Void},
                       (Ptr{Void}, Ptr{Void}), cf.pt.val[1], cf.cs.cm.pt[1])
    CholmodSparseOut{Tv,Ti}(cso, n, n, cf.cs.cm)
end

function SparseMatrixCSC{Tv,Ti}(cso::CholmodSparseOut{Tv,Ti})
    nz = nnz(cso)
    sp = SparseMatrixCSC{Tv,Ti}(cso.m, cso.n, Array(Ti, cso.n + 1), Array(Ti, nz), Array(Tv, nz))
    st = ccall((:jl_cholmod_sparse_copy_out, libsuitesparse_wrapper), BlasInt,
                (Ptr{Void}, Ptr{Ti}, Ptr{Ti}, Ptr{Tv}),
                cso.pt.val[1], sp.colptr, sp.rowval, sp.nzval)
    if st == 1 error("CholmodSparseOut object is not packed") end
    if st == 2 error("CholmodSparseOut object is not sorted") end # Should not occur
    if st == 3 error("CholmodSparseOut object has INTLONG itype") end
    convert_to_1_based_indexing!(sp)
end

function show(io::IO, cso::CholmodSparseOut)
    sp = ccall(_chm_print_sp, BlasInt, (Ptr{Void}, Ptr{Uint8},Ptr{Void}), cso.pt.val[1], "", cso.cm.pt[1])
    if sp != CHOLMOD_TRUE error("Cholmod error in print_sparse") end
end

function chm_aat{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, symm::Bool)
    cs = CholmodSparse(A, 0)
    aa = CholmodPtr{Tv,Ti}(Array(Ptr{Void},1))
    aa.val[1] = ccall(_chm_aat, Ptr{Void}, (Ptr{Void},Ptr{BlasInt},BlasInt,BlasInt,Ptr{Void}),
                      cs.pt.val[1], C_NULL, 0, 1, cs.cm.pt[1])
    if ccall(_chm_sort, BlasInt, (Ptr{Void}, Ptr{Void}), aa.val[1], cs.cm.pt[1]) != CHOLMOD_TRUE
        error("Cholmod error in sort")
    end
    if symm
        pt = ccall(_chm_copy, Ptr{Void}, (Ptr{Void}, BlasInt, BlasInt, Ptr{Void}),
                   aa.val[1], 1, 1, cs.cm.pt[1])
        if ccall(_chm_free_sp, BlasInt, (Ptr{Void}, Ptr{Void}), aa.val, cs.cm.pt[1]) != CHOLMOD_TRUE
            error("Cholmod error in free_sparse")
        end
        aa.val[1] = pt
    end
    m  = size(A, 1)
    CholmodSparseOut{Tv,Ti}(aa, m, m, cs.cm)
end

chm_aat{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}) = chm_aat(A, false)

## call wrapper function to create cholmod_sparse objects
cholmod_sparse(S) = cholmod_sparse(S, 0)

function cholmod_sparse{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}, stype::Int)
    cs = Array(Ptr{Void}, 1)

    if     Ti == Int; itype = CHOLMOD_INT;
    elseif Ti == Int64; itype = CHOLMOD_LONG; end

    if     Tv == Float64    || Tv == Float32;    xtype = CHOLMOD_REAL;
    elseif Tv == Complex128 || Tv == Complex64 ; xtype = CHOLMOD_COMPLEX; end

    if     Tv == Float64 || Tv == Complex128; dtype = CHOLMOD_DOUBLE; 
    elseif Tv == Float32 || Tv == Complex64 ; dtype = CHOLMOD_SINGLE; end

    ccall((:jl_cholmod_sparse, libsuitesparse_wrapper),
          Ptr{Void},
          (Ptr{Void}, BlasInt, BlasInt, BlasInt, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void},
           BlasInt, BlasInt, BlasInt, BlasInt, BlasInt, Int),
          cs, blas_int(S.m), blas_int(S.n), blas_int(length(S.nzval)), S.colptr, S.rowval, C_NULL, S.nzval, C_NULL,
          int32(stype), itype, xtype, dtype, CHOLMOD_TRUE, CHOLMOD_TRUE
          )

    return cs
end

## Call wrapper function to create cholmod_dense objects
function cholmod_dense{T}(B::VecOrMat{T})
    m = size(B, 1)
    n = isa(B, Matrix) ? size(B, 2) : 1

    cd = Array(Ptr{Void}, 1)

    if     T == Float64    || T == Float32;    xtype = CHOLMOD_REAL;
    elseif T == Complex128 || T == Complex64 ; xtype = CHOLMOD_COMPLEX; end

    if     T == Float64 || T == Complex128; dtype = CHOLMOD_DOUBLE; 
    elseif T == Float32 || T == Complex64 ; dtype = CHOLMOD_SINGLE; end

    ccall((:jl_cholmod_dense, libsuitesparse_wrapper),
          Ptr{Void},
          (Ptr{Void}, BlasInt, BlasInt, BlasInt, BlasInt, Ptr{T}, Ptr{Void}, BlasInt, Int),
          cd, m, n, length(B), m, B, C_NULL, xtype, dtype
          )

    return cd
end

function cholmod_dense_copy_out{T}(x::Ptr{Void}, sol::VecOrMat{T})
    ccall((:jl_cholmod_dense_copy_out, libsuitesparse_wrapper),
          Void,
          (Ptr{Void}, Ptr{T}),
          x, sol
          )
    return sol
end

function cholmod_transpose_unsym{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}, cm::Array{Ptr{Void}, 1})
    S_t = SparseMatrixCSC(Tv, S.n, S.m, nnz(S)+1)

    # Allocate space for a cholmod_sparse object
    cs = cholmod_sparse(S)
    cs_t = cholmod_sparse(S_t)
    
    status = ccall((:cholmod_transpose_unsym),
                   Int32,
                   (Ptr{Void}, BlasInt, Ptr{BlasInt}, Ptr{BlasInt}, BlasInt, Ptr{Void}, Ptr{Void}),
                   cs[1], int32(1), C_NULL, C_NULL, int32(-1), cs_t[1], cm[1]);

    # Deallocate space for cholmod_sparse objects
    c_free(cs[1])
    c_free(cs_t[1])

    return S_t
end

function cholmod_analyze{Tv<:Union(Float64,Complex128), Ti<:CHMITypes}(cs::Array{Ptr{Void},1}, cm::Array{Ptr{Void},1})
    ccall(_chm_analyze, Ptr{Void}, (Ptr{Void}, Ptr{Void}), cs[1], cm[1])
end

function cholmod_factorize{Tv<:Union(Float64,Complex128), Ti<:CHMITypes}(cs::Array{Ptr{Void},1}, cs_factor::Ptr{Void}, cm::Array{Ptr{Void},1})
    st = ccall(_chm_factorize, BlasInt, (Ptr{Void}, Ptr{Void}, Ptr{Void}), cs[1], cs_factor, cm[1])
    if st != CHOLMOD_TRUE error("CHOLMOD could not factorize the matrix") end
end

function cholmod_solve(cs_factor::Ptr{Void}, cd_rhs::Array{Ptr{Void},1}, cm::Array{Ptr{Void},1})
    ccall(_chm_solve, Ptr{Void}, (BlasInt, Ptr{Void}, Ptr{Void}, Ptr{Void}),
          CHOLMOD_A, cs_factor, cd_rhs[1], cm[1])
end

## UMFPACK

# Wrapper for memory allocated by umfpack. Carry along the value and index types.
type UmfpackPtr{Tv<:Union(Float64,Complex128),Ti<:CHMITypes}
    val::Vector{Ptr{Void}} 
end

type UmfpackLU{Tv<:Union(Float64,Complex128),Ti<:CHMITypes} <: Factorization{Tv}
    numeric::UmfpackPtr{Tv,Ti}
    mat::SparseMatrixCSC{Tv,Ti}
end

function show(io::IO, f::UmfpackLU)
    @printf(io, "UMFPACK LU Factorization of a %d-by-%d sparse matrix\n",
           size(f.mat,1), size(f.mat,2))
    println(f.numeric)
    umfpack_report(f)
end

type UmfpackLUTrans{Tv<:Union(Float64,Complex128),Ti<:CHMITypes} <: Factorization{Tv}
    numeric::UmfpackPtr{Tv,Ti}
    mat::SparseMatrixCSC{Tv,Ti}
end

function show(io::IO, f::UmfpackLUTrans)
    @printf(io, "UMFPACK LU Factorization of a transposed %d-by-%d sparse matrix\n",
           size(f.mat,1), size(f.mat,2))
    println(f.numeric)
    umfpack_report(f)
end

function UmfpackLU{Tv<:Union(Float64,Complex128),Ti<:CHMITypes}(S::SparseMatrixCSC{Tv,Ti})
    Scopy = copy(S) 
    Scopy = convert_to_0_based_indexing!(Scopy)
    numeric = []

    try
        symbolic = umfpack_symbolic(Scopy)
        numeric = umfpack_numeric(Scopy, symbolic)
    catch e
        if is(e,MatrixIllConditionedException)
            error("Input matrix is ill conditioned or singular");
        else
            error("Error calling UMFPACK")
        end
    end
    
    return UmfpackLU(numeric,Scopy) 
end

function UmfpackLU!{Tv<:Union(Float64,Complex128),Ti<:CHMITypes}(S::SparseMatrixCSC{Tv,Ti})
    Sshallow = SparseMatrixCSC(S.m,S.n,S.colptr,S.rowval,S.nzval)
    Sshallow = convert_to_0_based_indexing!(Sshallow)
    numeric = []

    try
        symbolic = umfpack_symbolic(Sshallow)
        numeric = umfpack_numeric(Sshallow, symbolic)
    catch e
        Sshallow = convert_to_1_based_indexing!(Sshallow)
        if is(e,MatrixIllConditionedException)
            error("Input matrix is ill conditioned or singular");
        else
            error("Error calling UMFPACK")
        end
    end

    S.rowval = []
    S.nzval = []
    S.colptr = ones(S.n+1)
    
    return UmfpackLU(numeric,Sshallow)
end

function UmfpackLUTrans(S::SparseMatrixCSC) 
    x = UmfpackLU(S)
    return UmfpackLUTrans(x.numeric, x.mat)
end

# Solve with Factorization

(\){T}(fact::UmfpackLU{T}, b::Vector) = fact \ convert(Array{T,1}, b)
(\){T}(fact::UmfpackLU{T}, b::Vector{T}) = umfpack_solve(fact.mat,b,fact.numeric)

(\){T}(fact::UmfpackLUTrans{T}, b::Vector) = fact \ convert(Array{T,1}, b)
(\){T}(fact::UmfpackLUTrans{T}, b::Vector{T}) = umfpack_transpose_solve(fact.mat,b,fact.numeric)

ctranspose(fact::UmfpackLU) = UmfpackLUTrans(fact.numeric, fact.mat)

# Solve directly with matrix

(\)(S::SparseMatrixCSC, b::Vector) = UmfpackLU(S) \ b
At_ldiv_B(S::SparseMatrixCSC, b::Vector) = UmfpackLUTrans(S) \ b
Ac_ldiv_B(S::SparseMatrixCSC, b::Vector) = UmfpackLUTrans(S) \ b

## Wrappers around UMFPACK routines

for (f_sym_r, f_sym_c, inttype) in
    (("umfpack_di_symbolic","umfpack_zi_symbolic",:Int32),
     ("umfpack_dl_symbolic","umfpack_zl_symbolic",:Int64))
    @eval begin

        function umfpack_symbolic{Tv<:Float64,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti})
            # Pointer to store the symbolic factorization returned by UMFPACK
            Symbolic = UmfpackPtr{Tv,Ti}(Array(Ptr{Void},1))
            status = ccall(($f_sym_r, libumfpack),
                           Ti,
                           (Ti, Ti, 
                            Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                           S.m, S.n, 
                           S.colptr, S.rowval, S.nzval, Symbolic.val, C_NULL, C_NULL)
            if status != UMFPACK_OK; error("Error in symbolic factorization"); end
            finalizer(Symbolic,umfpack_free_symbolic)
            return Symbolic
        end

        function umfpack_symbolic{Tv<:Complex128,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti})
            # Pointer to store the symbolic factorization returned by UMFPACK
            Symbolic = UmfpackPtr{Tv,Ti}(Array(Ptr{Void},1))
            status = ccall(($f_sym_c, libumfpack),
                           Ti,
                           (Ti, Ti, 
                            Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Float64}, Ptr{Void}, 
                            Ptr{Float64}, Ptr{Float64}),
                           S.m, S.n, 
                           S.colptr, S.rowval, real(S.nzval), imag(S.nzval), Symbolic.val, 
                           C_NULL, C_NULL)
            if status != UMFPACK_OK; error("Error in symbolic factorization"); end
            finalizer(Symbolic,umfpack_free_symbolic) # Check: do we need to free if there was an error?
            return Symbolic
        end

    end
end

for (f_num_r, f_num_c, inttype) in
    (("umfpack_di_numeric","umfpack_zi_numeric",:Int32),
     ("umfpack_dl_numeric","umfpack_zl_numeric",:Int64))
    @eval begin

        function umfpack_numeric{Tv<:Float64,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti}, Symbolic)
            # Pointer to store the numeric factorization returned by UMFPACK
            Numeric = UmfpackPtr{Tv,Ti}(Array(Ptr{Void},1))
            status = ccall(($f_num_r, libumfpack),
                           Ti,
                           (Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Void}, Ptr{Void}, 
                            Ptr{Float64}, Ptr{Float64}),
                           S.colptr, S.rowval, S.nzval, Symbolic.val[1], Numeric.val, 
                           C_NULL, C_NULL)
            if status > 0; throw(MatrixIllConditionedException); end
            if status != UMFPACK_OK; error("Error in numeric factorization"); end
            finalizer(Numeric,umfpack_free_numeric)
            return Numeric
        end

        function umfpack_numeric{Tv<:Complex128,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti}, Symbolic)
            # Pointer to store the numeric factorization returned by UMFPACK
            Numeric = UmfpackPtr{Tv,Ti}(Array(Ptr{Void},1))
            status = ccall(($f_num_c, libumfpack),
                           Ti,
                           (Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Float64}, Ptr{Void}, Ptr{Void}, 
                            Ptr{Float64}, Ptr{Float64}),
                           S.colptr, S.rowval, real(S.nzval), imag(S.nzval), Symbolic.val[1], Numeric.val, 
                           C_NULL, C_NULL)
            if status > 0; throw(MatrixIllConditionedException); end
            if status != UMFPACK_OK; error("Error in numeric factorization"); end
            finalizer(Numeric,umfpack_free_numeric)
            return Numeric
        end

    end
end

for (f_sol_r, f_sol_c, inttype) in
    (("umfpack_di_solve","umfpack_zi_solve",:Int32),
     ("umfpack_dl_solve","umfpack_zl_solve",:Int64))
    @eval begin

        function umfpack_solve{Tv<:Float64,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti}, 
                                                             b::Vector{Tv}, Numeric::UmfpackPtr{Tv,Ti})
            x = similar(b)
            status = ccall(($f_sol_r, libumfpack),
                           Ti,
                           (Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, 
                            Ptr{Float64}, Ptr{Float64}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                           UMFPACK_A, S.colptr, S.rowval, S.nzval, 
                           x, b, Numeric.val[1], C_NULL, C_NULL)
            if status != UMFPACK_OK; error("Error in solve"); end
            return x
        end

        function umfpack_solve{Tv<:Complex128,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti}, 
                                                                b::Vector{Tv}, Numeric::UmfpackPtr{Tv,Ti})
            xr = similar(b, Float64)
            xi = similar(b, Float64)
            status = ccall(($f_sol_c, libumfpack),
                           Ti,
                           (Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, 
                            Ptr{Float64}, Ptr{Float64}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                           UMFPACK_A, S.colptr, S.rowval, real(S.nzval), imag(S.nzval), 
                           xr, xi, real(b), imag(b), Numeric.val[1], C_NULL, C_NULL)
            if status != UMFPACK_OK; error("Error in solve"); end
            return complex(xr,xi)
        end

        function umfpack_transpose_solve{Tv<:Float64,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti}, 
                                                             b::Vector{Tv}, Numeric::UmfpackPtr{Tv,Ti})
            x = similar(b)
            status = ccall(($f_sol_r, libumfpack),
                           Ti,
                           (Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, 
                            Ptr{Float64}, Ptr{Float64}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                           UMFPACK_At, S.colptr, S.rowval, S.nzval, 
                           x, b, Numeric.val[1], C_NULL, C_NULL)
            if status != UMFPACK_OK; error("Error in solve"); end
            return x
        end

        function umfpack_transpose_solve{Tv<:Complex128,Ti<:$inttype}(S::SparseMatrixCSC{Tv,Ti}, 
                                                                b::Vector{Tv}, Numeric::UmfpackPtr{Tv,Ti})
            xr = similar(b, Float64)
            xi = similar(b, Float64)
            status = ccall(($f_sol_c, libumfpack),
                           Ti,
                           (Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, 
                            Ptr{Float64}, Ptr{Float64}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                           UMFPACK_At, S.colptr, S.rowval, real(S.nzval), imag(S.nzval), 
                           xr, xi, real(b), imag(b), Numeric.val[1], C_NULL, C_NULL)
            if status != UMFPACK_OK; error("Error in solve"); end
            return complex(xr,xi)
        end

    end
end

for (f_report, elty, inttype) in
    (("umfpack_di_report_numeric", :Float64,    :Int),
     ("umfpack_zi_report_numeric", :Complex128, :Int),
     ("umfpack_dl_report_numeric", :Float64,    :Int64),
     ("umfpack_zl_report_numeric", :Complex128, :Int64))
     @eval begin

         function umfpack_report{Tv<:$elty,Ti<:$inttype}(slu::UmfpackLU{Tv,Ti})

             control = zeros(Float64, UMFPACK_CONTROL)
             control[UMFPACK_PRL] = 4
         
             ccall(($f_report, libumfpack),
                   Ti,
                   (Ptr{Void}, Ptr{Float64}),
                   slu.numeric.val[1], control)
         end

    end
end


for (f_symfree, f_numfree, elty, inttype) in
    (("umfpack_di_free_symbolic","umfpack_di_free_numeric",:Float64,:Int32),
     ("umfpack_zi_free_symbolic","umfpack_zi_free_numeric",:Complex128,:Int32),
     ("umfpack_dl_free_symbolic","umfpack_dl_free_numeric",:Float64,:Int64),
     ("umfpack_zl_free_symbolic","umfpack_zl_free_numeric",:Complex128,:Int64))
    @eval begin

        umfpack_free_symbolic{Tv<:$elty,Ti<:$inttype}(Symbolic::UmfpackPtr{Tv,Ti}) =
        ccall(($f_symfree, libumfpack), Void, (Ptr{Void},), Symbolic.val)
        
        umfpack_free_numeric{Tv<:$elty,Ti<:$inttype}(Numeric::UmfpackPtr{Tv,Ti}) =
        ccall(($f_numfree, libumfpack), Void, (Ptr{Void},), Numeric.val)
        
    end
end

end #module
