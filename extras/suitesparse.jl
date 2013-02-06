module SuiteSparse

import Base.SparseMatrixCSC, Base.size, Base.nnz, Base.eltype, Base.show
import Base.triu, Base.norm, Base.solve, Base.(\), Base.lu
import Base.Ac_ldiv_B, Base.At_ldiv_B, Base.lud, Base.lud!

import Base.BlasInt
import Base.blas_int

export                                  # types
    CholmodPtr,
    CholmodCommon,
    CholmodSparse,
    CholmodFactor,
    CholmodDense,
    CholmodSparseOut,
    UmfpackLU,                          # call these lud and lud! instead?
    UmfpackLU!,
                                        # methods
    decrement,
    decrement!,
    increment,
    increment!,
    indtype,
    show_umf_ctrl,
    show_umf_info

include("suitesparse_h.jl")

type MatrixIllConditionedException <: Exception end

function decrement!{T<:Integer}(A::AbstractArray{T})
    for i in 1:length(A) A[i] -= one(T) end
    A
end
function decrement{T<:Integer}(A::AbstractArray{T})
    B = similar(A)
    for i in 1:length(B) B[i] = A[i] - one(T) end
    B
end
function increment!{T<:Integer}(A::AbstractArray{T})
    for i in 1:length(A) A[i] += one(T) end
    A
end
function increment{T<:Integer}(A::AbstractArray{T})
    increment!(copy(A))
end

typealias CHMITypes Union(Int32,Int64)       # also ITypes for UMFPACK
typealias CHMVTypes Union(Complex64, Complex128, Float32, Float64)
typealias UMFVTypes Union(Float64,Complex128)

## UMFPACK

# the control and info arrays
const umf_ctrl = Array(Float64, UMFPACK_CONTROL)
ccall((:umfpack_dl_defaults, :libumfpack), Void, (Ptr{Float64},), umf_ctrl)
const umf_info = Array(Float64, UMFPACK_INFO)

function show_umf_ctrl(level::Real)
    old_prt::Float64 = umf_ctrl[1]
    umf_ctrl[1] = float64(level)
    ccall((:umfpack_dl_report_control, :libumfpack), Void, (Ptr{Float64},), umf_ctrl)
    umf_ctrl[1] = old_prt
end
show_umf_ctrl() = show_umf_ctrl(2.)

function show_umf_info(level::Real)
    old_prt::Float64 = umf_ctrl[1]
    umf_ctrl[1] = float64(level)
    ccall((:umfpack_dl_report_info, :libumfpack), Void,
          (Ptr{Float64}, Ptr{Float64}), umf_ctrl, umf_info)
    umf_ctrl[1] = old_prt
end
show_umf_info() = show_umf_info(2.)

# Wrapper for memory allocated by umfpack. Carry along the value and index types.
## type UmfpackPtr{Tv<:UMFVTypes,Ti<:CHMITypes}
##     val::Vector{Ptr{Void}} 
## end

type UmfpackLU{Tv<:UMFVTypes,Ti<:CHMITypes} <: Factorization{Tv}
    symbolic::Ptr{Void}
    numeric::Ptr{Void}
    m::Int
    n::Int
    colptr::Vector{Ti}                  # 0-based column pointers
    rowval::Vector{Ti}                  # 0-based row indices
    nzval::Vector{Tv}
end

function lud{Tv<:UMFVTypes,Ti<:CHMITypes}(S::SparseMatrixCSC{Tv,Ti})
    zerobased = S.colptr[1] == 0
    lu = UmfpackLU(C_NULL, C_NULL, S.m, S.n,
                   zerobased ? copy(S.colptr) : decrement(S.colptr),
                   zerobased ? copy(S.rowval) : decrement(S.rowval),
                   copy(S.nzval))
    umfpack_numeric!(lu)
end

function lud!{Tv<:UMFVTypes,Ti<:CHMITypes}(S::SparseMatrixCSC{Tv,Ti})
    zerobased = S.colptr[1] == 0
    UmfpackLU(C_NULL, C_NULL, S.m, S.n,
              zerobased ? S.colptr : decrement!(S.colptr),
              zerobased ? S.rowval : decrement!(S.rowval),
              S.nzval)
end

function show(io::IO, f::UmfpackLU)
    @printf(io, "UMFPACK LU Factorization of a %d-by-%d sparse matrix\n",
           f.m, f.n)
    if f.numeric != C_NULL println(f.numeric) end
end

# Solve with Factorization

(\){T<:UMFVTypes}(fact::UmfpackLU{T}, b::Vector{T}) = umfpack_solve(fact, b)
(\){Ts<:UMFVTypes,Tb<:Number}(fact::UmfpackLU{Ts}, b::Vector{Tb}) = fact\convert(Vector{Ts},b)

# Solve directly with matrix

(\)(S::SparseMatrixCSC, b::Vector) = lud(S) \ b
At_ldiv_B{T<:UMFVTypes}(S::SparseMatrixCSC{T}, b::Vector{T}) = umfpack_solve(lud(S), b, UMFPACK_Aat)
function At_ldiv_B{Ts<:UMFVTypes,Tb<:Number}(S::SparseMatrixCSC{Ts}, b::Vector{Tb})
    ## should be more careful here in case Ts<:Real and Tb<:Complex
    At_ldiv_B(S, convert(Vector{Ts}, b))
end
Ac_ldiv_B{T<:UMFVTypes}(S::SparseMatrixCSC{T}, b::Vector{T}) = umfpack_solve(lud(S), b, UMFPACK_At)
function Ac_ldiv_B{Ts<:UMFVTypes,Tb<:Number}(S::SparseMatrixCSC{Ts}, b::Vector{Tb})
    ## should be more careful here in case Ts<:Real and Tb<:Complex
    Ac_ldiv_B(S, convert(Vector{Ts}, b))
end

## Wrappers around UMFPACK routines

for (f_sym_r, f_num_r, f_sym_c, f_num_c, itype) in
    (("umfpack_di_symbolic","umfpack_di_numeric","umfpack_zi_symbolic","umfpack_zi_numeric",:Int32),
     ("umfpack_dl_symbolic","umfpack_dl_numeric","umfpack_zl_symbolic","umfpack_zl_numeric",:Int64))
    @eval begin
        function umfpack_symbolic!{Tv<:Float64,Ti<:$itype}(U::UmfpackLU{Tv,Ti})
            if U.symbolic != C_NULL return U end
            tmp = Array(Ptr{Void},1)
            status = ccall(($f_sym_r, :libumfpack), Ti,
                           (Ti, Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Tv}, Ptr{Void},
                            Ptr{Float64}, Ptr{Float64}),
                           U.m, U.n, U.colptr, U.rowval, U.nzval, tmp,
                           umf_ctrl, umf_info)
            if status != UMFPACK_OK; error("Error code $status from symbolic factorization"); end
            U.symbolic = tmp[1]
            finalizer(U.symbolic,umfpack_free_symbolic)
            U
        end

        function umfpack_symbolic!{Tv<:Complex128,Ti<:$itype}(U::UmfpackLU{Tv,Ti})
            if U.symbolic != C_NULL return U end
            tmp = Array(Ptr{Void},1)
            status = ccall(($f_sym_r, :libumfpack), Ti,
                           (Ti, Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Float64}, Ptr{Void},
                            Ptr{Float64}, Ptr{Float64}),
                           U.m, U.n, U.colptr, U.rowval, real(U.nzval), imag(U.nzval), tmp,
                           umf_ctrl, umf_info)
            if status != UMFPACK_OK; error("Error code $status from symbolic factorization"); end
            U.symbolic = tmp[1]
            finalizer(U.symbolic,umfpack_free_symbolic)
            U
        end
        
        function umfpack_numeric!{Tv<:Float64,Ti<:$itype}(U::UmfpackLU{Tv,Ti})
            if U.numeric != C_NULL return U end
            if U.symbolic == C_NULL umfpack_symbolic!(U) end
            tmp = Array(Ptr{Void}, 1)
            status = ccall(($f_num_r, :libumfpack), Ti,
                           (Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Void}, Ptr{Void}, 
                            Ptr{Float64}, Ptr{Float64}),
                           U.colptr, U.rowval, U.nzval, U.symbolic, tmp,
                           umf_ctrl, umf_info)
            if status > 0; throw(MatrixIllConditionedException); end
            if status != UMFPACK_OK; error("Error code $status from numeric factorization"); end
            U.numeric = tmp[1]
            finalizer(U.numeric,umfpack_free_numeric)
            U
        end

        function umfpack_numeric!{Tv<:Complex128,Ti<:$itype}(U::UmfpackLU{Tv,Ti})
            if U.numeric != C_NULL return U end
            if U.symbolic == C_NULL umfpack_symbolic!(U) end
            tmp = Array(Ptr{Void}, 1)
            status = ccall(($f_num_r, :libumfpack), Ti,
                           (Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Float64}, Ptr{Void}, Ptr{Void}, 
                            Ptr{Float64}, Ptr{Float64}),
                           U.colptr, U.rowval, real(U.nzval), imag(U.nzval), U.symbolic, tmp,
                           umf_ctrl, umf_info)
            if status > 0; throw(MatrixIllConditionedException); end
            if status != UMFPACK_OK; error("Error code $status from numeric factorization"); end
            U.numeric = tmp[1]
            finalizer(U.numeric,umfpack_free_numeric)
            U
        end
    end
end

for (f_sol_r, f_sol_c, inttype) in
    (("umfpack_di_solve","umfpack_zi_solve",:Int32),
     ("umfpack_dl_solve","umfpack_zl_solve",:Int64))
    @eval begin
        function umfpack_solve{Tv<:Float64,Ti<:$inttype}(lu::UmfpackLU{Tv,Ti}, b::Vector{Tv}, typ::Integer)
            umfpack_numeric!(lu)
            x = similar(b)
            status = ccall(($f_sol_r, :libumfpack), Ti,
                           (Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Float64},
                            Ptr{Float64}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                           typ, lu.colptr, lu.rowval, lu.nzval, x, b, lu.numeric, umf_ctrl, umf_info)
            if status != UMFPACK_OK; error("Error code $status in umfpack_solve"); end
            return x
        end

        function umfpack_solve{Tv<:Complex128,Ti<:$inttype}(lu::UmfpackLU{Tv,Ti}, b::Vector{Tv}, typ::Integer)
            umfpack_numeric!(lu)
            xr = similar(b, Float64)
            xi = similar(b, Float64)
            status = ccall(($f_sol_c, :libumfpack),
                           Ti,
                           (Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, 
                            Ptr{Float64}, Ptr{Float64}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                           typ, lu.colptr, lu.rowval, real(lu.nzval), imag(lu.nzval), 
                           xr, xi, real(b), imag(b), lu.num, umf_ctrl, umf_info)
            if status != UMFPACK_OK; error("Error code $status from umfpack_solve"); end
            return complex(xr,xi)
        end
    end
end

umfpack_solve(lu::UmfpackLU, b::Vector) = umfpack_solve(lu, b, UMFPACK_A)
 
## The C functions called by these Julia functions do not depend on
## the numeric and index types, even though the umfpack names indicate
## they do.  The umfpack_free_* functions can be called on C_NULL without harm.
function umfpack_free_symbolic(symb::Ptr{Void})
    tmp = [symb]
    ccall((:umfpack_dl_free_symbolic, :libumfpack), Void, (Ptr{Void},), tmp)
end

function umfpack_free_symbolic(lu::UmfpackLU)
    if lu.symbolic == C_NULL return lu end
    umfpack_free_numeric(lu)
    umfpack_free_symbolic(lu.symbolic)
    lu.symbolic = C_NULL
    lu
end

function umfpack_free_numeric(num::Ptr{Void})
    tmp = [num]
    ccall((:umfpack_dl_free_numeric, :libumfpack), Void, (Ptr{Void},), tmp)
end

function umfpack_free_symbolic(lu::UmfpackLU)
    if lu.numeric == C_NULL return lu end
    umfpack_free_numeric(lu.numeric)
    lu.numeric = C_NULL
    lu
end
 
function umfpack_report_symbolic(symb::Ptr{Void}, level::Real)
    old_prl::Float64 = umf_ctrl[UMFPACK_PRL]
    umf_ctrl[UMFPACK_PRL] = float64(level)
    status = ccall((:umfpack_dl_report_symbolic, :libumfpack), Int,
                   (Ptr{Void}, Ptr{Float64}), symb, umf_ctrl)
    umf_ctrl[UMFPACK_PRL] = old_prl
    if status != 0
        error("Error code $status from umfpack_report_symbolic")
    end
end

umfpack_report_symbolic(symb::Ptr{Void}) = umfpack_report_symbolic(symb, 4.)

function umfpack_report_symbolic(lu::UmfpackLU, level::Real)
    umfpack_report_symbolic(umfpack_symbolic!(lu).symbolic, level)
end

umfpack_report_symbolic(lu::UmfpackLU) = umfpack_report_symbolic(lu.symbolic,4.)
function umfpack_report_numeric(num::Ptr{Void}, level::Real)
    old_prl::Float64 = umf_ctrl[UMFPACK_PRL]
    umf_ctrl[UMFPACK_PRL] = float64(level)
    status = ccall((:umfpack_dl_report_numeric, :libumfpack), Int,
                   (Ptr{Void}, Ptr{Float64}), num, umf_ctrl)
    umf_ctrl[UMFPACK_PRL] = old_prl
    if status != 0
        error("Error code $status from umfpack_report_numeric")
    end
end

umfpack_report_numeric(num::Ptr{Void}) = umfpack_report_numeric(num, 4.)
function umfpack_report_numeric(lu::UmfpackLU, level::Real)
    umfpack_report_numeric(umfpack_numeric!(lu).symbolic, level)
end

umfpack_report_numeric(lu::UmfpackLU) = umfpack_report_numeric(lu.symbolic,4.)

## CHOLMOD

const chm_com_sz = ccall((:jl_cholmod_common_size, :libsuitesparse_wrapper), Int, ())

### Need one cholmod_common struct for Int32 and one for Int64
const chm_com    = Array(Uint8, chm_com_sz)
ccall((:cholmod_start, :libcholmod), Int32, (Ptr{Uint8},), chm_com)

const chm_l_com  = Array(Uint8, chm_com_sz)
ccall((:cholmod_l_start, :libcholmod), Int32, (Ptr{Uint8},), chm_l_com)

### These offsets should be reconfigured to be less error-prone in matches
const chm_com_offsets = Array(Int, 20)
ccall((:jl_cholmod_common_offsets, :libsuitesparse_wrapper),
      Void, (Ptr{Uint},), chm_com_offsets)
                                               
### A way of examining some of the fields in chm_com or chm_l_com
type ChmCommon
    dbound::Float64
    maxrank::Int
    supernodal_switch::Float64
    supernodal::Int32
    final_asis::Int32
    final_super::Int32
    final_ll::Int32
    final_pack::Int32
    final_monotonic::Int32
    final_resymbol::Int32
    prefer_zomplex::Int32               # should always be false
    prefer_upper::Int32
    print::Int32                        # print level. Default: 3
    precise::Int32                      # print 16 digits, otherwise 5
    nmethods::Int32                     # number of ordering methods
    selected::Int32
    postorder::Int32
    itype::Int32
    dtype::Int32
end

### there must be an easier way but at least this works.
function ChmCommon(aa::Array{Uint8,1})
    eval(Expr(:call,
              unshift!({reinterpret(ChmCommon.types[i],
                                    aa[(1:sizeof(ChmCommon.types[i])) +
                                       chm_com_offsets[i]])[1]
                        for i in 1:length(ChmCommon.types)}, :ChmCommon),
              Any))
end

chm_itype{Tv,Ti<:CHMITypes}(S::SparseMatrixCSC{Tv,Ti}) = Ti == Int32 ? CHOLMOD_INT : CHOLMOD_LONG
chm_xtype{Tv<:CHMVTypes}(S::SparseMatrixCSC{Tv}) = Tv <: Complex ? CHOLMOD_COMPLEX : CHOLMOD_REAL
chm_dtype{Tv<:CHMVTypes}(S::SparseMatrixCSC{Tv}) = T <: Union(Float32, Complex64) ? CHOLMOD_SINGLE : CHOLMOD_DOUBLE

# Wrapper for memory allocated by CHOLMOD. Carry along the value and index types.
## FIXME: CholmodPtr and UmfpackPtr should be amalgamated
type CholmodPtr{Tv<:CHMVTypes,Ti<:CHMITypes}
    val::Vector{Ptr{Void}} 
end

eltype{Tv,Ti}(P::CholmodPtr{Tv,Ti}) = Tv
indtype{Tv,Ti}(P::CholmodPtr{Tv,Ti}) = Ti

function cholmod_common_finalizer(x::Vector{Ptr{Void}})
    st = ccall((:cholmod_finish, :libcholmod), BlasInt, (Ptr{Void},), x[1])
    if st != CHOLMOD_TRUE error("Error calling cholmod_finish") end
    c_free(x[1])
end

type CholmodCommon
    pt::Vector{Ptr{Void}}
    function CholmodCommon()
        pt = Array(Ptr{Void}, 1)
        ccall((:jl_cholmod_common, :libsuitesparse_wrapper), Void,
              (Ptr{Void},), pt)
        st = ccall((:cholmod_start, :libcholmod), BlasInt, (Ptr{Void}, ), pt[1])
        if st != CHOLMOD_TRUE error("Error calling cholmod_start") end
        finalizer(pt, cholmod_common_finalizer)
        new(pt)
    end
end

function show(io::IO, cm::CholmodCommon)
    st = ccall((:cholmod_print_common, :libcholmod), BlasInt,
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
        
        ccall((:jl_cholmod_sparse, :libsuitesparse_wrapper), Void,
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
    ccall((:cholmod_norm_sparse, :libcholmod), Float64,
          (Ptr{Void}, BlasInt, Ptr{Void}), cs.pt.val[1], p == Inf ? 0 : 1, cs.cm.pt[1])
end

norm(cs::CholmodSparse) = norm(cs, Inf)

## Approximate minimal degree ordering
function chm_amd(cs::CholmodSparse)
    aa = Array(BlasInt, cs.cp.m)
    st = cs.stype == 0 ? ccall(:cholmod_colamd, BlasInt,
                               (Ptr{Void}, Ptr{Void}, Uint, BlasInt, Ptr{BlasInt}, Ptr{Void}),
                               cs.pt.val[1], C_NULL, 0, 1, aa, cs.cm.pt[1]) :
                         ccall(:cholmod_amd, BlasInt, (Ptr{Void}, Ptr{Void}, Uint, Ptr{BlasInt}, Ptr{Void}),
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
    if ccall((:cholmod_free_factor, :libcholmod), BlasInt, (Ptr{Void}, Ptr{Void}), x.pt.val, x.cs.cm[1]) != CHOLMOD_TRUE
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
    pt.val[1] = ccall(:cholmod_analyze, Ptr{Void},
                      (Ptr{Void}, Ptr{Void}), cs.pt.val[1], cs.cm.pt[1])
    st = ccall(:cholmod_factorize, BlasInt,
               (Ptr{Void}, Ptr{Void}, Ptr{Void}), cs.pt.val[1], pt.val[1], cs.cm.pt[1])
    if st != CHOLMOD_TRUE error("CHOLMOD failure in factorize") end
    CholmodFactor{Tv,Ti}(pt, cs)
end

function show(io::IO, cf::CholmodFactor)
    st = ccall(:cholmod_print_fa, BlasInt, (Ptr{Void}, Ptr{Uint8}, Ptr{Void}), cf.pt.val[1], "", cf.cs.cm.pt[1])
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

    ccall((:jl_cholmod_dense, :libsuitesparse_wrapper), Void,
          (Ptr{Void}, Uint, Uint, Uint, Uint, Ptr{Void}, Ptr{Void}, BlasInt, Int),
          pt, m, n, length(b), m, b, C_NULL, xtype, dtype)
    finalizer(pt, x->c_free(pt[1]))
    CholmodDense{T}(pt, m, n, copy(b), cm)
end

CholmodDense{T<:Integer}(B::VecOrMat{T}, cm::CholmodCommon) = CholmodDense(float64(B), cm)
    
size(cd::CholmodDense) = (cd.m, cd.n)

function show(io::IO, cd::CholmodDense)
    st = ccall(:cholmod_print_dn, BlasInt, (Ptr{Void},Ptr{Uint8},Ptr{Void}), cd.pt[1], "", cd.cm.pt[1])
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
    st = ccall((:cholmod_free_dense, :libcholmod), BlasInt, (Ptr{Void}, Ptr{Void}), cd.pt.val, cd.cm.pt[1])
    if st != CHOLMOD_TRUE error("Error in cholmod_free_dense") end
end

eltype{T}(cdo::CholmodDenseOut{T}) = T
indtype{Tv,Ti}(cdo::CholmodDenseOut{Tv,Ti}) = Ti
size(cd::CholmodDenseOut) = (cd.m, cd.n)

function convert{T}(::Type{Array{T}}, cdo::CholmodDenseOut{T})
    mm = Array(T, size(cdo))
    ccall((:jl_cholmod_dense_copy_out, :libsuitesparse_wrapper), Void,
          (Ptr{Void}, Ptr{T}), cdo.pt.val[1], mm)
    mm
end

function solve{Tv,Ti}(cf::CholmodFactor{Tv,Ti}, B::CholmodDense{Tv}, solv::Integer)
    m, n = size(B)
    cdo = CholmodPtr{Tv,Ti}(Array(Ptr{Void},1))
    cdo.val[1] = ccall(:cholmod_solve, Ptr{Void},
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
    st = ccall((:cholmod_free_sparse, :libcholmod), BlasInt,
               (Ptr{Void}, Ptr{Void}), cso.pt.val, cso.cm.pt[1])
    if st != CHOLMOD_TRUE error("Error in cholmod_free_sparse") end
end

function nnz(cso::CholmodSparseOut)
    ccall((:cholmod_nnz, :libcholmod), BlasInt,
          (Ptr{Void}, Ptr{Void}), cso.pt.val[1], cso.cm.pt[1])
end
size(cso::CholmodSparseOut) = (cso.m, cso.n)
eltype{T}(cso::CholmodSparseOut{T}) = T
indtype{Tv,Ti}(cso::CholmodSparseOut{Tv,Ti}) = Ti

function solve{Tv,Ti}(cf::CholmodFactor{Tv,Ti}, B::CholmodSparse{Tv,Ti}, solv::Integer)
    m, n = size(B)
    cso = CholmodPtr{Tv,Ti}(Array(Ptr{Void},1))
    cso.val[1] = ccall((:cholmod_spsolve, :libcholmod), Ptr{Void},
                       (BlasInt, Ptr{Void}, Ptr{Void}, Ptr{Void}),
                       solv, cf.pt.val[1], B.pt[1], B.cm.pt[1])
    CholmodSparseOut{Tv,Ti}(cso, m, n, cf.cs.cm)
end

function CholmodSparseOut{Tv,Ti}(cf::CholmodFactor{Tv,Ti})
    n = size(cf.cs)[1]
    cso = CholmodPtr{Tv,Ti}(Array(Ptr{Void},1))
    cso.val[1] = ccall((:cholmod_factor_to_sparse, :libcholmod), Ptr{Void},
                       (Ptr{Void}, Ptr{Void}), cf.pt.val[1], cf.cs.cm.pt[1])
    CholmodSparseOut{Tv,Ti}(cso, n, n, cf.cs.cm)
end

function SparseMatrixCSC{Tv,Ti}(cso::CholmodSparseOut{Tv,Ti})
    nz = nnz(cso)
    sp = SparseMatrixCSC{Tv,Ti}(cso.m, cso.n, Array(Ti, cso.n + 1), Array(Ti, nz), Array(Tv, nz))
    st = ccall((:jl_cholmod_sparse_copy_out, :libsuitesparse_wrapper), BlasInt,
                (Ptr{Void}, Ptr{Ti}, Ptr{Ti}, Ptr{Tv}),
                cso.pt.val[1], sp.colptr, sp.rowval, sp.nzval)
    if st == 1 error("CholmodSparseOut object is not packed") end
    if st == 2 error("CholmodSparseOut object is not sorted") end # Should not occur
    if st == 3 error("CholmodSparseOut object has INTLONG itype") end
    convert_to_1_based_indexing!(sp)
end

function show(io::IO, cso::CholmodSparseOut)
    sp = ccall(:cholmod_print_sp, BlasInt, (Ptr{Void}, Ptr{Uint8},Ptr{Void}), cso.pt.val[1], "", cso.cm.pt[1])
    if sp != CHOLMOD_TRUE error("Cholmod error in print_sparse") end
end

function chm_aat{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, symm::Bool)
    cs = CholmodSparse(A, 0)
    aa = CholmodPtr{Tv,Ti}(Array(Ptr{Void},1))
    aa.val[1] = ccall(:cholmod_aat, Ptr{Void}, (Ptr{Void},Ptr{BlasInt},BlasInt,BlasInt,Ptr{Void}),
                      cs.pt.val[1], C_NULL, 0, 1, cs.cm.pt[1])
    if ccall(:cholmod_sort, BlasInt, (Ptr{Void}, Ptr{Void}), aa.val[1], cs.cm.pt[1]) != CHOLMOD_TRUE
        error("Cholmod error in sort")
    end
    if symm
        pt = ccall(:cholmod_copy, Ptr{Void}, (Ptr{Void}, BlasInt, BlasInt, Ptr{Void}),
                   aa.val[1], 1, 1, cs.cm.pt[1])
        if ccall((:cholmod_free_sparse, :libcholmod), BlasInt,
                 (Ptr{Void}, Ptr{Void}), aa.val, cs.cm.pt[1]) != CHOLMOD_TRUE
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

    ccall((:jl_cholmod_sparse, :libsuitesparse_wrapper),
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

    ccall((:jl_cholmod_dense, :libsuitesparse_wrapper), Ptr{Void},
          (Ptr{Void}, BlasInt, BlasInt, BlasInt, BlasInt, Ptr{T}, Ptr{Void}, BlasInt, Int),
          cd, size(B,1), size(B,2), length(B), stride(B,2), B, C_NULL, xtype, dtype
          )

    return cd
end

function cholmod_dense_copy_out{T}(x::Ptr{Void}, sol::VecOrMat{T})
    ccall((:jl_cholmod_dense_copy_out, :libsuitesparse_wrapper),
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

function cholmod_analyze{Tv<:CHMVTypes, Ti<:CHMITypes}(cs::Array{Ptr{Void},1}, cm::Array{Ptr{Void},1})
    ccall(:cholmod_analyze, Ptr{Void}, (Ptr{Void}, Ptr{Void}), cs[1], cm[1])
end

function cholmod_factorize{Tv<:CHMVTypes, Ti<:CHMITypes}(cs::Array{Ptr{Void},1}, cs_factor::Ptr{Void}, cm::Array{Ptr{Void},1})
    st = ccall(:cholmod_factorize, BlasInt, (Ptr{Void}, Ptr{Void}, Ptr{Void}), cs[1], cs_factor, cm[1])
    if st != CHOLMOD_TRUE error("CHOLMOD could not factorize the matrix") end
end

function cholmod_solve(cs_factor::Ptr{Void}, cd_rhs::Array{Ptr{Void},1}, cm::Array{Ptr{Void},1})
    ccall(:cholmod_solve, Ptr{Void}, (BlasInt, Ptr{Void}, Ptr{Void}, Ptr{Void}),
          CHOLMOD_A, cs_factor, cd_rhs[1], cm[1])
end

end #module
