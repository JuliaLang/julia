module SuiteSparse

import Base.SparseMatrixCSC, Base.size, Base.nnz, Base.eltype, Base.show
import Base.triu, Base.norm, Base.solve, Base.lu, Base.lu!
#, Base.(\)
import Base.Ac_ldiv_B, Base.At_ldiv_B, Base.convert

import Base.BlasInt
import Base.blas_int

export                                  # types
CholmodSparse,
#    CholmodFactor,
CholmodDense,
#    CholmodSparseOut,
UmfpackLU,                          # call these lu and lu! instead?
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
type CholmodException <: Exception end

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

function lu{Tv<:UMFVTypes,Ti<:CHMITypes}(S::SparseMatrixCSC{Tv,Ti})
    zerobased = S.colptr[1] == 0
    lu = UmfpackLU(C_NULL, C_NULL, S.m, S.n,
                   zerobased ? copy(S.colptr) : decrement(S.colptr),
                   zerobased ? copy(S.rowval) : decrement(S.rowval),
                   copy(S.nzval))
    umfpack_numeric!(lu)
end

function lu!{Tv<:UMFVTypes,Ti<:CHMITypes}(S::SparseMatrixCSC{Tv,Ti})
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

### Solve with Factorization

#(\){T<:UMFVTypes}(fact::UmfpackLU{T}, b::Vector{T}) = umfpack_solve(fact, b)
#(\){Ts<:UMFVTypes,Tb<:Number}(fact::UmfpackLU{Ts}, b::Vector{Tb}) = fact\convert(Vector{Ts},b)
  
### Solve directly with matrix

#(\)(S::SparseMatrixCSC, b::Vector) = lu(S) \ b
At_ldiv_B{T<:UMFVTypes}(S::SparseMatrixCSC{T}, b::Vector{T}) = umfpack_solve(lu(S), b, UMFPACK_Aat)
function At_ldiv_B{Ts<:UMFVTypes,Tb<:Number}(S::SparseMatrixCSC{Ts}, b::Vector{Tb})
    ## should be more careful here in case Ts<:Real and Tb<:Complex
    At_ldiv_B(S, convert(Vector{Ts}, b))
end
Ac_ldiv_B{T<:UMFVTypes}(S::SparseMatrixCSC{T}, b::Vector{T}) = umfpack_solve(lu(S), b, UMFPACK_At)
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

### These offsets should be reconfigured to be less error-prone in matches
const chm_com_offsets = Array(Int, length(ChmCommon.types))
ccall((:jl_cholmod_common_offsets, :libsuitesparse_wrapper),
      Void, (Ptr{Uint8},), chm_com_offsets)
                                               
### there must be an easier way but at least this works.
function ChmCommon(aa::Array{Uint8,1})
    typs = ChmCommon.types
    sz = map(sizeof, typs)
    args = map(i->reinterpret(typs[i], aa[chm_com_offsets[i] + (1:sz[i])])[1], 1:length(sz))
    eval(Expr(:call, unshift!(args, :ChmCommon), Any))
end

function chm_itype{Tv,Ti<:CHMITypes}(S::SparseMatrixCSC{Tv,Ti})
    int32(Ti == Int32 ? CHOLMOD_INT : CHOLMOD_LONG)
end
function chm_xtype{T<:CHMVTypes}(S::SparseMatrixCSC{T})
    int32(T<:Complex ? CHOLMOD_COMPLEX : CHOLMOD_REAL)
end
function chm_dtype{T<:CHMVTypes}(S::SparseMatrixCSC{T})
    int32(T<:Union(Float32, Complex64) ? CHOLMOD_SINGLE : CHOLMOD_DOUBLE)
end

function set_chm_prt_lev(cm::Array{Uint8}, lev::Integer)
    cm[(1:4) + chm_com_offsets[13]] = reinterpret(Uint8, [int32(lev)])
end

## cholmod_dense pointers passed to or returned from C functions are of Julia type
## Ptr{c_CholmodDense}.  The CholmodDense type contains a c_CholmodDense object and other
## fields then ensure the memory pointed to is freed when it should be and not before.
type c_CholmodDense{T<:CHMVTypes}
    m::Int
    n::Int
    nzmax::Int
    lda::Int
    xpt::Ptr{T}
    zpt::Ptr{Void}
    xtype::Int32
    dtype::Int32
end

type CholmodDense{T<:CHMVTypes}
    c::c_CholmodDense
    m::Matrix{T} # Array(T,(0,0)) when created from Ptr{c_CholmodDense{T}}
    ptr::Ptr{c_CholmodDense{T}} # null pointer when created from Julia array
end

convert(::Type{c_CholmodDense}, d::CholmodDense) = d.c

function CholmodDense{T<:CHMVTypes}(aa::VecOrMat{T})
    m = size(aa,1); n = size(aa,2)
    CholmodDense{T}(c_CholmodDense{T}(m, n, m*n, stride(aa,2), convert(Ptr{T}, aa), C_NULL,
                                      int32(T<:Complex ? CHOLMOD_COMPLEX : CHOLMOD_REAL),
                                      int32(T<:Union(Float32,Complex64) ? CHOLMOD_SINGLE : CHOLMOD_DOUBLE)),
                    aa, convert(Ptr{c_CholmodDense{T}}, C_NULL))
end

function CholmodDense{T<:CHMVTypes}(c::Ptr{c_CholmodDense{T}})
    cp = unsafe_ref(c)
    if cp.lda != cp.m || cp.nzmax != cp.m * cp.n
        error("overallocated cholmod_sparse returned object of size $(cp.m) by $(cp.n) with leading dim $(cp.lda) and nzmax $(cp.nzmax)")
    end
    CholmodDense{T}(cp, Array(T, (0,0)), c)
end

function convert{T<:CHMVTypes}(::Type{Array}, chmpt::Ptr{c_CholmodDense{T}}, own::Bool)
    cp = unsafe_ref(chmpt)
    if cp.lda != cp.m || cp.nzmax != cp.m * cp.n
        error("overallocated cholmod_sparse returned object of size $(cp.m) by $(cp.n) with leading dim $(cp.lda) and nzmax $(cp.nzmax)")
    end
    arr = pointer_to_array(cp.xpt, (cp.m, cp.n), own)
    if !own return copy(arr) end
    c_free(chmpt)
end
convert{T<:CHMVTypes}(::Type{Array}, chmpt::Ptr{c_CholmodDense{T}}) = convert(Array, chmpt, false)
    
function chm_chk_dn{T<:CHMVTypes}(cd::CholmodDense{T})
    ccall((:cholmod_check_dense, :libcholmod), Int,
          (Ptr{CholmodDense{T}}, Ptr{Uint8}), &cd, chm_com)
end

function chm_ones(m::Integer, n::Integer, t::Float64)
    ccall((:cholmod_ones, :libcholmod), Ptr{c_CholmodDense{Float64}},
                  (Int, Int, Int32, Ptr{Uint8}),
                  m, n, CHOLMOD_REAL, chm_com)
end
function chm_ones(m::Integer, n::Integer, t::Complex128)
    ccall((:cholmod_ones, :libcholmod), Ptr{c_CholmodDense{Complex128}},
          (Int, Int, Int32, Ptr{Uint8}),
          m, n, CHOLMOD_COMPLEX, chm_com)
end
chm_ones(m::Integer, n::Integer) = chm_ones(m, n, 1.)

function chm_free_dn{T<:UMFVTypes}(A::Ptr{c_CholmodDense{T}})
    aa = [A]
    status = ccall((:cholmod_free_dense, :libcholmod), Int32,
                   (Ptr{Ptr{c_CholmodDense{T}}}, Ptr{Uint8}), aa, chm_com)
    if status != CHOLMOD_TRUE throw(CholmodException) end
end

## This should be converted to a show method    
function chm_prt_dn{T<:CHMVTypes}(cd::CholmodDense{T}, lev::Integer, nm::ASCIIString)
    inds = (1:4) + chm_com_offsets[13]
    orig = chm_com[inds]
    chm_com[inds] = reinterpret(Uint8, [int32(lev)])
    status = ccall((:cholmod_print_dense, :libcholmod), Int32,
                   (Ptr{CholmodDense{T}}, Ptr{Uint8}, Ptr{Uint8}),
                   &cd, nm, chm_com)
    chm_com[inds] = orig
end

chm_prt_dn{T<:CHMVTypes}(cd::CholmodDense{T}, lev::Integer) = chm_prt_dn(cd, lev, "")    
chm_prt_dn{T<:CHMVTypes}(cd::CholmodDense{T}) = chm_prt_dn(cd, int32(4), "")

type CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}
    m::Int
    n::Int
    nzmax::Int
    ppt::Ptr{Ti}
    ipt::Ptr{Ti}
    nzpt::Ptr{Void}
    xpt::Ptr{Tv}
    zpt::Ptr{Void}
    stype::Int32
    itype::Int32
    xtype::Int32
    dtype::Int32
    sorted::Int32
    packed::Int32
    colptr0::Vector{Ti} # 0-based column pointers
    rowval0::Vector{Ti} # 0-based row indices
    nzval::Vector{Tv}   # a copy of the non-zero values
    chm_free::Bool      # was storage allocated by Cholmod?
    structpt::Ptr{Void} # pointer to the C struct (when created from Cholmod results)
end

function CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}(A::SparseMatrixCSC{Tv,Ti})
    colptr0 = decrement(A.colptr)
    rowval0 = decrement(A.rowval)
    nzval = copy(A.nzval)
    CholmodSparse{Tv,Ti}(size(A,1),size(A,2),int(colptr0[end]),
                         convert(Ptr{Ti}, colptr0), convert(Ptr{Ti}, rowval0),
                         C_NULL, convert(Ptr{Tv}, nzval), C_NULL, int32(0),
                         chm_itype(A), chm_xtype(A), chm_dtype(A),
                         CHOLMOD_TRUE, CHOLMOD_TRUE,
                         colptr0, rowval0, nzval, false, C_NULL)
end

end #module
