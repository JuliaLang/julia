module SuiteSparse

export ChmCommon,
       CholmodDense,                    # types
       CholmodFactor,
       CholmodSparse,
       CholmodTriplet,
       UmfpackLU,
                                        # methods
       chm_aat,
       chm_analyze,
       chm_check,
       chm_chng_fac!,
       chm_eye,
       chm_fac_xtype!,
       chm_factorize,
       chm_factorize!,
       chm_norm,
       chm_ones,
       chm_pack_fac!,
       chm_print,
       chm_scale!,
       chm_sdmult,
       chm_solve,
       chm_sort,
       chm_speye,
       chm_spsolve,
       chm_sp_to_tr,
       chm_zeros,
       decrement,
       decrement!,
       increment,
       increment!,
       indtype,
       show_umf_ctrl,
       show_umf_info,
       umf_extract,
       umf_lunz

import Base.(\)
import Base.Ac_ldiv_B
import Base.At_ldiv_B
import Base.SparseMatrixCSC
import Base.copy

import Base.nnz
import Base.findn_nzs
import Base.show
import Base.size
import Base.convert

import LinAlg.Factorization
import LinAlg.chol
import LinAlg.det             
import LinAlg.diag
import LinAlg.diagmm
import LinAlg.logdet
import LinAlg.lu
import LinAlg.solve

include("linalg/suitesparse_h.jl")

type MatrixIllConditionedException <: Exception end
type CholmodException <: Exception end

function decrement!{T<:Integer}(A::AbstractArray{T})
    for i in 1:length(A) A[i] -= one(T) end
    A
end
decrement{T<:Integer}(A::AbstractArray{T}) = decrement!(copy(A))
function increment!{T<:Integer}(A::AbstractArray{T})
    for i in 1:length(A) A[i] += one(T) end
    A
end
increment{T<:Integer}(A::AbstractArray{T}) = increment!(copy(A))

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
    res = UmfpackLU(C_NULL, C_NULL, S.m, S.n,
                    zerobased ? copy(S.colptr) : decrement(S.colptr),
                    zerobased ? copy(S.rowval) : decrement(S.rowval),
                    copy(S.nzval))
    finalizer(res, umfpack_free_symbolic)
    umfpack_numeric!(res)
end

function lu!{Tv<:UMFVTypes,Ti<:CHMITypes}(S::SparseMatrixCSC{Tv,Ti})
    zerobased = S.colptr[1] == 0
    res = UmfpackLU(C_NULL, C_NULL, S.m, S.n,
                    zerobased ? S.colptr : decrement!(S.colptr),
                    zerobased ? S.rowval : decrement!(S.rowval),
                    S.nzval)
    finalizer(res, umfpack_free_symbolic)
    umfpack_numeric!(res)
end

function show(io::IO, f::UmfpackLU)
    @printf(io, "UMFPACK LU Factorization of a %d-by-%d sparse matrix\n",
            f.m, f.n)
    if f.numeric != C_NULL println(f.numeric) end
end

### Solve with Factorization

(\){T<:UMFVTypes}(fact::UmfpackLU{T}, b::Vector{T}) = umfpack_solve(fact, b)
(\){Ts<:UMFVTypes,Tb<:Number}(fact::UmfpackLU{Ts}, b::Vector{Tb}) = fact\convert(Vector{Ts},b)

### Solve directly with matrix

(\)(S::SparseMatrixCSC, b::Vector) = lu(S) \ b
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
            U
        end
    end
end

for (f_sol_r, f_sol_c, itype) in
    (("umfpack_di_solve","umfpack_zi_solve",:Int32),
     ("umfpack_dl_solve","umfpack_zl_solve",:Int64))
    @eval begin
        function umfpack_solve{Tv<:Float64,Ti<:$itype}(lu::UmfpackLU{Tv,Ti},
                                                       b::Vector{Tv}, typ::Integer)
            umfpack_numeric!(lu)
            x = similar(b)
            status = ccall(($f_sol_r, :libumfpack), Ti,
                           (Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Float64},
                            Ptr{Float64}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                           typ, lu.colptr, lu.rowval, lu.nzval, x, b, lu.numeric, umf_ctrl, umf_info)
            if status != UMFPACK_OK; error("Error code $status in umfpack_solve"); end
            return x
        end

        function umfpack_solve{Tv<:Complex128,Ti<:$itype}(lu::UmfpackLU{Tv,Ti},
                                                          b::Vector{Tv}, typ::Integer)
            umfpack_numeric!(lu)
            xr = similar(b, Float64)
            xi = similar(b, Float64)
            status = ccall(($f_sol_c, :libumfpack),
                           Ti,
                           (Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Float64},
                            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
                            Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                           typ, lu.colptr, lu.rowval, real(lu.nzval), imag(lu.nzval),
                           xr, xi, real(b), imag(b),
                           lu.num, umf_ctrl, umf_info)
            if status != UMFPACK_OK; error("Error code $status from umfpack_solve"); end
            return complex(xr,xi)
        end
    end
end
show_umf_ctrl() = show_umf_ctrl(2.)

umfpack_solve(lu::UmfpackLU, b::Vector) = umfpack_solve(lu, b, UMFPACK_A)

for (det_r,det_z,itype) in
    (("umfpack_di_get_determinant","umfpack_zi_get_determinant",:Int32),
     ("umfpack_dl_get_determinant","umfpack_zl_get_determinant",:Int64))
    @eval begin
        function det{Tv<:Float64,Ti<:$itype}(lu::UmfpackLU{Tv,Ti})
            mx = Array(Tv,1)
            status = ccall(($det_r,:libumfpack), Ti,
                           (Ptr{Tv},Ptr{Tv},Ptr{Void},Ptr{Float64}),
                           mx, C_NULL, lu.numeric, umf_info)
            if status != UMFPACK_OK error("Error code $status from umfpack_get_determinant") end
            mx[1]
        end
        function det{Tv<:Complex128,Ti<:$itype}(lu::UmfpackLU{Tv,Ti})
            mx = Array(Float64,1)
            mz = Array(Float64,1)
            status = ccall(($det_z,:libumfpack), Ti,
                           (Ptr{Float64},Ptr{Float64},Ptr{Float64},Ptr{Void},Ptr{Float64}),
                           mx, mz, C_NULL, lu.numeric, umf_info)
            if status != UMFPACK_OK error("Error code $status from umfpack_get_determinant") end
            complex(mx[1], mz[1])
        end
    end
end

for (lunz,get_numeric_r,get_numeric_z,itype) in
    (("umfpack_di_get_lunz","umfpack_di_get_numeric","umfpack_zi_get_numeric",:Int32),
     ("umfpack_dl_get_lunz","umfpack_dl_get_numeric","umfpack_zl_get_numeric",:Int64))
    @eval begin
        function umf_lunz{Tv<:UMFVTypes,Ti<:$itype}(lu::UmfpackLU{Tv,Ti})
            lnz = Array(Ti, 1)
            unz = Array(Ti, 1)
            n_row = Array(Ti, 1)
            n_col = Array(Ti, 1)
            nz_diag = Array(Ti, 1)
            status = ccall(($lunz,:libumfpack), Ti,
                           (Ptr{Ti},Ptr{Ti},Ptr{Ti},Ptr{Ti},Ptr{Ti},Ptr{Void}),
                           lnz, unz, n_row, n_col, nz_diag, lu.numeric)
            if status != UMFPACK_OK error("Error code $status from umfpack_get_lunz") end
            (lnz[1], unz[1], n_row[1], n_col[1], nz_diag[1])
        end
        function umf_extract{Tv<:Float64,Ti<:$itype}(lu::UmfpackLU{Tv,Ti})
            umfpack_numeric!(lu)        # ensure the numeric decomposition exists
            (lnz,unz,n_row,n_col,nz_diag) = umf_lunz(lu)
            Lp = Array(Ti, n_col + 1)
            Lj = Array(Ti, lnz) # L is returned in CSR (compressed sparse row) format
            Lx = Array(Tv, lnz)
            Up = Array(Ti, n_col + 1)
            Ui = Array(Ti, unz)
            Ux = Array(Tv, unz)
            P  = Array(Ti, n_row)
            Q  = Array(Ti, n_col)
            Rs = Array(Tv, n_row)
            status = ccall(($get_numeric_r,:libumfpack), Ti,
                           (Ptr{Ti},Ptr{Ti},Ptr{Tv},
                            Ptr{Ti},Ptr{Ti},Ptr{Tv},
                            Ptr{Ti},Ptr{Ti},Ptr{Void},
                            Ptr{Ti},Ptr{Tv},Ptr{Void}),
                           Lp,Lj,Lx,
                           Up,Ui,Ux,
                           P, Q, C_NULL,
                           &0, Rs, lu.numeric)
            if status != UMFPACK_OK error("Error code $status from numeric") end
            (transpose(SparseMatrixCSC(n_row,n_row,increment!(Lp),increment!(Lj),Lx)),
             SparseMatrixCSC(n_row,n_col,increment!(Up),increment!(Ui),Ux),
             increment!(P), increment!(Q), Rs)
        end
    end
end

## The C functions called by these Julia functions do not depend on
## the numeric and index types, even though the umfpack names indicate
## they do.  The umfpack_free_* functions can be called on C_NULL without harm.
function umfpack_free_symbolic(symb::Ptr{Void})
    tmp = [symb]
    ccall((:umfpack_dl_free_symbolic, :libumfpack), Void, (Ptr{Void},), tmp)
end
show_umf_info() = show_umf_info(2.)

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

function umfpack_free_numeric(lu::UmfpackLU)
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

const chm_com_sz = ccall((:jl_cholmod_common_size,:libsuitesparse_wrapper),Int,())
const chm_com    = ones(Uint8, chm_com_sz)

### A way of examining some of the fields in chm_com
### Probably better to make this a Dict{ASCIIString,Tuple} and
### save the offsets and the lengths and the types.  Then the names can be checked.
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
const chm_prt_inds = (1:4) + chm_com_offsets[13]
const chm_ityp_inds = (1:4) + chm_com_offsets[18]

### there must be an easier way but at least this works.
function ChmCommon(aa::Array{Uint8,1})
    typs = ChmCommon.types
    sz = map(sizeof, typs)
    args = map(i->reinterpret(typs[i], aa[chm_com_offsets[i] + (1:sz[i])])[1], 1:length(sz))
    eval(Expr(:call, unshift!(args, :ChmCommon), Any))
end
function chm_itype{Tv<:CHMVTypes,Ti<:CHMITypes}(S::SparseMatrixCSC{Tv,Ti})
    int32(Ti<:Int64 ? CHOLMOD_LONG : CHOLMOD_INT)
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
immutable c_CholmodDense{T<:CHMVTypes}
    m::Int
    n::Int
    nzmax::Int
    lda::Int
    xpt::Ptr{T}
    zpt::Ptr{Void}
    xtype::Int32
    dtype::Int32
end

immutable CholmodDense{T<:CHMVTypes}
    c::c_CholmodDense
    mat::Matrix{T}
end

immutable c_CholmodFactor{Tv<:CHMVTypes,Ti<:CHMITypes}
    n::Int
    minor::Int
    Perm::Ptr{Ti}
    ColCount::Ptr{Ti}
    nzmax::Int
    p::Ptr{Ti}
    i::Ptr{Ti}
    x::Ptr{Tv}
    z::Ptr{Void}
    nz::Ptr{Ti}
    next::Ptr{Ti}
    prev::Ptr{Ti}
    nsuper::Int
    ssize::Int
    xsize::Int
    maxcsize::Int
    maxesize::Int
    super::Ptr{Ti}
    pi::Ptr{Ti}
    px::Ptr{Tv}
    s::Ptr{Ti}
    ordering::Int32
    is_ll::Int32
    is_super::Int32
    is_monotonic::Int32
    itype::Int32
    xtype::Int32
    dtype::Int32
end

immutable CholmodFactor{Tv<:CHMVTypes,Ti<:CHMITypes}
    c::c_CholmodFactor{Tv,Ti}
    Perm::Vector{Ti}
    ColCount::Vector{Ti}
    p::Vector{Ti}
    i::Vector{Ti}
    x::Vector{Tv}
    nz::Vector{Ti}
    next::Vector{Ti}
    prev::Vector{Ti}
    super::Vector{Ti}
    pi::Vector{Ti}
    px::Vector{Tv}
    s::Vector{Ti}
end

immutable c_CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}
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
end

immutable CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}
    c::c_CholmodSparse{Tv,Ti}
    colptr0::Vector{Ti}
    rowval0::Vector{Ti}
    nzval::Vector{Tv}
end

immutable c_CholmodTriplet{Tv<:CHMVTypes,Ti<:CHMITypes}
    m::Int
    n::Int
    nzmax::Int
    nnz::Int
    i::Ptr{Ti}
    j::Ptr{Ti}
    x::Ptr{Tv}
    z::Ptr{Void}
    stype:Int32
    itype::Int32
    xtype::Int32
    dtype::Int32
end

immutable CholmodTriplet{Tv<:CHMVTypes,Ti<:CHMITypes}
    c::c_CholmodTriplet{Tv,Ti}
    i::Vector{Ti}
    j::Vector{Ti}
    x::Vector{Tv}
end

function CholmodDense{T<:CHMVTypes}(aa::VecOrMat{T})
    m = size(aa,1); n = size(aa,2)
    CholmodDense(c_CholmodDense{T}(m, n, m*n, stride(aa,2),
                                   convert(Ptr{T}, aa), C_NULL,
                                   T<:Complex ? CHOLMOD_COMPLEX : CHOLMOD_REAL,
                                   T<:Union(Float32,Complex64) ? CHOLMOD_SINGLE : CHOLMOD_DOUBLE),
                 length(size(aa)) == 2 ? aa : reshape(aa, (m,n)))
end

function CholmodDense{T<:CHMVTypes}(c::Ptr{c_CholmodDense{T}})
    cp = unsafe_ref(c)
    if cp.lda != cp.m || cp.nzmax != cp.m * cp.n
        error("overallocated cholmod_sparse returned object of size $(cp.m) by $(cp.n) with leading dim $(cp.lda) and nzmax $(cp.nzmax)")
    end
    ## the true in the call to pointer_to_array means Julia will free the memory
    val = CholmodDense(cp, pointer_to_array(cp.xpt, (cp.m,cp.n), true))
    c_free(c)
    val
end
show(io::IO, cd::CholmodDense) = show(io, cd.mat)

function chm_check{T<:CHMVTypes}(cd::CholmodDense{T})
    status = ccall((:cholmod_check_dense, :libcholmod), Int32,
                   (Ptr{c_CholmodDense{T}}, Ptr{Uint8}), &cd.c, chm_com)
    if status != CHOLMOD_TRUE throw(CholmodException) end
end

function chm_ones{T<:Union(Float64,Complex128)}(m::Integer, n::Integer, t::T)
    CholmodDense(ccall((:cholmod_ones, :libcholmod), Ptr{c_CholmodDense{T}},
                       (Int, Int, Int32, Ptr{Uint8}),
                       m, n,
                       T<:Complex ? CHOLMOD_COMPLEX : CHOLMOD_REAL,
                       chm_com))
end
chm_ones(m::Integer, n::Integer) = chm_ones(m, n, 1.)

function chm_zeros{T<:Union(Float64,Complex128)}(m::Integer, n::Integer, t::T)
    CholmodDense(ccall((:cholmod_zeros, :libcholmod), Ptr{c_CholmodDense{T}},
                       (Int, Int, Int32, Ptr{Uint8}),
                       m, n,
                       T<:Complex ? CHOLMOD_COMPLEX : CHOLMOD_REAL,
                       chm_com))
end
chm_zeros(m::Integer, n::Integer) = chm_zeros(m, n, 1.)

function chm_eye{T<:Union(Float64,Complex128)}(m::Integer, n::Integer, t::T)
    CholmodDense(ccall((:cholmod_eye, :libcholmod), Ptr{c_CholmodDense{T}},
                       (Int, Int, Int32, Ptr{Uint8}),
                       m, n,
                       T<:Complex ? CHOLMOD_COMPLEX : CHOLMOD_REAL,
                       chm_com))
end
chm_eye(m::Integer, n::Integer) = chm_eye(m, n, 1.)
chm_eye(n::Integer) = chm_eye(n, n, 1.)


function chm_print{T<:CHMVTypes}(cd::CholmodDense{T}, lev::Integer, nm::ASCIIString)
    orig = chm_com[chm_prt_inds]
    chm_com[chm_prt_inds] = reinterpret(Uint8, [int32(lev)])
    status = ccall((:cholmod_print_dense, :libcholmod), Int32,
                   (Ptr{c_CholmodDense{T}}, Ptr{Uint8}, Ptr{Uint8}),
                   &cd.c, nm, chm_com)
    chm_com[chm_prt_inds] = orig
    if status != CHOLMOD_TRUE throw(CholmodException) end
end
chm_print(cd::CholmodDense, lev::Integer) = chm_print(cd, lev, "")
chm_print(cd::CholmodDense) = chm_print(cd, int32(4), "")

function CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}(A::SparseMatrixCSC{Tv,Ti}, stype::Integer)
    zerobased = A.colptr[1] == 0
    colptr0 = zerobased ? copy(A.colptr) : decrement(A.colptr)
    rowval0 = zerobased ? copy(A.rowptr) : decrement(A.rowval)
    nzval = copy(A.nzval)
    CholmodSparse{Tv,Ti}(c_CholmodSparse{Tv,Ti}(size(A,1),size(A,2),
                                                int(colptr0[end]),
                                                convert(Ptr{Ti}, colptr0),
                                                convert(Ptr{Ti}, rowval0), C_NULL,
                                                convert(Ptr{Tv}, nzval), C_NULL,
                                                int32(stype), chm_itype(A),
                                                chm_xtype(A), chm_dtype(A),
### Assuming that a SparseMatrixCSC always has sorted row indices. Need to check.
                                                CHOLMOD_TRUE, CHOLMOD_TRUE),
                         colptr0, rowval0, nzval)
end
function CholmodSparse(A::SparseMatrixCSC)
    stype = ishermitian(A) ? 1 : 0
    CholmodSparse(stype > 0 ? triu(A) : A, stype)
end

function cmn{Ti<:CHMITypes}(i::Ti) # turns out this is as fast as checking for initialization
    if Ti <: Int64
        ccall((:cholmod_l_start, :libcholmod), Int32, (Ptr{Uint8},), chm_com)
    else
        ccall((:cholmod_start, :libcholmod), Int32, (Ptr{Uint8},), chm_com)
    end
    chm_com
end
cmn{Tv,Ti<:CHMITypes}(A::CholmodSparse{Tv,Ti}) = cmn(one(Ti))
cmn{Tv,Ti<:CHMITypes}(a::c_CholmodSparse{Tv,Ti}) = cmn(one(Ti))
cmn{Tv,Ti<:CHMITypes}(ap::Ptr{c_CholmodSparse{Tv,Ti}}) = cmn(one(Ti))
cmn{Tv,Ti<:CHMITypes}(L::CholmodFactor{Tv,Ti}) = cmn(one(Ti))
cmn{Tv,Ti<:CHMITypes}(l::c_CholmodFactor{Tv,Ti}) = cmn(one(Ti))
cmn{Tv,Ti<:CHMITypes}(lp::Ptr{c_CholmodFactor{Tv,Ti}}) = cmn(one(Ti))
    
function chm_rdsp(fnm::String)
    fd = ccall(:fopen, Ptr{Void}, (Ptr{Uint8},Ptr{Uint8}), fnm, "r")
    res = ccall((:cholmod_read_sparse,:libcholmod), Ptr{c_CholmodSparse{Float64,Int32}},
                (Ptr{Void},Ptr{Uint8}),fd,cmn(one(Int32)))
    ccall(:fclose, Cint, (Ptr{Void},), fd)
    CholmodSparse(res)
end

function CholmodSparse{Tv<:CHMVTypes,Ti<:CHMITypes}(cp::Ptr{c_CholmodSparse{Tv,Ti}})
    csp = unsafe_ref(cp)
    colptr0 = pointer_to_array(csp.ppt, (csp.n + 1,), true)
    nnz = int(colptr0[end])
    cms = CholmodSparse{Tv,Ti}(csp, colptr0,
                               pointer_to_array(csp.ipt, (nnz,), true),
                               pointer_to_array(csp.xpt, (nnz,), true))
    c_free(cp)
    cms
end

for (chk,prt,srt,itype) in
    (("cholmod_check_sparse","cholmod_print_sparse","cholmod_sort",:Int32),
     ("cholmod_l_check_sparse","cholmod_l_print_sparse","cholmod_l_sort",:Int64))
    @eval begin
        function chm_check{Tv<:CHMVTypes}(cs::CholmodSparse{Tv,$itype})
            cmn(cs)
            status = ccall(($chk,:libcholmod), Int32,
                           (Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Uint8}),
                           &cs.c, chm_com)
            if status != CHOLMOD_TRUE throw(CholmodException) end
        end
        function chm_print{Tv<:CHMVTypes}(cs::CholmodSparse{Tv,$itype},lev,nm)
            cmn(cs)                     # initialize if necessary
            orig = chm_com[chm_prt_inds]
            chm_com[chm_prt_inds] = reinterpret(Uint8, [int32(lev)])
            status = ccall(($prt,:libcholmod), Int32,
                           (Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Uint8}, Ptr{Uint8}),
                           &cs.c, nm, chm_com)
            chm_com[chm_prt_inds] = orig
            if status != CHOLMOD_TRUE throw(CholmodException) end
        end
        function chm_sort{Tv<:CHMVTypes}(cs::CholmodSparse{Tv,$itype})
            status = ccall(($srt,:libcholmod), Int32,
                           (Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Uint8}),
                           &cs.c, cmn(cs))
            if status != CHOLMOD_TRUE throw(CholmodException) end
            cs
        end
    end
end

chm_print(cd::CholmodSparse, lev::Integer) = chm_print(cd, lev, "")
chm_print(cd::CholmodSparse) = chm_print(cd, int32(4), "")

nnz{Tv<:CHMVTypes,Ti<:CHMITypes}(cp::CholmodSparse{Tv,Ti}) = int(cp.colptr0[end])
size{Tv<:CHMVTypes,Ti<:CHMITypes}(cp::CholmodSparse{Tv,Ti}) = (int(cp.c.m), int(cp.c.n))
function size{Tv<:CHMVTypes,Ti<:CHMITypes}(cp::CholmodSparse{Tv,Ti}, d::Integer)
    d == 1 ? cp.c.m : (d == 2 ? cp.c.n : 1)
end

for (aat,allocsp,cop,copsp,freesp,normsp,sdmult,speye,transsym,itype) in
    (("cholmod_aat","cholmod_allocate_sparse","cholmod_copy","cholmod_copy_sparse",
      "cholmod_free_sparse","cholmod_norm_sparse","cholmod_sdmult","cholmod_speye",
      "cholmod_transpose_sym",:Int32),
     ("cholmod_l_aat","cholmod_l_allocate_sparse","cholmod_l_copy",
      "cholmod_l_copy_sparse","cholmod_l_free_sparse","cholmod_norm_sparse",
      "cholmod_l_sdmult","cholmod_l_speye","cholmod_l_transpose_sym",:Int64))
    @eval begin
        function chm_aat{Tv<:CHMVTypes}(a::c_CholmodSparse{Tv,$itype})
            cm = cmn(a)
            ## strangely the matrix returned by $aat is not marked as symmetric
            ## all of the code past the call to $aat is to create the symmetric-storage
            ## version of the result then transpose it to provide sorted columns
            aa = Array(Ptr{c_CholmodSparse{Tv,$itype}}, 2)
            aa[1] = ccall(($aat, :libcholmod), Ptr{c_CholmodSparse{Tv,$itype}},
                          (Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Void}, Int, Int32, Ptr{Uint8}),
                          &a, C_NULL, 0, 1, cm)
            ## Create the lower triangle unsorted
            aa[2] = ccall(($cop, :libcholmod), Ptr{c_CholmodSparse{Tv,$itype}},
                          (Ptr{c_CholmodSparse{Tv,$itype}}, Int32, Int32, Ptr{Uint8}),
                          aa[1], -1, 1, cm)
            status = ccall(($freesp, :libcholmod), Int32,
                           (Ptr{Ptr{c_CholmodSparse{Tv,$itype}}}, Ptr{Uint8}), aa, cm)
            if status != CHOLMOD_TRUE throw(CholmodException) end
            aa[1] = aa[2]
            r = unsafe_ref(aa[1])
            ## Now transpose the lower triangle to the upper triangle to do the sorting
            rpt = ccall(($allocsp,:libcholmod),Ptr{c_CholmodSparse{Tv,$itype}},
                        (Csize_t,Csize_t,Csize_t,Cint,Cint,Cint,Cint,Ptr{Cuchar}),
                        r.m,r.n,r.nzmax,r.sorted,r.packed,-r.stype,r.xtype,cm)
            status = ccall(($transsym,:libcholmod),Int32,
                           (Ptr{c_CholmodSparse{Tv,$itype}}, Int32, Ptr{$itype},
                            Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Uint8}),
                           aa[1],1,C_NULL,rpt,cm)
            if status != CHOLMOD_TRUE throw(CholmodException) end
            status = ccall(($freesp, :libcholmod), Int32,
                           (Ptr{Ptr{c_CholmodSparse{Tv,$itype}}}, Ptr{Uint8}), aa, cm)
            if status != CHOLMOD_TRUE throw(CholmodException) end
            CholmodSparse(rpt)
        end
        function chm_copy_sp{Tv<:CHMVTypes}(a::c_CholmodSparse{Tv,$itype})
            ccall(($copsp,:libcholmod), Ptr{c_CholmodSparse{Tv,$itype}},
                  (Ptr{c_CholmodSparse{Tv,$itype}},Ptr{Uint8}), &a, cmn(a))
        end
        function chm_norm{Tv<:CHMVTypes}(a::c_CholmodSparse{Tv,$itype}, norm::Integer)
            ccall(($normsp, :libcholmod), Float64, 
                  (Ptr{c_CholmodSparse{Tv,$itype}}, Int32, Ptr{Uint8}),
                  &a,norm,cmn(a))
        end
        function chm_sdmult{Tv<:CHMVTypes}(a::c_CholmodSparse{Tv,$itype},
                                           trans::Bool,
                                           alpha::Tv,
                                           beta::Tv,
                                           x::c_CholmodDense{Tv})
            nc = trans ? a.m : a.n
            nr = trans ? a.n : a.m
            if nc != x.m
                error("Incompatible dimensions, $nc and $(x.m), in sdmult")
            end
            Y = CholmodDense(Array(Tv,nr,x.n))
            status = ccall(($sdmult,:libcholmod), Int32,
                           (Ptr{c_CholmodSparse{Tv,$itype}},Int32,Ptr{Tv},Ptr{Tv},
                            Ptr{c_CholmodDense{Tv}}, Ptr{c_CholmodDense{Tv}}, Ptr{Uint8}),
                           &a,trans,&alpha,&beta,&x,&Y.c,cmn(a))
            if status != CHOLMOD_TRUE throw(CholmodException) end
            Y
        end
        function chm_speye{Tv<:Union(Float64,Complex128)}(m::Integer, n::Integer, t::Tv, i::$itype)
            CholmodSparse(ccall(($speye, :libcholmod), Ptr{c_CholmodSparse{Tv,$itype}},
                                (Int, Int, Int32, Ptr{Uint8}),
                                m, n,
                                Tv<:Complex ? CHOLMOD_COMPLEX : CHOLMOD_REAL,
                                cmn(one($itype))))
        end
    end
end
chm_speye(m::Integer, n::Integer) = chm_speye(m, n, 1., 1)
chm_speye(n::Integer) = chm_speye(n, n, 1., 1)
chm_aat(A::CholmodSparse) = chm_aat(A.c)
chm_aat(A::SparseMatrixCSC) = chm_aat(CholmodSparse(A).c)
chm_norm(A::CholmodSparse,norm::Integer) = chm_norm(A.c,norm)
chm_norm(A::SparseMatrixCSC,norm::Integer) = chm_norm(CholmodSparse(A).c,norm)
chm_norm(A::CholmodSparse) = chm_norm(A.c,one(Int32))
chm_norm(A::SparseMatrixCSC) = chm_norm(CholmodSparse(A).c,one(Int32))
copy(A::CholmodSparse) = CholmodSparse(chm_copy_sp(A.c))

for (scl,itype) in
    (("cholmod_scale",:Int32),
     ("cholmod_l_scale",:Int64))
    @eval begin
        function chm_scale!{Tv<:CHMVTypes}(a::c_CholmodSparse{Tv,$itype},
                                           s::c_CholmodDense{Tv},
                                           typ::Integer)
            status = ccall(($scl,:libcholmod), Int32,
                           (Ptr{c_CholmodDense{Tv}},Int32,Ptr{c_CholmodSparse{Tv,$itype}},
                            Ptr{Uint8}), &s, typ, &a, cmn(a))
            if status != CHOLMOD_TRUE throw(CholmodException) end
        end
    end
end
function chm_scale!{T<:CHMVTypes}(A::CholmodSparse{T},S::CholmodDense{T},typ::Integer)
    chm_scale!(A.c,S.c,typ)
end
function diagmm{T<:CHMVTypes}(b::Vector{T}, A::CholmodSparse{T})
    Acp = copy(A)
    chm_scale!(Acp,CholmodDense(b),CHOLMOD_ROW)
    Acp
end
function diagmm{T<:CHMVTypes}(A::CholmodSparse{T},b::Vector{T})
    Acp = copy(A)
    chm_scale!(copy(A),CholmodDense(b),CHOLMOD_COL)
    Acp
end

function CholmodFactor{Tv<:CHMVTypes,Ti<:CHMITypes}(cp::Ptr{c_CholmodFactor{Tv,Ti}})
    cfp = unsafe_ref(cp)
    Perm = pointer_to_array(cfp.Perm, (cfp.n,), true)
    ColCount = pointer_to_array(cfp.ColCount, (cfp.n,), true)
    p = pointer_to_array(cfp.p, (cfp.p == C_NULL ? 0 : cfp.n + 1,), true)
    i = pointer_to_array(cfp.i, (cfp.i == C_NULL ? 0 : cfp.nzmax,), true)
    x = pointer_to_array(cfp.x, (cfp.x == C_NULL ? 0 : cfp.nzmax,), true)
    nz = pointer_to_array(cfp.nz, (cfp.nz == C_NULL ? 0 : cfp.n,), true)
    next = pointer_to_array(cfp.next, (cfp.next == C_NULL ? 0 : cfp.n + 2,), true)
    prev = pointer_to_array(cfp.prev, (cfp.prev == C_NULL ? 0 : cfp.n + 2,), true)
    super = pointer_to_array(cfp.super, (cfp.super == C_NULL ? 0 : cfp.nsuper + 1,), true)
    pi = pointer_to_array(cfp.pi, (cfp.pi == C_NULL ? 0 : cfp.nsuper + 1,), true)
    px = pointer_to_array(cfp.px, (cfp.px == C_NULL ? 0 : cfp.nsuper + 1,), true)
    s = pointer_to_array(cfp.s, (cfp.s == C_NULL ? 0 : cfp.ssize + 1,), true)
    cf = CholmodFactor{Tv,Ti}(cfp, Perm, ColCount, p, i, x, nz, next, prev,
                              super, pi, px, s)
    c_free(cp)
    cf
end

for (anl,chng,fac,slv,spslv,itype) in
    ((:cholmod_analyze,:cholmod_change_factor,:cholmod_factorize,
      :cholmod_solve,:cholmod_spsolve,:Int32),
     (:cholmod_l_analyze,:cholmod_l_change_factor,:cholmod_l_factorize,
      :cholmod_l_solve,:cholmod_l_spsolve,:Int64))
    @eval begin
        function chm_analyze{Tv<:CHMVTypes}(a::c_CholmodSparse{Tv,$itype})
            ccall(($(string(anl)),:libcholmod), Ptr{c_CholmodFactor{Tv,$itype}},
                  (Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Uint8}), &a, cmn(a))
        end
        # update the factorization
        function chm_factorize!{Tv<:CHMVTypes}(l::c_CholmodFactor{Tv,$itype},
                                               a::c_CholmodSparse{Tv,$itype})
            status = ccall(($(string(fac)),:libcholmod), Int32,
                           (Ptr{c_CholmodSparse{Tv,$itype}},
                            Ptr{c_CholmodFactor{Tv,$itype}}, Ptr{Uint8}),
                           &a, &l, cmn(a))
            if status != CHOLMOD_TRUE throw(CholmodException) end
        end
        # initialize a factorization
        function chm_factorize{Tv<:CHMVTypes}(a::c_CholmodSparse{Tv,$itype}, ll::Bool)
            Lpt = ccall(($(string(anl)),:libcholmod), Ptr{c_CholmodFactor{Tv,$itype}},
                        (Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Uint8}), &a, cmn(a))
            status = ccall(($(string(fac)),:libcholmod), Int32,
                           (Ptr{c_CholmodSparse{Tv,$itype}},
                            Ptr{c_CholmodFactor{Tv,$itype}}, Ptr{Uint8}),
                           &a, Lpt, cmn(a))
            if status != CHOLMOD_TRUE throw(CholmodException) end
            l = unsafe_ref(Lpt)
            if int32(ll) != l.is_ll
                status = ccall(($(string(chng)),:libcholmod), Int32,
                            (Int32,Int32,Int32,Int32,Int32,
                             Ptr{c_CholmodFactor{Tv,$itype}}, Ptr{Uint8}),
                            l.xtype,ll,l.is_super,true,true,Lpt,cmn(l))
                if status != CHOLMOD_TRUE throw(CholmodException) end
            end
            CholmodFactor(Lpt)
        end
        function chm_solve{Tv<:CHMVTypes}(l::c_CholmodFactor{Tv,$itype},
                                          b::c_CholmodDense{Tv}, typ::Integer)
            ccall(($(string(slv)),:libcholmod), Ptr{c_CholmodDense{Tv}},
                  (Int32, Ptr{c_CholmodFactor{Tv,$itype}},
                   Ptr{c_CholmodDense{Tv}}, Ptr{Uint8}),
                  typ, &l, &b, cmn(l))
        end
        function chm_spsolve{Tv<:CHMVTypes}(l::c_CholmodFactor{Tv,$itype},
                                            b::c_CholmodSparse{Tv,$itype},
                                            typ::Integer)
            ccall(($(string(spslv)),:libcholmod), Ptr{c_CholmodSparse{Tv,$itype}},
                  (Int32, Ptr{c_CholmodFactor{Tv,$itype}},
                   Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Uint8}),
                  typ, &l, &b, cmn(l))
        end
    end
end
chm_analyze(ap::Ptr{c_CholmodSparse}) = chm_analyze(unsafe_ref(ap))
chm_analyze(A::CholmodSparse) = chm_analyze(A.c)
chm_analyze(A::SparseMatrixCSC) = chm_analyze(CholmodSparse(A).c)

chm_factorize(a::c_CholmodSparse) = chm_factorize(a,false)
chm_factorize(A::CholmodSparse) = chm_factorize(A.c,false)
chm_factorize(A::CholmodSparse,ll::Bool) = chm_factorize(A.c,ll) 
chm_factorize(A::SparseMatrixCSC) = chm_factorize(CholmodSparse(A).c,false)
chm_factorize(A::SparseMatrixCSC,ll::Bool) = chm_factorize(CholmodSparse(A).c,ll)
 
function chm_solve{T<:CHMVTypes}(l::c_CholmodFactor{T},b::c_CholmodDense{T})
    chm_solve(l,b,CHOLMOD_A)
end
function chm_solve{T<:CHMVTypes}(L::CholmodFactor{T},B::CholmodDense{T})
    chm_solve(L.c,B.c,CHOLMOD_A)
end

function chm_spsolve{Tv<:CHMVTypes,Ti<:CHMITypes}(l::c_CholmodFactor{Tv,Ti},
                                                  b::c_CholmodSparse{Tv,Ti})
    chm_spsolve(l,b,CHOLMOD_A)
end
function chm_spsolve{Tv<:CHMVTypes,Ti<:CHMITypes}(L::CholmodFactor{Tv,Ti},
                                                  B::CholmodSparse{Tv,Ti})
    chm_spsolve(L.c,B.c,CHOLMOD_A)
end

for (chng,pack,cop,xtyp,f2s,itype) in
    ((:cholmod_change_factor,:cholmod_pack_factor,
      :cholmod_copy_factor,:cholmod_factor_xtype,
      :cholmod_factor_to_sparse,:Int32),
     (:cholmod_l_change_factor,:cholmod_l_pack_factor,
      :cholmod_l_copy_factor,:cholmod_l_factor_xtype,
      :cholmod_l_factor_to_sparse,:Int64))
    @eval begin
        ## changing the factor is problematic because it reallocates the storage
        ## for the arrays and frees the old arrays but Julia retains the old pointers
        ## in the vectors
        ## function chm_chng_fac!{Tv<:CHMVTypes}(l::c_CholmodFactor{Tv,$itype},
        ##                                       xt,ll,super,packed,monotonic)
        ##     status = ccall(($(string(chng)),:libcholmod), Int32,
        ##                    (Int32,Int32,Int32,Int32,Int32,
        ##                     Ptr{c_CholmodFactor{Tv,$itype}}, Ptr{Uint8}),
        ##                    xt,ll,super,packed,monotonic,&l,cmn(l))
        ##     if status != CHOLMOD_TRUE throw(CholmodException) end
        ## end
        function chm_copy_fac{Tv<:CHMVTypes}(l::c_CholmodFactor{Tv,$itype})
            ccall(($(string(cop)),:libcholmod), Ptr{c_CholmodFactor{Tv,$itype}},
                  (Ptr{c_CholmodFactor{Tv,$itype}}, Ptr{Uint8}), &l,cmn(l))
        end
        function chm_fac_to_sp{Tv<:CHMVTypes}(l::c_CholmodFactor{Tv,$itype})
            ccall(($(string(f2s)),:libcholmod), Ptr{c_CholmodSparse{Tv,$itype}},
                   (Ptr{c_CholmodFactor{Tv,$itype}}, Ptr{Uint8}), &l,cmn(l))
        end
        function chm_fac_xtype!{Tv<:CHMVTypes}(l::c_CholmodFactor{Tv,$itype},to_xtype)
            status = ccall(($(string(xtyp)),:libcholmod), Int32,
                           (Int32, Ptr{c_CholmodFactor{Tv,$itype}}, Ptr{Uint8}),
                           to_xtype,&l,cmn(l))
            if status != CHOLMOD_TRUE throw(CholmodException) end
        end
        function chm_pack_fac!{Tv<:CHMVTypes}(l::c_CholmodFactor{Tv,$itype})
            status = ccall(($(string(pack)),:libcholmod), Int32,
                           (Ptr{c_CholmodFactor{Tv,$itype}}, Ptr{Uint8}),
                           &l,cmn(l))
            if status != CHOLMOD_TRUE throw(CholmodException) end
        end
    end
end
function chm_chng_fac!(L::CholmodFactor,xt,ll,super,packed,monotonic)
    chm_chng_fac!(L.c, xt,ll,super,packed,monotonic)
end

copy(L::CholmodFactor) = CholmodFactor(chm_copy_fac(L.c))
CholmodSparse(L::CholmodFactor) = CholmodSparse(chm_fac_to_sp(L.c))

function chm_fac_xtype!{Tv<:CHMVTypes,Ti<:CHMITypes}(L::CholmodFactor{Tv,Ti},to_xtype)
    chm_fac_xtype(L.c,to_xtype)
end

function CholmodTriplet{Tv<:CHMVTypes,Ti<:CHMITypes}(tp::Ptr{c_CholmodTriplet{Tv,Ti}})
    ctp = unsafe_ref(tp)
    i = pointer_to_array(ctp.i, (ctp.nnz,), true)
    j = pointer_to_array(ctp.j, (ctp.nnz,), true)    
    x = pointer_to_array(ctp.x, (ctp.x == C_NULL ? 0 : ctp.nnz), true)
    ct = CholmodTriplet{Tv,Ti}(ctp, i, j, x)
    c_free(tp)
    ct
end
    
for (s2t,itype) in
    ((:cholmod_sparse_to_triplet, :Int32),
     (:cholmod_l_sparse_to_triplet, :Int64))
    @eval begin
        function chm_sp_to_tr{Tv<:CHMVTypes}(a::c_CholmodSparse{Tv,$itype})
            ccall(($(string(s2t)), :libcholmod), Ptr{c_CholmodTriplet{Tv,$itype}},
                  (Ptr{c_CholmodSparse{Tv,$itype}}, Ptr{Uint8}), &a, chm(a))
        end
    end
end
chm_sp_to_tr(A::CholmodSparse) = chm_sp_to_tr(A.c)

function findn_nzs{Tv,Ti}(A::CholmodSparse{Tv,Ti})
    jj = similar(A.rowval0)             # expand A.colptr0 to a vector of indices
    for j in 1:A.c.n, k in (A.colptr0[j]+1):A.colptr0[j+1]
        jj[k] = j
    end

    ind = similar(A.rowval0)
    ipos = 1
    count = 0
    for k in 1:length(A.nzval)
        if A.nzval[k] != 0
            ind[ipos] = k
            ipos += 1
            count += 1
        else
            println("Warning: sparse matrix contains explicitly stored zeros.")
        end
    end
    ind = ind[1:count]                  # ind is the indices of nonzeros in A.nzval
    (increment!(A.rowval0[ind]), jj[ind], A.nzval[ind])
end

findn_nzs(L::CholmodFactor) = findn_nzs(chm_fac_to_sp(L))

function diag{Tv}(A::CholmodSparse{Tv})
    minmn = min(size(A))
    res = zeros(Tv,minmn)
    cp0 = A.colptr0
    rv0 = A.rowval0
    anz = A.nzval
    for j in 1:minmn, k in (cp0[j]+1):cp0[j+1]
        if rv0[k] == j-1
            res[j] += anz[k]
        end
    end
    res
end

function diag{Tv}(L::CholmodFactor{Tv})
    res = zeros(Tv,L.c.n)
    if L.c.is_super != 0 error("Method for supernodal factors not yet written") end
    c0 = L.p
    r0 = L.i
    xv = L.x
    for j in 1:length(c0)-1
        jj = c0[j]+1
        assert(r0[jj] == j-1)
        res[j] = xv[jj]
    end
    res
end

function logdet{Tv,Ti}(L::CholmodFactor{Tv,Ti})
    if L.c.is_super != 0 error("Method for supernodal factors not yet written") end
    c0 = L.p
    r0 = L.i
    xv = L.x
    res = zero(Tv)
    for j in 1:length(c0)-1
        jj = c0[j]+1
        assert(r0[jj] == j-1)
        res += log(xv[jj])
    end
    L.c.is_ll != 0 ? 2res : res
end

end #module
