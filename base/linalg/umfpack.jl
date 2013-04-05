module UMFPACK

export UmfpackLU,

       decrement,
       decrement!,
       increment,
       increment!

import Base: (\), Ac_ldiv_B, At_ldiv_B, findnz, getindex, nnz, show, size

import .LinAlg: Factorization, det, lufact, lufact!, solve

include("linalg/umfpack_h.jl")

type MatrixIllConditionedException <: Exception end

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

typealias UMFVTypes Union(Float64,Complex128)
typealias UMFITypes Union(Int32,Int64)

## UMFPACK

# the control and info arrays
const umf_ctrl = Array(Float64, UMFPACK_CONTROL)
ccall((:umfpack_dl_defaults,:libumfpack), Void, (Ptr{Float64},), umf_ctrl)
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

## Should this type be immutable?
type UmfpackLU{Tv<:UMFVTypes,Ti<:UMFITypes} <: Factorization{Tv}
    symbolic::Ptr{Void}
    numeric::Ptr{Void}
    m::Int
    n::Int
    colptr::Vector{Ti}                  # 0-based column pointers
    rowval::Vector{Ti}                  # 0-based row indices
    nzval::Vector{Tv}
end

function lufact{Tv<:UMFVTypes,Ti<:UMFITypes}(S::SparseMatrixCSC{Tv,Ti})
    zerobased = S.colptr[1] == 0
    res = UmfpackLU(C_NULL, C_NULL, S.m, S.n,
                    zerobased ? copy(S.colptr) : decrement(S.colptr),
                    zerobased ? copy(S.rowval) : decrement(S.rowval),
                    copy(S.nzval))
    finalizer(res, umfpack_free_symbolic)
    umfpack_numeric!(res)
end

function lufact!{Tv<:UMFVTypes,Ti<:UMFITypes}(S::SparseMatrixCSC{Tv,Ti})
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

## Wrappers for UMFPACK functions

for (sym_r,sym_c,num_r,num_c,sol_r,sol_c,det_r,det_z,lunz,get_num_r,get_num_z,itype) in
    (("umfpack_di_symbolic","umfpack_zi_symbolic",
      "umfpack_di_numeric","umfpack_zi_numeric",
      "umfpack_di_solve","umfpack_zi_solve",
      "umfpack_di_get_determinant","umfpack_zi_get_determinant",
      "umfpack_di_get_lunz","umfpack_di_get_numeric","umfpack_zi_get_numeric",:Int32),
     ("umfpack_dl_symbolic","umfpack_zl_symbolic",
      "umfpack_dl_numeric","umfpack_zl_numeric",
      "umfpack_dl_solve","umfpack_zl_solve",
      "umfpack_dl_get_determinant","umfpack_zl_get_determinant",
      "umfpack_dl_get_lunz","umfpack_dl_get_numeric","umfpack_zl_get_numeric",:Int64))
    @eval begin
        function umfpack_symbolic!{Tv<:Float64,Ti<:$itype}(U::UmfpackLU{Tv,Ti})
            if U.symbolic != C_NULL return U end
            tmp = Array(Ptr{Void},1)
            status = ccall(($sym_r, :libumfpack), Ti,
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
            status = ccall(($sym_r, :libumfpack), Ti,
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
            status = ccall(($num_r, :libumfpack), Ti,
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
            status = ccall(($num_r, :libumfpack), Ti,
                           (Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Float64}, Ptr{Void}, Ptr{Void}, 
                            Ptr{Float64}, Ptr{Float64}),
                           U.colptr, U.rowval, real(U.nzval), imag(U.nzval), U.symbolic, tmp,
                           umf_ctrl, umf_info)
            if status > 0; throw(MatrixIllConditionedException); end
            if status != UMFPACK_OK; error("Error code $status from numeric factorization"); end
            U.numeric = tmp[1]
            U
        end
        function solve{Tv<:Float64,Ti<:$itype}(lu::UmfpackLU{Tv,Ti}, b::Vector{Tv}, typ::Integer)
            umfpack_numeric!(lu)
            x = similar(b)
            status = ccall(($sol_r, :libumfpack), Ti,
                           (Ti, Ptr{Ti}, Ptr{Ti}, Ptr{Float64}, Ptr{Float64},
                            Ptr{Float64}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                           typ, lu.colptr, lu.rowval, lu.nzval, x, b, lu.numeric, umf_ctrl, umf_info)
            if status != UMFPACK_OK; error("Error code $status in umfpack_solve"); end
            return x
        end
        function solve{Tv<:Complex128,Ti<:$itype}(lu::UmfpackLU{Tv,Ti}, b::Vector{Tv}, typ::Integer)
            umfpack_numeric!(lu)
            xr = similar(b, Float64)
            xi = similar(b, Float64)
            status = ccall(($sol_c, :libumfpack),
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
            status = ccall(($get_num_r,:libumfpack), Ti,
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

### Solve with Factorization

(\){T<:UMFVTypes}(fact::UmfpackLU{T}, b::Vector{T}) = solve(fact, b)
(\){Ts<:UMFVTypes,Tb<:Number}(fact::UmfpackLU{Ts}, b::Vector{Tb}) = fact\convert(Vector{Ts},b)
At_ldiv_B{T<:UMFVTypes}(fact::UmfpackLU{T}, b::Vector{T}) = solve(fact, b, UMFPACK_Aat)
At_ldiv_B{Ts<:UMFVTypes,Tb<:Number}(fact::UmfpackLU{Ts}, b::Vector{Tb}) = fact.'\convert(Vector{Ts},b)
Ac_ldiv_B{T<:UMFVTypes}(fact::UmfpackLU{T}, b::Vector{T}) = solve(fact, b, UMFPACK_At)
Ac_ldiv_B{Ts<:UMFVTypes,Tb<:Number}(fact::UmfpackLU{Ts}, b::Vector{Tb}) = fact'\convert(Vector{Ts},b)

### Solve directly with matrix

(\)(S::SparseMatrixCSC, b::Vector) = lufact(S) \ b
At_ldiv_B{T<:UMFVTypes}(S::SparseMatrixCSC{T}, b::Vector{T}) = solve(lufact(S), b, UMFPACK_Aat)
function At_ldiv_B{Ts<:UMFVTypes,Tb<:Number}(S::SparseMatrixCSC{Ts}, b::Vector{Tb})
    ## should be more careful here in case Ts<:Real and Tb<:Complex
    At_ldiv_B(S, convert(Vector{Ts}, b))
end
Ac_ldiv_B{T<:UMFVTypes}(S::SparseMatrixCSC{T}, b::Vector{T}) = solve(lufact(S), b, UMFPACK_At)
function Ac_ldiv_B{Ts<:UMFVTypes,Tb<:Number}(S::SparseMatrixCSC{Ts}, b::Vector{Tb})
    ## should be more careful here in case Ts<:Real and Tb<:Complex
    Ac_ldiv_B(S, convert(Vector{Ts}, b))
end

solve(lu::UmfpackLU, b::Vector) = solve(lu, b, UMFPACK_A)

function getindex(lu::UmfpackLU, d::Symbol)
    L,U,p,q,Rs = umf_extract(lu)
    d == :L ? L :
    (d == :U ? U :
     (d == :p ? p :
      (d == :q ? q :
       (d == :Rs ? Rs :
        (d == :(:) ? (L,U,p,q,Rs) :
         error("No component for symbol $d"))))))
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
    umfpack_report_numeric(umfpack_numeric!(lu).numeric, level)
end

umfpack_report_numeric(lu::UmfpackLU) = umfpack_report_numeric(lu,4.)

end # UMFPACK module
 
