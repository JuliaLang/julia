# This file is a part of Julia. License is MIT: http://julialang.org/license

module UMFPACK

export UmfpackLU

import Base: (\), Ac_ldiv_B, At_ldiv_B, findnz, getindex, show, size
import Base.LinAlg: A_ldiv_B!, Ac_ldiv_B!, At_ldiv_B!, Factorization, det, lufact

importall ..SparseArrays
import ..SparseArrays: increment, increment!, decrement, decrement!, nnz

include("umfpack_h.jl")
type MatrixIllConditionedException <: Exception
    message::AbstractString
end

function umferror(status::Integer)
    if status==UMFPACK_OK
        return
    elseif status==UMFPACK_WARNING_singular_matrix
        throw(LinAlg.SingularException(0))
    elseif status==UMFPACK_WARNING_determinant_underflow
        throw(MatrixIllConditionedException("the determinant is nonzero but underflowed"))
    elseif status==UMFPACK_WARNING_determinant_overflow
        throw(MatrixIllConditionedException("the determinant overflowed"))
    elseif status==UMFPACK_ERROR_out_of_memory
        throw(OutOfMemoryError())
    elseif status==UMFPACK_ERROR_invalid_Numeric_object
        throw(ArgumentError("invalid UMFPack numeric object"))
    elseif status==UMFPACK_ERROR_invalid_Symbolic_object
        throw(ArgumentError("invalid UMFPack symbolic object"))
    elseif status==UMFPACK_ERROR_argument_missing
        throw(ArgumentError("a required argument to UMFPack is missing"))
    elseif status==UMFPACK_ERROR_n_nonpositive
        throw(ArgumentError("the number of rows or columns of the matrix must be greater than zero"))
    elseif status==UMFPACK_ERROR_invalid_matrix
        throw(ArgumentError("invalid matrix"))
    elseif status==UMFPACK_ERROR_different_pattern
        throw(ArgumentError("pattern of the matrix changed"))
    elseif status==UMFPACK_ERROR_invalid_system
        throw(ArgumentError("invalid sys argument provided to UMFPack solver"))
    elseif status==UMFPACK_ERROR_invalid_permutation
        throw(ArgumentError("invalid permutation"))
    elseif status==UMFPACK_ERROR_file_IO
        throw(ErrorException("error saving / loading UMFPack decomposition"))
    elseif status==UMFPACK_ERROR_ordering_failed
        throw(ErrorException("the ordering method failed"))
    elseif status==UMFPACK_ERROR_internal_error
        throw(ErrorException("an internal error has occurred, of unknown cause"))
    else
        throw(ErrorException("unknown UMFPack error code: $status"))
    end
end

macro isok(A)
    :(umferror($A))
end

# check the size of SuiteSparse_long
if Int(ccall((:jl_cholmod_sizeof_long,:libsuitesparse_wrapper),Csize_t,())) == 4
    const UmfpackIndexTypes = (:Int32,)
    typealias UMFITypes Int32
else
    const UmfpackIndexTypes = (:Int32, :Int64)
    typealias UMFITypes Union{Int32, Int64}
end

typealias UMFVTypes Union{Float64,Complex128}

## UMFPACK

# the control and info arrays
const umf_ctrl = Array(Float64, UMFPACK_CONTROL)
ccall((:umfpack_dl_defaults,:libumfpack), Void, (Ptr{Float64},), umf_ctrl)
const umf_info = Array(Float64, UMFPACK_INFO)

function show_umf_ctrl(level::Real = 2.0)
    old_prt::Float64 = umf_ctrl[1]
    umf_ctrl[1] = Float64(level)
    ccall((:umfpack_dl_report_control, :libumfpack), Void, (Ptr{Float64},), umf_ctrl)
    umf_ctrl[1] = old_prt
end

function show_umf_info(level::Real = 2.0)
    old_prt::Float64 = umf_ctrl[1]
    umf_ctrl[1] = Float64(level)
    ccall((:umfpack_dl_report_info, :libumfpack), Void,
          (Ptr{Float64}, Ptr{Float64}), umf_ctrl, umf_info)
    umf_ctrl[1] = old_prt
end

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

"""
    lufact(A::SparseMatrixCSC) -> F::UmfpackLU

Compute the LU factorization of a sparse matrix `A`.

For sparse `A` with real or complex element type, the return type of `F` is
`UmfpackLU{Tv, Ti}`, with `Tv` = `Float64` or `Complex128` respectively and
`Ti` is an integer type (`Int32` or `Int64`).

The individual components of the factorization `F` can be accessed by indexing:

| Component | Description                         |
|:----------|:------------------------------------|
| `F[:L]`   | `L` (lower triangular) part of `LU` |
| `F[:U]`   | `U` (upper triangular) part of `LU` |
| `F[:p]`   | right permutation `Vector`          |
| `F[:q]`   | left permutation `Vector`           |
| `F[:Rs]`  | `Vector` of scaling factors         |
| `F[:(:)]` | `(L,U,p,q,Rs)` components           |

The relation between `F` and `A` is

`F[:L]*F[:U] == (F[:Rs] .* A)[F[:p], F[:q]]`

`F` further supports the following functions:

- [`\\`](:func:`\\`)
- [`cond`](:func:`cond`)
- [`det`](:func:`det`)

** Implementation note **

`lufact(A::SparseMatrixCSC)` uses the UMFPACK library that is part of
SuiteSparse. As this library only supports sparse matrices with `Float64` or
`Complex128` elements, `lufact` converts `A` into a copy that is of type
`SparseMatrixCSC{Float64}` or `SparseMatrixCSC{Complex128}` as appropriate.
"""
function lufact{Tv<:UMFVTypes,Ti<:UMFITypes}(S::SparseMatrixCSC{Tv,Ti})
    zerobased = S.colptr[1] == 0
    res = UmfpackLU(C_NULL, C_NULL, S.m, S.n,
                    zerobased ? copy(S.colptr) : decrement(S.colptr),
                    zerobased ? copy(S.rowval) : decrement(S.rowval),
                    copy(S.nzval))
    finalizer(res, umfpack_free_symbolic)
    umfpack_numeric!(res)
end
lufact{Tv<:Union{Float16,Float32}, Ti<:UMFITypes}(A::SparseMatrixCSC{Tv,Ti}) =
    lufact(convert(SparseMatrixCSC{Float64,Ti}, A))
lufact{Tv<:Union{Complex32,Complex64}, Ti<:UMFITypes}(A::SparseMatrixCSC{Tv,Ti}) =
    lufact(convert(SparseMatrixCSC{Complex128,Ti}, A))
lufact{T<:AbstractFloat}(A::Union{SparseMatrixCSC{T},SparseMatrixCSC{Complex{T}}}) =
    throw(ArgumentError(string("matrix type ", typeof(A), "not supported. ",
    "Try lufact(convert(SparseMatrixCSC{Float64/Complex128,Int}, A)) for ",
    "sparse floating point LU using UMFPACK or lufact(full(A)) for generic ",
    "dense LU.")))
lufact(A::SparseMatrixCSC) = lufact(float(A))
lufact(A::SparseMatrixCSC, pivot::Type{Val{false}}) = lufact(A)


size(F::UmfpackLU) = (F.m, F.n)
function size(F::UmfpackLU, dim::Integer)
    if dim < 1
        throw(ArgumentError("size: dimension $dim out of range"))
    elseif dim == 1
        return Int(F.m)
    elseif dim == 2
        return Int(F.n)
    else
        return 1
    end
end

function show(io::IO, F::UmfpackLU)
    println(io, "UMFPACK LU Factorization of a $(size(F)) sparse matrix")
    F.numeric != C_NULL && println(io, F.numeric)
end

## Wrappers for UMFPACK functions

# generate the name of the C function according to the value and integer types
umf_nm(nm,Tv,Ti) = "umfpack_" * (Tv == :Float64 ? "d" : "z") * (Ti == :Int64 ? "l_" : "i_") * nm

for itype in UmfpackIndexTypes
    sym_r = umf_nm("symbolic", :Float64, itype)
    sym_c = umf_nm("symbolic", :Complex128, itype)
    num_r = umf_nm("numeric", :Float64, itype)
    num_c = umf_nm("numeric", :Complex128, itype)
    sol_r = umf_nm("solve", :Float64, itype)
    sol_c = umf_nm("solve", :Complex128, itype)
    det_r = umf_nm("get_determinant", :Float64, itype)
    det_z = umf_nm("get_determinant", :Complex128, itype)
    lunz_r = umf_nm("get_lunz", :Float64, itype)
    lunz_z = umf_nm("get_lunz", :Complex128, itype)
    get_num_r = umf_nm("get_numeric", :Float64, itype)
    get_num_z = umf_nm("get_numeric", :Complex128, itype)
    @eval begin
        function umfpack_symbolic!(U::UmfpackLU{Float64,$itype})
            if U.symbolic != C_NULL return U end
            tmp = Array(Ptr{Void},1)
            @isok ccall(($sym_r, :libumfpack), $itype,
                        ($itype, $itype, Ptr{$itype}, Ptr{$itype}, Ptr{Float64}, Ptr{Void},
                         Ptr{Float64}, Ptr{Float64}),
                        U.m, U.n, U.colptr, U.rowval, U.nzval, tmp,
                        umf_ctrl, umf_info)
            U.symbolic = tmp[1]
            return U
        end
        function umfpack_symbolic!(U::UmfpackLU{Complex128,$itype})
            if U.symbolic != C_NULL return U end
            tmp = Array(Ptr{Void},1)
            @isok ccall(($sym_c, :libumfpack), $itype,
                        ($itype, $itype, Ptr{$itype}, Ptr{$itype}, Ptr{Float64}, Ptr{Float64}, Ptr{Void},
                         Ptr{Float64}, Ptr{Float64}),
                        U.m, U.n, U.colptr, U.rowval, real(U.nzval), imag(U.nzval), tmp,
                        umf_ctrl, umf_info)
            U.symbolic = tmp[1]
            return U
        end
        function umfpack_numeric!(U::UmfpackLU{Float64,$itype})
            if U.numeric != C_NULL return U end
            if U.symbolic == C_NULL umfpack_symbolic!(U) end
            tmp = Array(Ptr{Void}, 1)
            status = ccall(($num_r, :libumfpack), $itype,
                           (Ptr{$itype}, Ptr{$itype}, Ptr{Float64}, Ptr{Void}, Ptr{Void},
                            Ptr{Float64}, Ptr{Float64}),
                           U.colptr, U.rowval, U.nzval, U.symbolic, tmp,
                           umf_ctrl, umf_info)
            if status != UMFPACK_WARNING_singular_matrix
                umferror(status)
            end
            U.numeric = tmp[1]
            return U
        end
        function umfpack_numeric!(U::UmfpackLU{Complex128,$itype})
            if U.numeric != C_NULL return U end
            if U.symbolic == C_NULL umfpack_symbolic!(U) end
            tmp = Array(Ptr{Void}, 1)
            status = ccall(($num_c, :libumfpack), $itype,
                           (Ptr{$itype}, Ptr{$itype}, Ptr{Float64}, Ptr{Float64}, Ptr{Void}, Ptr{Void},
                            Ptr{Float64}, Ptr{Float64}),
                           U.colptr, U.rowval, real(U.nzval), imag(U.nzval), U.symbolic, tmp,
                           umf_ctrl, umf_info)
            if status != UMFPACK_WARNING_singular_matrix
                umferror(status)
            end
            U.numeric = tmp[1]
            return U
        end
        function solve(lu::UmfpackLU{Float64,$itype}, b::VecOrMat{Float64}, typ::Integer)
            umfpack_numeric!(lu)
            size(b,1)==lu.m || throw(DimensionMismatch())
            x = similar(b)
            joff = 1
            for k = 1:size(b,2)
                @isok ccall(($sol_r, :libumfpack), $itype,
                            ($itype, Ptr{$itype}, Ptr{$itype}, Ptr{Float64}, Ptr{Float64},
                             Ptr{Float64}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                            typ, lu.colptr, lu.rowval, lu.nzval, pointer(x,joff), pointer(b,joff), lu.numeric, umf_ctrl, umf_info)
                joff += size(b,1)
            end
            x
        end
        function solve(lu::UmfpackLU{Complex128,$itype}, b::VecOrMat{Complex128}, typ::Integer)
            umfpack_numeric!(lu)
            size(b,1)==lu.m || throw(DimensionMismatch())
            x = similar(b)
            n = size(b,1)
            br = Array(Float64, n)
            bi = Array(Float64, n)
            xr = Array(Float64, n)
            xi = Array(Float64, n)
            joff = 0
            for k = 1:size(b,2)
                for j = 1:n
                    bj = b[joff+j]
                    br[j] = real(bj)
                    bi[j] = imag(bj)
                end
                @isok ccall(($sol_c, :libumfpack), $itype,
                            ($itype, Ptr{$itype}, Ptr{$itype}, Ptr{Float64}, Ptr{Float64},
                             Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
                             Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                            typ, lu.colptr, lu.rowval, real(lu.nzval), imag(lu.nzval),
                            xr, xi, br, bi,
                            lu.numeric, umf_ctrl, umf_info)
                for j = 1:n
                    x[joff+j] = complex(xr[j],xi[j])
                end
                joff += n
            end
            x
        end
        function det(lu::UmfpackLU{Float64,$itype})
            mx = Array(Float64,1)
            @isok ccall(($det_r,:libumfpack), $itype,
                           (Ptr{Float64},Ptr{Float64},Ptr{Void},Ptr{Float64}),
                           mx, C_NULL, lu.numeric, umf_info)
            mx[1]
        end
        function det(lu::UmfpackLU{Complex128,$itype})
            mx = Array(Float64,1)
            mz = Array(Float64,1)
            @isok ccall(($det_z,:libumfpack), $itype,
                        (Ptr{Float64},Ptr{Float64},Ptr{Float64},Ptr{Void},Ptr{Float64}),
                        mx, mz, C_NULL, lu.numeric, umf_info)
            complex(mx[1], mz[1])
        end
        function umf_lunz(lu::UmfpackLU{Float64,$itype})
            lnz = Array($itype, 1)
            unz = Array($itype, 1)
            n_row = Array($itype, 1)
            n_col = Array($itype, 1)
            nz_diag = Array($itype, 1)
            @isok ccall(($lunz_r,:libumfpack), $itype,
                           (Ptr{$itype},Ptr{$itype},Ptr{$itype},Ptr{$itype},Ptr{$itype},Ptr{Void}),
                           lnz, unz, n_row, n_col, nz_diag, lu.numeric)
            (lnz[1], unz[1], n_row[1], n_col[1], nz_diag[1])
        end
        function umf_lunz(lu::UmfpackLU{Complex128,$itype})
            lnz = Array($itype, 1)
            unz = Array($itype, 1)
            n_row = Array($itype, 1)
            n_col = Array($itype, 1)
            nz_diag = Array($itype, 1)
            @isok ccall(($lunz_z,:libumfpack), $itype,
                           (Ptr{$itype},Ptr{$itype},Ptr{$itype},Ptr{$itype},Ptr{$itype},Ptr{Void}),
                           lnz, unz, n_row, n_col, nz_diag, lu.numeric)
            (lnz[1], unz[1], n_row[1], n_col[1], nz_diag[1])
        end
        function umf_extract(lu::UmfpackLU{Float64,$itype})
            umfpack_numeric!(lu)        # ensure the numeric decomposition exists
            (lnz, unz, n_row, n_col, nz_diag) = umf_lunz(lu)
            Lp = Array($itype, n_row + 1)
            Lj = Array($itype, lnz) # L is returned in CSR (compressed sparse row) format
            Lx = Array(Float64, lnz)
            Up = Array($itype, n_col + 1)
            Ui = Array($itype, unz)
            Ux = Array(Float64, unz)
            P  = Array($itype, n_row)
            Q  = Array($itype, n_col)
            Rs = Array(Float64, n_row)
            @isok ccall(($get_num_r,:libumfpack), $itype,
                        (Ptr{$itype},Ptr{$itype},Ptr{Float64},
                         Ptr{$itype},Ptr{$itype},Ptr{Float64},
                         Ptr{$itype},Ptr{$itype},Ptr{Void},
                         Ptr{$itype},Ptr{Float64},Ptr{Void}),
                        Lp,Lj,Lx,
                        Up,Ui,Ux,
                        P, Q, C_NULL,
                        &0, Rs, lu.numeric)
            (transpose(SparseMatrixCSC(min(n_row, n_col), n_row, increment!(Lp), increment!(Lj), Lx)),
             SparseMatrixCSC(min(n_row, n_col), n_col, increment!(Up), increment!(Ui), Ux),
             increment!(P), increment!(Q), Rs)
        end
        function umf_extract(lu::UmfpackLU{Complex128,$itype})
            umfpack_numeric!(lu)        # ensure the numeric decomposition exists
            (lnz, unz, n_row, n_col, nz_diag) = umf_lunz(lu)
            Lp = Array($itype, n_row + 1)
            Lj = Array($itype, lnz) # L is returned in CSR (compressed sparse row) format
            Lx = Array(Float64, lnz)
            Lz = Array(Float64, lnz)
            Up = Array($itype, n_col + 1)
            Ui = Array($itype, unz)
            Ux = Array(Float64, unz)
            Uz = Array(Float64, unz)
            P  = Array($itype, n_row)
            Q  = Array($itype, n_col)
            Rs = Array(Float64, n_row)
            @isok ccall(($get_num_z,:libumfpack), $itype,
                        (Ptr{$itype},Ptr{$itype},Ptr{Float64},Ptr{Float64},
                         Ptr{$itype},Ptr{$itype},Ptr{Float64},Ptr{Float64},
                         Ptr{$itype},Ptr{$itype},Ptr{Void}, Ptr{Void},
                         Ptr{$itype},Ptr{Float64},Ptr{Void}),
                        Lp,Lj,Lx,Lz,
                        Up,Ui,Ux,Uz,
                        P, Q, C_NULL, C_NULL,
                        &0, Rs, lu.numeric)
            (transpose(SparseMatrixCSC(min(n_row, n_col), n_row, increment!(Lp), increment!(Lj), complex(Lx, Lz))),
             SparseMatrixCSC(min(n_row, n_col), n_col, increment!(Up), increment!(Ui), complex(Ux, Uz)),
             increment!(P), increment!(Q), Rs)
        end
    end
end

function nnz(lu::UmfpackLU)
    lnz, unz, = umf_lunz(lu)
    return Int(lnz + unz)
end

### Solve with Factorization
A_ldiv_B!{T<:UMFVTypes}(lu::UmfpackLU{T}, b::Vector{T}) = solve(lu, b, UMFPACK_A)
A_ldiv_B!{T<:UMFVTypes}(lu::UmfpackLU{T}, b::Matrix{T}) = solve(lu, b, UMFPACK_A)
function A_ldiv_B!{Tb<:Complex}(lu::UmfpackLU{Float64}, b::Vector{Tb})
    r = solve(lu, [convert(Tlu,real(be)) for be in b], UMFPACK_A)
    i = solve(lu, [convert(Tlu,imag(be)) for be in b], UMFPACK_A)
    Tb[r[k]+im*i[k] for k = 1:length(r)]
end

Ac_ldiv_B!{T<:UMFVTypes}(lu::UmfpackLU{T}, b::VecOrMat{T}) = solve(lu, b, UMFPACK_At)
function Ac_ldiv_B!{Tb<:Complex}(lu::UmfpackLU{Float64}, b::Vector{Tb})
    r = solve(lu, [convert(Float64,real(be)) for be in b], UMFPACK_At)
    i = solve(lu, [convert(Float64,imag(be)) for be in b], UMFPACK_At)
    Tb[r[k]+im*i[k] for k = 1:length(r)]
end

At_ldiv_B!{T<:UMFVTypes}(lu::UmfpackLU{T}, b::VecOrMat{T}) = solve(lu, b, UMFPACK_Aat)
function At_ldiv_B!{Tb<:Complex}(lu::UmfpackLU{Float64}, b::Vector{Tb})
    r = solve(lu, [convert(Float64,real(be)) for be in b], UMFPACK_Aat)
    i = solve(lu, [convert(Float64,imag(be)) for be in b], UMFPACK_Aat)
    Tb[r[k]+im*i[k] for k = 1:length(r)]
end

function getindex(lu::UmfpackLU, d::Symbol)
    L,U,p,q,Rs = umf_extract(lu)
    d == :L ? L :
    (d == :U ? U :
     (d == :p ? p :
      (d == :q ? q :
       (d == :Rs ? Rs :
        (d == :(:) ? (L,U,p,q,Rs) :
         throw(KeyError(d)))))))
end

for Tv in (:Float64, :Complex128), Ti in UmfpackIndexTypes
    f = symbol(umf_nm("free_symbolic", Tv, Ti))
    @eval begin
        function ($f)(symb::Ptr{Void})
            tmp = [symb]
            ccall(($(string(f)), :libumfpack), Void, (Ptr{Void},), tmp)
        end

        function umfpack_free_symbolic(lu::UmfpackLU{$Tv,$Ti})
            if lu.symbolic == C_NULL return lu end
            umfpack_free_numeric(lu)
            ($f)(lu.symbolic)
            lu.symbolic = C_NULL
            return lu
        end
    end

    f = symbol(umf_nm("free_numeric", Tv, Ti))
    @eval begin
        function ($f)(num::Ptr{Void})
            tmp = [num]
            ccall(($(string(f)), :libumfpack), Void, (Ptr{Void},), tmp)
        end
        function umfpack_free_numeric(lu::UmfpackLU{$Tv,$Ti})
            if lu.numeric == C_NULL return lu end
            ($f)(lu.numeric)
            lu.numeric = C_NULL
            return lu
        end
    end
end

function umfpack_report_symbolic(symb::Ptr{Void}, level::Real)
    old_prl::Float64 = umf_ctrl[UMFPACK_PRL]
    umf_ctrl[UMFPACK_PRL] = Float64(level)
    @isok ccall((:umfpack_dl_report_symbolic, :libumfpack), Int,
                (Ptr{Void}, Ptr{Float64}), symb, umf_ctrl)
    umf_ctrl[UMFPACK_PRL] = old_prl
end

umfpack_report_symbolic(symb::Ptr{Void}) = umfpack_report_symbolic(symb, 4.)

function umfpack_report_symbolic(lu::UmfpackLU, level::Real)
    umfpack_report_symbolic(umfpack_symbolic!(lu).symbolic, level)
end

umfpack_report_symbolic(lu::UmfpackLU) = umfpack_report_symbolic(lu.symbolic,4.)
function umfpack_report_numeric(num::Ptr{Void}, level::Real)
    old_prl::Float64 = umf_ctrl[UMFPACK_PRL]
    umf_ctrl[UMFPACK_PRL] = Float64(level)
    @isok ccall((:umfpack_dl_report_numeric, :libumfpack), Int,
                (Ptr{Void}, Ptr{Float64}), num, umf_ctrl)
    umf_ctrl[UMFPACK_PRL] = old_prl
end

umfpack_report_numeric(num::Ptr{Void}) = umfpack_report_numeric(num, 4.)
function umfpack_report_numeric(lu::UmfpackLU, level::Real)
    umfpack_report_numeric(umfpack_numeric!(lu).numeric, level)
end

umfpack_report_numeric(lu::UmfpackLU) = umfpack_report_numeric(lu,4.)

end # UMFPACK module
