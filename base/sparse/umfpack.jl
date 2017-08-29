# This file is a part of Julia. License is MIT: https://julialang.org/license

module UMFPACK

export UmfpackLU

import Base: (\), Ac_ldiv_B, At_ldiv_B, findnz, getindex, show, size
import Base.LinAlg: A_ldiv_B!, Ac_ldiv_B!, At_ldiv_B!, Factorization, det, lufact

importall ..SparseArrays
import ..SparseArrays: increment, increment!, decrement, decrement!, nnz

include("umfpack_h.jl")
mutable struct MatrixIllConditionedException <: Exception
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
    :(umferror($(esc(A))))
end

# check the size of SuiteSparse_long
if Int(ccall((:jl_cholmod_sizeof_long,:libsuitesparse_wrapper),Csize_t,())) == 4
    const UmfpackIndexTypes = (:Int32,)
    const UMFITypes = Int32
else
    const UmfpackIndexTypes = (:Int32, :Int64)
    const UMFITypes = Union{Int32, Int64}
end

const UMFVTypes = Union{Float64,Complex128}

## UMFPACK

# the control and info arrays
const umf_ctrl = Vector{Float64}(UMFPACK_CONTROL)
ccall((:umfpack_dl_defaults,:libumfpack), Void, (Ptr{Float64},), umf_ctrl)
const umf_info = Vector{Float64}(UMFPACK_INFO)

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
mutable struct UmfpackLU{Tv<:UMFVTypes,Ti<:UMFITypes} <: Factorization{Tv}
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
`UmfpackLU{Tv, Ti}`, with `Tv` = [`Float64`](@ref) or `Complex128` respectively and
`Ti` is an integer type ([`Int32`](@ref) or [`Int64`](@ref)).

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

- [`\\`](@ref)
- [`cond`](@ref)
- [`det`](@ref)

!!! note
    `lufact(A::SparseMatrixCSC)` uses the UMFPACK library that is part of
    SuiteSparse. As this library only supports sparse matrices with [`Float64`](@ref) or
    `Complex128` elements, `lufact` converts `A` into a copy that is of type
    `SparseMatrixCSC{Float64}` or `SparseMatrixCSC{Complex128}` as appropriate.
"""
function lufact(S::SparseMatrixCSC{<:UMFVTypes,<:UMFITypes})
    zerobased = S.colptr[1] == 0
    res = UmfpackLU(C_NULL, C_NULL, S.m, S.n,
                    zerobased ? copy(S.colptr) : decrement(S.colptr),
                    zerobased ? copy(S.rowval) : decrement(S.rowval),
                    copy(S.nzval))
    finalizer(res, umfpack_free_symbolic)
    umfpack_numeric!(res)
end
lufact(A::SparseMatrixCSC{<:Union{Float16,Float32},Ti}) where {Ti<:UMFITypes} =
    lufact(convert(SparseMatrixCSC{Float64,Ti}, A))
lufact(A::SparseMatrixCSC{<:Union{Complex32,Complex64},Ti}) where {Ti<:UMFITypes} =
    lufact(convert(SparseMatrixCSC{Complex128,Ti}, A))
lufact(A::Union{SparseMatrixCSC{T},SparseMatrixCSC{Complex{T}}}) where {T<:AbstractFloat} =
    throw(ArgumentError(string("matrix type ", typeof(A), "not supported. ",
    "Try lufact(convert(SparseMatrixCSC{Float64/Complex128,Int}, A)) for ",
    "sparse floating point LU using UMFPACK or lufact(Array(A)) for generic ",
    "dense LU.")))
lufact(A::SparseMatrixCSC) = lufact(float(A))


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
            tmp = Vector{Ptr{Void}}(1)
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
            tmp = Vector{Ptr{Void}}(1)
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
            tmp = Vector{Ptr{Void}}(1)
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
            tmp = Vector{Ptr{Void}}(1)
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
        function solve!(x::StridedVector{Float64}, lu::UmfpackLU{Float64,$itype}, b::StridedVector{Float64}, typ::Integer)
            if x === b
                throw(ArgumentError("output array must not be aliased with input array"))
            end
            if stride(x, 1) != 1 || stride(b, 1) != 1
                throw(ArgumentError("in and output vectors must have unit strides"))
            end
            umfpack_numeric!(lu)
            (size(b,1) == lu.m) && (size(b) == size(x)) || throw(DimensionMismatch())
            @isok ccall(($sol_r, :libumfpack), $itype,
                ($itype, Ptr{$itype}, Ptr{$itype}, Ptr{Float64},
                 Ptr{Float64}, Ptr{Float64}, Ptr{Void}, Ptr{Float64},
                 Ptr{Float64}),
                typ, lu.colptr, lu.rowval, lu.nzval,
                x, b, lu.numeric, umf_ctrl,
                umf_info)
            return x
        end
        function solve!(x::StridedVector{Complex128}, lu::UmfpackLU{Complex128,$itype}, b::StridedVector{Complex128}, typ::Integer)
            if x === b
                throw(ArgumentError("output array must not be aliased with input array"))
            end
            if stride(x, 1) != 1 || stride(b, 1) != 1
                throw(ArgumentError("in and output vectors must have unit strides"))
            end
            umfpack_numeric!(lu)
            (size(b, 1) == lu.m) && (size(b) == size(x)) || throw(DimensionMismatch())
            n = size(b, 1)
            @isok ccall(($sol_c, :libumfpack), $itype,
                        ($itype, Ptr{$itype}, Ptr{$itype}, Ptr{Float64},
                         Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
                         Ptr{Float64}, Ptr{Void}, Ptr{Float64}, Ptr{Float64}),
                        typ, lu.colptr, lu.rowval, lu.nzval,
                        C_NULL, x, C_NULL, b,
                        C_NULL, lu.numeric, umf_ctrl, umf_info)
            return x
        end
        function det(lu::UmfpackLU{Float64,$itype})
            mx = Ref{Float64}()
            @isok ccall(($det_r,:libumfpack), $itype,
                           (Ptr{Float64},Ptr{Float64},Ptr{Void},Ptr{Float64}),
                           mx, C_NULL, lu.numeric, umf_info)
            mx[]
        end
        function det(lu::UmfpackLU{Complex128,$itype})
            mx = Ref{Float64}()
            mz = Ref{Float64}()
            @isok ccall(($det_z,:libumfpack), $itype,
                        (Ptr{Float64},Ptr{Float64},Ptr{Float64},Ptr{Void},Ptr{Float64}),
                        mx, mz, C_NULL, lu.numeric, umf_info)
            complex(mx[], mz[])
        end
        function umf_lunz(lu::UmfpackLU{Float64,$itype})
            lnz = Ref{$itype}()
            unz = Ref{$itype}()
            n_row = Ref{$itype}()
            n_col = Ref{$itype}()
            nz_diag = Ref{$itype}()
            @isok ccall(($lunz_r,:libumfpack), $itype,
                           (Ptr{$itype},Ptr{$itype},Ptr{$itype},Ptr{$itype},Ptr{$itype},Ptr{Void}),
                           lnz, unz, n_row, n_col, nz_diag, lu.numeric)
            (lnz[], unz[], n_row[], n_col[], nz_diag[])
        end
        function umf_lunz(lu::UmfpackLU{Complex128,$itype})
            lnz = Ref{$itype}()
            unz = Ref{$itype}()
            n_row = Ref{$itype}()
            n_col = Ref{$itype}()
            nz_diag = Ref{$itype}()
            @isok ccall(($lunz_z,:libumfpack), $itype,
                           (Ptr{$itype},Ptr{$itype},Ptr{$itype},Ptr{$itype},Ptr{$itype},Ptr{Void}),
                           lnz, unz, n_row, n_col, nz_diag, lu.numeric)
            (lnz[], unz[], n_row[], n_col[], nz_diag[])
        end
        function umf_extract(lu::UmfpackLU{Float64,$itype})
            umfpack_numeric!(lu)        # ensure the numeric decomposition exists
            (lnz, unz, n_row, n_col, nz_diag) = umf_lunz(lu)
            Lp = Vector{$itype}(n_row + 1)
            Lj = Vector{$itype}(lnz) # L is returned in CSR (compressed sparse row) format
            Lx = Vector{Float64}(lnz)
            Up = Vector{$itype}(n_col + 1)
            Ui = Vector{$itype}(unz)
            Ux = Vector{Float64}(unz)
            P  = Vector{$itype}(n_row)
            Q  = Vector{$itype}(n_col)
            Rs = Vector{Float64}(n_row)
            @isok ccall(($get_num_r,:libumfpack), $itype,
                        (Ptr{$itype},Ptr{$itype},Ptr{Float64},
                         Ptr{$itype},Ptr{$itype},Ptr{Float64},
                         Ptr{$itype},Ptr{$itype},Ptr{Void},
                         Ref{$itype},Ptr{Float64},Ptr{Void}),
                        Lp,Lj,Lx,
                        Up,Ui,Ux,
                        P, Q, C_NULL,
                        0, Rs, lu.numeric)
            (transpose(SparseMatrixCSC(min(n_row, n_col), n_row, increment!(Lp), increment!(Lj), Lx)),
             SparseMatrixCSC(min(n_row, n_col), n_col, increment!(Up), increment!(Ui), Ux),
             increment!(P), increment!(Q), Rs)
        end
        function umf_extract(lu::UmfpackLU{Complex128,$itype})
            umfpack_numeric!(lu)        # ensure the numeric decomposition exists
            (lnz, unz, n_row, n_col, nz_diag) = umf_lunz(lu)
            Lp = Vector{$itype}(n_row + 1)
            Lj = Vector{$itype}(lnz) # L is returned in CSR (compressed sparse row) format
            Lx = Vector{Float64}(lnz)
            Lz = Vector{Float64}(lnz)
            Up = Vector{$itype}(n_col + 1)
            Ui = Vector{$itype}(unz)
            Ux = Vector{Float64}(unz)
            Uz = Vector{Float64}(unz)
            P  = Vector{$itype}(n_row)
            Q  = Vector{$itype}(n_col)
            Rs = Vector{Float64}(n_row)
            @isok ccall(($get_num_z,:libumfpack), $itype,
                        (Ptr{$itype},Ptr{$itype},Ptr{Float64},Ptr{Float64},
                         Ptr{$itype},Ptr{$itype},Ptr{Float64},Ptr{Float64},
                         Ptr{$itype},Ptr{$itype},Ptr{Void}, Ptr{Void},
                         Ref{$itype},Ptr{Float64},Ptr{Void}),
                        Lp,Lj,Lx,Lz,
                        Up,Ui,Ux,Uz,
                        P, Q, C_NULL, C_NULL,
                        0, Rs, lu.numeric)
            (transpose(SparseMatrixCSC(min(n_row, n_col), n_row, increment!(Lp), increment!(Lj), complex.(Lx, Lz))),
             SparseMatrixCSC(min(n_row, n_col), n_col, increment!(Up), increment!(Ui), complex.(Ux, Uz)),
             increment!(P), increment!(Q), Rs)
        end
    end
end

function nnz(lu::UmfpackLU)
    lnz, unz, = umf_lunz(lu)
    return Int(lnz + unz)
end

### Solve with Factorization
A_ldiv_B!(lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} = A_ldiv_B!(B, lu, copy(B))
At_ldiv_B!(lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} = At_ldiv_B!(B, lu, copy(B))
Ac_ldiv_B!(lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} = Ac_ldiv_B!(B, lu, copy(B))
A_ldiv_B!(lu::UmfpackLU{Float64}, B::StridedVecOrMat{<:Complex}) = A_ldiv_B!(B, lu, copy(B))
At_ldiv_B!(lu::UmfpackLU{Float64}, B::StridedVecOrMat{<:Complex}) = At_ldiv_B!(B, lu, copy(B))
Ac_ldiv_B!(lu::UmfpackLU{Float64}, B::StridedVecOrMat{<:Complex}) = Ac_ldiv_B!(B, lu, copy(B))

A_ldiv_B!(X::StridedVecOrMat{T}, lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} =
    _Aq_ldiv_B!(X, lu, B, UMFPACK_A)
At_ldiv_B!(X::StridedVecOrMat{T}, lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} =
    _Aq_ldiv_B!(X, lu, B, UMFPACK_Aat)
Ac_ldiv_B!(X::StridedVecOrMat{T}, lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} =
    _Aq_ldiv_B!(X, lu, B, UMFPACK_At)
A_ldiv_B!(X::StridedVecOrMat{Tb}, lu::UmfpackLU{Float64}, B::StridedVecOrMat{Tb}) where {Tb<:Complex} =
    _Aq_ldiv_B!(X, lu, B, UMFPACK_A)
At_ldiv_B!(X::StridedVecOrMat{Tb}, lu::UmfpackLU{Float64}, B::StridedVecOrMat{Tb}) where {Tb<:Complex} =
    _Aq_ldiv_B!(X, lu, B, UMFPACK_Aat)
Ac_ldiv_B!(X::StridedVecOrMat{Tb}, lu::UmfpackLU{Float64}, B::StridedVecOrMat{Tb}) where {Tb<:Complex} =
    _Aq_ldiv_B!(X, lu, B, UMFPACK_At)

function _Aq_ldiv_B!(X::StridedVecOrMat, lu::UmfpackLU, B::StridedVecOrMat, transposeoptype)
    if size(X, 2) != size(B, 2)
        throw(DimensionMismatch("input and output arrays must have same number of columns"))
    end
    _AqldivB_kernel!(X, lu, B, transposeoptype)
    return X
end
function _AqldivB_kernel!(x::StridedVector{T}, lu::UmfpackLU{T},
                          b::StridedVector{T}, transposeoptype) where T<:UMFVTypes
    solve!(x, lu, b, transposeoptype)
end
function _AqldivB_kernel!(X::StridedMatrix{T}, lu::UmfpackLU{T},
                          B::StridedMatrix{T}, transposeoptype) where T<:UMFVTypes
    for col in 1:size(X, 2)
        solve!(view(X, :, col), lu, view(B, :, col), transposeoptype)
    end
end
function _AqldivB_kernel!(x::StridedVector{Tb}, lu::UmfpackLU{Float64},
                          b::StridedVector{Tb}, transposeoptype) where Tb<:Complex
    r, i = similar(b, Float64), similar(b, Float64)
    solve!(r, lu, Vector{Float64}(real(b)), transposeoptype)
    solve!(i, lu, Vector{Float64}(imag(b)), transposeoptype)
    map!(complex, x, r, i)
end
function _AqldivB_kernel!(X::StridedMatrix{Tb}, lu::UmfpackLU{Float64},
                          B::StridedMatrix{Tb}, transposeoptype) where Tb<:Complex
    r = similar(B, Float64, size(B, 1))
    i = similar(B, Float64, size(B, 1))
    for j in 1:size(B, 2)
        solve!(r, lu, Vector{Float64}(real(view(B, :, j))), transposeoptype)
        solve!(i, lu, Vector{Float64}(imag(view(B, :, j))), transposeoptype)
        map!(complex, view(X, :, j), r, i)
    end
end


function getindex(lu::UmfpackLU, d::Symbol)
    L,U,p,q,Rs = umf_extract(lu)
    if d == :L
        return L
    elseif d == :U
        return U
    elseif d == :p
        return p
    elseif d == :q
        return q
    elseif d == :Rs
        return Rs
    elseif d == :(:)
        return (L,U,p,q,Rs)
    else
        throw(KeyError(d))
    end
end

for Tv in (:Float64, :Complex128), Ti in UmfpackIndexTypes
    f = Symbol(umf_nm("free_symbolic", Tv, Ti))
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

    f = Symbol(umf_nm("free_numeric", Tv, Ti))
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
