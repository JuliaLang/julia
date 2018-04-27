# This file is a part of Julia. License is MIT: https://julialang.org/license

module UMFPACK

export UmfpackLU

import Base: (\), getproperty, show, size
using LinearAlgebra
import LinearAlgebra: Factorization, det, lufact, ldiv!

using SparseArrays
import SparseArrays: nnz

import ..increment, ..increment!, ..decrement, ..decrement!

include("umfpack_h.jl")
struct MatrixIllConditionedException <: Exception
    msg::AbstractString
end

function umferror(status::Integer)
    if status==UMFPACK_OK
        return
    elseif status==UMFPACK_WARNING_singular_matrix
        throw(LinearAlgebra.SingularException(0))
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

const UMFVTypes = Union{Float64,ComplexF64}

## UMFPACK

# the control and info arrays
const umf_ctrl = Vector{Float64}(undef, UMFPACK_CONTROL)
ccall((:umfpack_dl_defaults,:libumfpack), Cvoid, (Ptr{Float64},), umf_ctrl)
const umf_info = Vector{Float64}(undef, UMFPACK_INFO)

function show_umf_ctrl(level::Real = 2.0)
    old_prt::Float64 = umf_ctrl[1]
    umf_ctrl[1] = Float64(level)
    ccall((:umfpack_dl_report_control, :libumfpack), Cvoid, (Ptr{Float64},), umf_ctrl)
    umf_ctrl[1] = old_prt
end

function show_umf_info(level::Real = 2.0)
    old_prt::Float64 = umf_ctrl[1]
    umf_ctrl[1] = Float64(level)
    ccall((:umfpack_dl_report_info, :libumfpack), Cvoid,
          (Ptr{Float64}, Ptr{Float64}), umf_ctrl, umf_info)
    umf_ctrl[1] = old_prt
end

## Should this type be immutable?
mutable struct UmfpackLU{Tv<:UMFVTypes,Ti<:UMFITypes} <: Factorization{Tv}
    symbolic::Ptr{Cvoid}
    numeric::Ptr{Cvoid}
    m::Int
    n::Int
    colptr::Vector{Ti}                  # 0-based column pointers
    rowval::Vector{Ti}                  # 0-based row indices
    nzval::Vector{Tv}
end

Base.adjoint(F::UmfpackLU) = Adjoint(F)
Base.transpose(F::UmfpackLU) = Transpose(F)

"""
    lufact(A::SparseMatrixCSC) -> F::UmfpackLU

Compute the LU factorization of a sparse matrix `A`.

For sparse `A` with real or complex element type, the return type of `F` is
`UmfpackLU{Tv, Ti}`, with `Tv` = [`Float64`](@ref) or `ComplexF64` respectively and
`Ti` is an integer type ([`Int32`](@ref) or [`Int64`](@ref)).

The individual components of the factorization `F` can be accessed by indexing:

| Component | Description                         |
|:----------|:------------------------------------|
| `L`       | `L` (lower triangular) part of `LU` |
| `U`       | `U` (upper triangular) part of `LU` |
| `p`       | right permutation `Vector`          |
| `q`       | left permutation `Vector`           |
| `Rs`      | `Vector` of scaling factors         |
| `:`       | `(L,U,p,q,Rs)` components           |

The relation between `F` and `A` is

`F.L*F.U == (F.Rs .* A)[F.p, F.q]`

`F` further supports the following functions:

- [`\\`](@ref)
- [`cond`](@ref)
- [`det`](@ref)

!!! note
    `lufact(A::SparseMatrixCSC)` uses the UMFPACK library that is part of
    SuiteSparse. As this library only supports sparse matrices with [`Float64`](@ref) or
    `ComplexF64` elements, `lufact` converts `A` into a copy that is of type
    `SparseMatrixCSC{Float64}` or `SparseMatrixCSC{ComplexF64}` as appropriate.
"""
function lufact(S::SparseMatrixCSC{<:UMFVTypes,<:UMFITypes})
    zerobased = S.colptr[1] == 0
    res = UmfpackLU(C_NULL, C_NULL, S.m, S.n,
                    zerobased ? copy(S.colptr) : decrement(S.colptr),
                    zerobased ? copy(S.rowval) : decrement(S.rowval),
                    copy(S.nzval))
    finalizer(umfpack_free_symbolic, res)
    umfpack_numeric!(res)
end
lufact(A::SparseMatrixCSC{<:Union{Float16,Float32},Ti}) where {Ti<:UMFITypes} =
    lufact(convert(SparseMatrixCSC{Float64,Ti}, A))
lufact(A::SparseMatrixCSC{<:Union{ComplexF16,ComplexF32},Ti}) where {Ti<:UMFITypes} =
    lufact(convert(SparseMatrixCSC{ComplexF64,Ti}, A))
lufact(A::Union{SparseMatrixCSC{T},SparseMatrixCSC{Complex{T}}}) where {T<:AbstractFloat} =
    throw(ArgumentError(string("matrix type ", typeof(A), "not supported. ",
    "Try lufact(convert(SparseMatrixCSC{Float64/ComplexF64,Int}, A)) for ",
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
    sym_c = umf_nm("symbolic", :ComplexF64, itype)
    num_r = umf_nm("numeric", :Float64, itype)
    num_c = umf_nm("numeric", :ComplexF64, itype)
    sol_r = umf_nm("solve", :Float64, itype)
    sol_c = umf_nm("solve", :ComplexF64, itype)
    det_r = umf_nm("get_determinant", :Float64, itype)
    det_z = umf_nm("get_determinant", :ComplexF64, itype)
    lunz_r = umf_nm("get_lunz", :Float64, itype)
    lunz_z = umf_nm("get_lunz", :ComplexF64, itype)
    get_num_r = umf_nm("get_numeric", :Float64, itype)
    get_num_z = umf_nm("get_numeric", :ComplexF64, itype)
    @eval begin
        function umfpack_symbolic!(U::UmfpackLU{Float64,$itype})
            if U.symbolic != C_NULL return U end
            tmp = Vector{Ptr{Cvoid}}(undef, 1)
            @isok ccall(($sym_r, :libumfpack), $itype,
                        ($itype, $itype, Ptr{$itype}, Ptr{$itype}, Ptr{Float64}, Ptr{Cvoid},
                         Ptr{Float64}, Ptr{Float64}),
                        U.m, U.n, U.colptr, U.rowval, U.nzval, tmp,
                        umf_ctrl, umf_info)
            U.symbolic = tmp[1]
            return U
        end
        function umfpack_symbolic!(U::UmfpackLU{ComplexF64,$itype})
            if U.symbolic != C_NULL return U end
            tmp = Vector{Ptr{Cvoid}}(undef, 1)
            @isok ccall(($sym_c, :libumfpack), $itype,
                        ($itype, $itype, Ptr{$itype}, Ptr{$itype}, Ptr{Float64}, Ptr{Float64}, Ptr{Cvoid},
                         Ptr{Float64}, Ptr{Float64}),
                        U.m, U.n, U.colptr, U.rowval, real(U.nzval), imag(U.nzval), tmp,
                        umf_ctrl, umf_info)
            U.symbolic = tmp[1]
            return U
        end
        function umfpack_numeric!(U::UmfpackLU{Float64,$itype})
            if U.numeric != C_NULL return U end
            if U.symbolic == C_NULL umfpack_symbolic!(U) end
            tmp = Vector{Ptr{Cvoid}}(undef, 1)
            status = ccall(($num_r, :libumfpack), $itype,
                           (Ptr{$itype}, Ptr{$itype}, Ptr{Float64}, Ptr{Cvoid}, Ptr{Cvoid},
                            Ptr{Float64}, Ptr{Float64}),
                           U.colptr, U.rowval, U.nzval, U.symbolic, tmp,
                           umf_ctrl, umf_info)
            if status != UMFPACK_WARNING_singular_matrix
                umferror(status)
            end
            U.numeric = tmp[1]
            return U
        end
        function umfpack_numeric!(U::UmfpackLU{ComplexF64,$itype})
            if U.numeric != C_NULL return U end
            if U.symbolic == C_NULL umfpack_symbolic!(U) end
            tmp = Vector{Ptr{Cvoid}}(undef, 1)
            status = ccall(($num_c, :libumfpack), $itype,
                           (Ptr{$itype}, Ptr{$itype}, Ptr{Float64}, Ptr{Float64}, Ptr{Cvoid}, Ptr{Cvoid},
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
                 Ptr{Float64}, Ptr{Float64}, Ptr{Cvoid}, Ptr{Float64},
                 Ptr{Float64}),
                typ, lu.colptr, lu.rowval, lu.nzval,
                x, b, lu.numeric, umf_ctrl,
                umf_info)
            return x
        end
        function solve!(x::StridedVector{ComplexF64}, lu::UmfpackLU{ComplexF64,$itype}, b::StridedVector{ComplexF64}, typ::Integer)
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
                         Ptr{Float64}, Ptr{Cvoid}, Ptr{Float64}, Ptr{Float64}),
                        typ, lu.colptr, lu.rowval, lu.nzval,
                        C_NULL, x, C_NULL, b,
                        C_NULL, lu.numeric, umf_ctrl, umf_info)
            return x
        end
        function det(lu::UmfpackLU{Float64,$itype})
            mx = Ref{Float64}()
            @isok ccall(($det_r,:libumfpack), $itype,
                           (Ptr{Float64},Ptr{Float64},Ptr{Cvoid},Ptr{Float64}),
                           mx, C_NULL, lu.numeric, umf_info)
            mx[]
        end
        function det(lu::UmfpackLU{ComplexF64,$itype})
            mx = Ref{Float64}()
            mz = Ref{Float64}()
            @isok ccall(($det_z,:libumfpack), $itype,
                        (Ptr{Float64},Ptr{Float64},Ptr{Float64},Ptr{Cvoid},Ptr{Float64}),
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
                           (Ptr{$itype},Ptr{$itype},Ptr{$itype},Ptr{$itype},Ptr{$itype},Ptr{Cvoid}),
                           lnz, unz, n_row, n_col, nz_diag, lu.numeric)
            (lnz[], unz[], n_row[], n_col[], nz_diag[])
        end
        function umf_lunz(lu::UmfpackLU{ComplexF64,$itype})
            lnz = Ref{$itype}()
            unz = Ref{$itype}()
            n_row = Ref{$itype}()
            n_col = Ref{$itype}()
            nz_diag = Ref{$itype}()
            @isok ccall(($lunz_z,:libumfpack), $itype,
                           (Ptr{$itype},Ptr{$itype},Ptr{$itype},Ptr{$itype},Ptr{$itype},Ptr{Cvoid}),
                           lnz, unz, n_row, n_col, nz_diag, lu.numeric)
            (lnz[], unz[], n_row[], n_col[], nz_diag[])
        end
        function umf_extract(lu::UmfpackLU{Float64,$itype})
            umfpack_numeric!(lu)        # ensure the numeric decomposition exists
            (lnz, unz, n_row, n_col, nz_diag) = umf_lunz(lu)
            Lp = Vector{$itype}(undef, n_row + 1)
            Lj = Vector{$itype}(undef, lnz) # L is returned in CSR (compressed sparse row) format
            Lx = Vector{Float64}(undef, lnz)
            Up = Vector{$itype}(undef, n_col + 1)
            Ui = Vector{$itype}(undef, unz)
            Ux = Vector{Float64}(undef, unz)
            P  = Vector{$itype}(undef, n_row)
            Q  = Vector{$itype}(undef, n_col)
            Rs = Vector{Float64}(undef, n_row)
            @isok ccall(($get_num_r,:libumfpack), $itype,
                        (Ptr{$itype},Ptr{$itype},Ptr{Float64},
                         Ptr{$itype},Ptr{$itype},Ptr{Float64},
                         Ptr{$itype},Ptr{$itype},Ptr{Cvoid},
                         Ref{$itype},Ptr{Float64},Ptr{Cvoid}),
                        Lp,Lj,Lx,
                        Up,Ui,Ux,
                        P, Q, C_NULL,
                        0, Rs, lu.numeric)
            (copy(transpose(SparseMatrixCSC(min(n_row, n_col), n_row, increment!(Lp), increment!(Lj), Lx))),
             SparseMatrixCSC(min(n_row, n_col), n_col, increment!(Up), increment!(Ui), Ux),
             increment!(P), increment!(Q), Rs)
        end
        function umf_extract(lu::UmfpackLU{ComplexF64,$itype})
            umfpack_numeric!(lu)        # ensure the numeric decomposition exists
            (lnz, unz, n_row, n_col, nz_diag) = umf_lunz(lu)
            Lp = Vector{$itype}(undef, n_row + 1)
            Lj = Vector{$itype}(undef, lnz) # L is returned in CSR (compressed sparse row) format
            Lx = Vector{Float64}(undef, lnz)
            Lz = Vector{Float64}(undef, lnz)
            Up = Vector{$itype}(undef, n_col + 1)
            Ui = Vector{$itype}(undef, unz)
            Ux = Vector{Float64}(undef, unz)
            Uz = Vector{Float64}(undef, unz)
            P  = Vector{$itype}(undef, n_row)
            Q  = Vector{$itype}(undef, n_col)
            Rs = Vector{Float64}(undef, n_row)
            @isok ccall(($get_num_z,:libumfpack), $itype,
                        (Ptr{$itype},Ptr{$itype},Ptr{Float64},Ptr{Float64},
                         Ptr{$itype},Ptr{$itype},Ptr{Float64},Ptr{Float64},
                         Ptr{$itype},Ptr{$itype},Ptr{Cvoid}, Ptr{Cvoid},
                         Ref{$itype},Ptr{Float64},Ptr{Cvoid}),
                        Lp,Lj,Lx,Lz,
                        Up,Ui,Ux,Uz,
                        P, Q, C_NULL, C_NULL,
                        0, Rs, lu.numeric)
            (copy(transpose(SparseMatrixCSC(min(n_row, n_col), n_row, increment!(Lp), increment!(Lj), complex.(Lx, Lz)))),
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

import LinearAlgebra.ldiv!

ldiv!(lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} =
    ldiv!(B, lu, copy(B))
ldiv!(translu::Transpose{T,<:UmfpackLU{T}}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} =
    (lu = translu.parent; ldiv!(B, transpose(lu), copy(B)))
ldiv!(adjlu::Adjoint{T,<:UmfpackLU{T}}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} =
    (lu = adjlu.parent; ldiv!(B, adjoint(lu), copy(B)))
ldiv!(lu::UmfpackLU{Float64}, B::StridedVecOrMat{<:Complex}) =
    ldiv!(B, lu, copy(B))
ldiv!(translu::Transpose{Float64,<:UmfpackLU{Float64}}, B::StridedVecOrMat{<:Complex}) =
    (lu = translu.parent; ldiv!(B, transpose(lu), copy(B)))
ldiv!(adjlu::Adjoint{Float64,<:UmfpackLU{Float64}}, B::StridedVecOrMat{<:Complex}) =
    (lu = adjlu.parent; ldiv!(B, adjoint(lu), copy(B)))

ldiv!(X::StridedVecOrMat{T}, lu::UmfpackLU{T}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} =
    _Aq_ldiv_B!(X, lu, B, UMFPACK_A)
ldiv!(X::StridedVecOrMat{T}, translu::Transpose{T,<:UmfpackLU{T}}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} =
    (lu = translu.parent; _Aq_ldiv_B!(X, lu, B, UMFPACK_Aat))
ldiv!(X::StridedVecOrMat{T}, adjlu::Adjoint{T,<:UmfpackLU{T}}, B::StridedVecOrMat{T}) where {T<:UMFVTypes} =
    (lu = adjlu.parent; _Aq_ldiv_B!(X, lu, B, UMFPACK_At))
ldiv!(X::StridedVecOrMat{Tb}, lu::UmfpackLU{Float64}, B::StridedVecOrMat{Tb}) where {Tb<:Complex} =
    _Aq_ldiv_B!(X, lu, B, UMFPACK_A)
ldiv!(X::StridedVecOrMat{Tb}, translu::Transpose{Float64,<:UmfpackLU{Float64}}, B::StridedVecOrMat{Tb}) where {Tb<:Complex} =
    (lu = translu.parent; _Aq_ldiv_B!(X, lu, B, UMFPACK_Aat))
ldiv!(X::StridedVecOrMat{Tb}, adjlu::Adjoint{Float64,<:UmfpackLU{Float64}}, B::StridedVecOrMat{Tb}) where {Tb<:Complex} =
    (lu = adjlu.parent; _Aq_ldiv_B!(X, lu, B, UMFPACK_At))

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


@inline function getproperty(lu::UmfpackLU, d::Symbol)
    if d == :L || d == :U || d == :p || d == :q || d == :Rs || d == :(:)
        # Guard the call to umf_extract behaind a branch to avoid infinite recursion
        L, U, p, q, Rs = umf_extract(lu)
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
            return (L, U, p, q, Rs)
        end
    else
        getfield(lu, d)
    end
end

for Tv in (:Float64, :ComplexF64), Ti in UmfpackIndexTypes
    f = Symbol(umf_nm("free_symbolic", Tv, Ti))
    @eval begin
        function ($f)(symb::Ptr{Cvoid})
            tmp = [symb]
            ccall(($(string(f)), :libumfpack), Cvoid, (Ptr{Cvoid},), tmp)
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
        function ($f)(num::Ptr{Cvoid})
            tmp = [num]
            ccall(($(string(f)), :libumfpack), Cvoid, (Ptr{Cvoid},), tmp)
        end
        function umfpack_free_numeric(lu::UmfpackLU{$Tv,$Ti})
            if lu.numeric == C_NULL return lu end
            ($f)(lu.numeric)
            lu.numeric = C_NULL
            return lu
        end
    end
end

function umfpack_report_symbolic(symb::Ptr{Cvoid}, level::Real)
    old_prl::Float64 = umf_ctrl[UMFPACK_PRL]
    umf_ctrl[UMFPACK_PRL] = Float64(level)
    @isok ccall((:umfpack_dl_report_symbolic, :libumfpack), Int,
                (Ptr{Cvoid}, Ptr{Float64}), symb, umf_ctrl)
    umf_ctrl[UMFPACK_PRL] = old_prl
end

umfpack_report_symbolic(symb::Ptr{Cvoid}) = umfpack_report_symbolic(symb, 4.)

function umfpack_report_symbolic(lu::UmfpackLU, level::Real)
    umfpack_report_symbolic(umfpack_symbolic!(lu).symbolic, level)
end

umfpack_report_symbolic(lu::UmfpackLU) = umfpack_report_symbolic(lu.symbolic,4.)
function umfpack_report_numeric(num::Ptr{Cvoid}, level::Real)
    old_prl::Float64 = umf_ctrl[UMFPACK_PRL]
    umf_ctrl[UMFPACK_PRL] = Float64(level)
    @isok ccall((:umfpack_dl_report_numeric, :libumfpack), Int,
                (Ptr{Cvoid}, Ptr{Float64}), num, umf_ctrl)
    umf_ctrl[UMFPACK_PRL] = old_prl
end

umfpack_report_numeric(num::Ptr{Cvoid}) = umfpack_report_numeric(num, 4.)
function umfpack_report_numeric(lu::UmfpackLU, level::Real)
    umfpack_report_numeric(umfpack_numeric!(lu).numeric, level)
end

umfpack_report_numeric(lu::UmfpackLU) = umfpack_report_numeric(lu,4.)

end # UMFPACK module
