# This file is a part of Julia. License is MIT: https://julialang.org/license

## Create an extractor that extracts the modified original matrix, e.g.
## LD for BunchKaufman, UL for CholeskyDense, LU for LUDense and
## define size methods for Factorization types using it.

##----------- Type utilities for generic Bunch-Kaufman implementation ------------
# Generic real type. Any real number type should able to approximate
# real numbers, and thus be closed under arithmetic operations.
# Therefore so Int, Complex{Int}, etc. are excluded.
ClosedReal = T where T <: Union{AbstractFloat, Rational}
# Similarly, we also use a closed scalar type
ClosedScalar = Union{T, Complex{T}} where T <: ClosedReal
##--------------------------------------------------------------------------------

"""
    BunchKaufman <: Factorization

Matrix factorization type of the Bunch-Kaufman factorization of a symmetric or
Hermitian matrix `A` as `P'UDU'P` or `P'LDL'P`, depending on whether the upper
(the default) or the lower triangle is stored in `A`. If `A` is complex symmetric
then `U'` and `L'` denote the unconjugated transposes, i.e. `transpose(U)` and
`transpose(L)`, respectively. This is the return type of [`bunchkaufman`](@ref),
the corresponding matrix factorization function.

If `S::BunchKaufman` is the factorization object, the components can be obtained
via `S.D`, `S.U` or `S.L` as appropriate given `S.uplo`, and `S.p`.

Iterating the decomposition produces the components `S.D`, `S.U` or `S.L`
as appropriate given `S.uplo`, and `S.p`.

# Examples
```jldoctest
julia> A = Float64.([1 2; 2 3])
2×2 Matrix{Float64}:
 1.0  2.0
 2.0  3.0

julia> S = bunchkaufman(A) # A gets wrapped internally by Symmetric(A)
BunchKaufman{Float64, Matrix{Float64}, Vector{Int64}}
D factor:
2×2 Tridiagonal{Float64, Vector{Float64}}:
 -0.333333  0.0
  0.0       3.0
U factor:
2×2 UnitUpperTriangular{Float64, Matrix{Float64}}:
 1.0  0.666667
  ⋅   1.0
permutation:
2-element Vector{Int64}:
 1
 2

julia> d, u, p = S; # destructuring via iteration

julia> d == S.D && u == S.U && p == S.p
true

julia> S = bunchkaufman(Symmetric(A, :L))
BunchKaufman{Float64, Matrix{Float64}, Vector{Int64}}
D factor:
2×2 Tridiagonal{Float64, Vector{Float64}}:
 3.0   0.0
 0.0  -0.333333
L factor:
2×2 UnitLowerTriangular{Float64, Matrix{Float64}}:
 1.0        ⋅
 0.666667  1.0
permutation:
2-element Vector{Int64}:
 2
 1
```
"""
struct BunchKaufman{T,S<:AbstractMatrix,P<:AbstractVector{<:Integer}} <: Factorization{T}
    LD::S
    ipiv::P
    uplo::Char
    symmetric::Bool
    rook::Bool
    info::BlasInt

    function BunchKaufman{T,S,P}(LD, ipiv, uplo, symmetric, rook, info) where {T,S<:AbstractMatrix,P<:AbstractVector}
        require_one_based_indexing(LD)
        new{T,S,P}(LD, ipiv, uplo, symmetric, rook, info)
    end
end
BunchKaufman(A::AbstractMatrix{T}, ipiv::AbstractVector{<:Integer}, uplo::AbstractChar,
             symmetric::Bool, rook::Bool, info::BlasInt) where {T} =
        BunchKaufman{T,typeof(A),typeof(ipiv)}(A, ipiv, uplo, symmetric, rook, info)
# backwards-compatible constructors (remove with Julia 2.0)
@deprecate(BunchKaufman{T,S}(LD, ipiv, uplo, symmetric, rook, info) where {T,S},
           BunchKaufman{T,S,typeof(ipiv)}(LD, ipiv, uplo, symmetric, rook, info), false)

# iteration for destructuring into components
Base.iterate(S::BunchKaufman) = (S.D, Val(:UL))
Base.iterate(S::BunchKaufman, ::Val{:UL}) = (S.uplo == 'L' ? S.L : S.U, Val(:p))
Base.iterate(S::BunchKaufman, ::Val{:p}) = (S.p, Val(:done))
Base.iterate(S::BunchKaufman, ::Val{:done}) = nothing
copy(S::BunchKaufman) = BunchKaufman(copy(S.LD), copy(S.ipiv), S.uplo, S.symmetric, S.rook, S.info)

"""
    bunchkaufman!(A, rook::Bool=false; check = true) -> BunchKaufman

`bunchkaufman!` is the same as [`bunchkaufman`](@ref), but saves space by overwriting the
input `A`, instead of creating a copy.
"""
function bunchkaufman!(A::RealHermSymComplexSym{<:BlasReal,<:StridedMatrix},
                       rook::Bool = false; check::Bool = true)
    LD, ipiv, info = rook ? LAPACK.sytrf_rook!(A.uplo, A.data) : LAPACK.sytrf!(A.uplo, A.data)
    check && checknonsingular(info)
    BunchKaufman(LD, ipiv, A.uplo, true, rook, info)
end
function bunchkaufman!(A::Hermitian{<:BlasComplex,<:StridedMatrix},
                       rook::Bool = false; check::Bool = true)
    LD, ipiv, info = rook ? LAPACK.hetrf_rook!(A.uplo, A.data) : LAPACK.hetrf!(A.uplo, A.data)
    check && checknonsingular(info)
    BunchKaufman(LD, ipiv, A.uplo, false, rook, info)
end
function bunchkaufman!(A::StridedMatrix{<:BlasFloat}, rook::Bool = false; check::Bool = true)
    if ishermitian(A)
        return bunchkaufman!(Hermitian(A), rook; check = check)
    elseif issymmetric(A)
        return bunchkaufman!(Symmetric(A), rook; check = check)
    else
        throw(ArgumentError("Bunch-Kaufman decomposition is only valid for symmetric or Hermitian matrices"))
    end
end

"""
    bunchkaufman(A, rook::Bool=false; check = true) -> S::BunchKaufman

Compute the Bunch-Kaufman [^Bunch1977] factorization of a symmetric or
Hermitian matrix `A` as `P'*U*D*U'*P` or `P'*L*D*L'*P`, depending on
which triangle is stored in `A`, and return a [`BunchKaufman`](@ref) object.
Note that if `A` is complex symmetric then `U'` and `L'` denote
the unconjugated transposes, i.e. `transpose(U)` and `transpose(L)`.

Iterating the decomposition produces the components `S.D`, `S.U` or `S.L`
as appropriate given `S.uplo`, and `S.p`.

If `rook` is `true`, rook pivoting is used. If `rook` is false,
rook pivoting is not used.

When `check = true`, an error is thrown if the decomposition fails.
When `check = false`, responsibility for checking the decomposition's
validity (via [`issuccess`](@ref)) lies with the user.

The following functions are available for `BunchKaufman` objects:
[`size`](@ref), `\\`, [`inv`](@ref), [`issymmetric`](@ref),
[`ishermitian`](@ref), [`getindex`](@ref).

[^Bunch1977]: J R Bunch and L Kaufman, Some stable methods for calculating inertia and solving symmetric linear systems, Mathematics of Computation 31:137 (1977), 163-179. [url](https://www.ams.org/journals/mcom/1977-31-137/S0025-5718-1977-0428694-0/).

# Examples
```jldoctest
julia> A = Float64.([1 2; 2 3])
2×2 Matrix{Float64}:
 1.0  2.0
 2.0  3.0

julia> S = bunchkaufman(A) # A gets wrapped internally by Symmetric(A)
BunchKaufman{Float64, Matrix{Float64}, Vector{Int64}}
D factor:
2×2 Tridiagonal{Float64, Vector{Float64}}:
 -0.333333  0.0
  0.0       3.0
U factor:
2×2 UnitUpperTriangular{Float64, Matrix{Float64}}:
 1.0  0.666667
  ⋅   1.0
permutation:
2-element Vector{Int64}:
 1
 2

julia> d, u, p = S; # destructuring via iteration

julia> d == S.D && u == S.U && p == S.p
true

julia> S.U*S.D*S.U' - S.P*A*S.P'
2×2 Matrix{Float64}:
 0.0  0.0
 0.0  0.0

julia> S = bunchkaufman(Symmetric(A, :L))
BunchKaufman{Float64, Matrix{Float64}, Vector{Int64}}
D factor:
2×2 Tridiagonal{Float64, Vector{Float64}}:
 3.0   0.0
 0.0  -0.333333
L factor:
2×2 UnitLowerTriangular{Float64, Matrix{Float64}}:
 1.0        ⋅
 0.666667  1.0
permutation:
2-element Vector{Int64}:
 2
 1

julia> S.L*S.D*S.L' - A[S.p, S.p]
2×2 Matrix{Float64}:
 0.0  0.0
 0.0  0.0
```
"""
bunchkaufman(A::AbstractMatrix{T}, rook::Bool=false; check::Bool = true) where {T} =
    bunchkaufman!(eigencopy_oftype(A, typeof(sqrt(oneunit(T)))), rook; check = check)

BunchKaufman{T}(B::BunchKaufman) where {T} =
    BunchKaufman(convert(Matrix{T}, B.LD), B.ipiv, B.uplo, B.symmetric, B.rook, B.info)
Factorization{T}(B::BunchKaufman) where {T} = BunchKaufman{T}(B)

size(B::BunchKaufman) = size(getfield(B, :LD))
size(B::BunchKaufman, d::Integer) = size(getfield(B, :LD), d)
issymmetric(B::BunchKaufman) = B.symmetric
ishermitian(B::BunchKaufman{T}) where T = T<:Real || !B.symmetric

function _ipiv2perm_bk(v::AbstractVector{T}, maxi::Integer, uplo::AbstractChar, rook::Bool) where T
    require_one_based_indexing(v)
    p = T[1:maxi;]
    uploL = uplo == 'L'
    i = uploL ? 1 : maxi
    # if uplo == 'U' we construct the permutation backwards
    @inbounds while 1 <= i <= length(v)
        vi = v[i]
        if vi > 0 # the 1x1 blocks
            p[i], p[vi] = p[vi], p[i]
            i += uploL ? 1 : -1
        else # the 2x2 blocks
            if rook
                p[i], p[-vi] = p[-vi], p[i]
            end
            if uploL
                vp = rook ? -v[i+1] : -vi
                p[i + 1], p[vp] = p[vp], p[i + 1]
                i += 2
            else # 'U'
                vp = rook ? -v[i-1] : -vi
                p[i - 1], p[vp] = p[vp], p[i - 1]
                i -= 2
            end
        end
    end
    return p
end

function getproperty(B::BunchKaufman{TS},
    d::Symbol) where TS <: ClosedScalar{TR} where TR <: ClosedReal
    n = size(B, 1)
    if d === :p
        return _ipiv2perm_bk(getfield(B, :ipiv), n, getfield(B, :uplo), B.rook)
    elseif d === :P
        return Matrix{TS}(I, n, n)[:,invperm(B.p)]
    elseif d === :L || d === :U || d === :D
        if d === :D
            _, od, md = generic_syconv(B, false)
        elseif typeof(B) <: BunchKaufman{T,<:StridedMatrix} where {T<:BlasFloat}
            # We use LAPACK whenever we can
            if getfield(B, :rook)
                LUD, _ = LAPACK.syconvf_rook!(getfield(B, :uplo), 'C',
                    copy(getfield(B, :LD)), getfield(B, :ipiv))
            else
                LUD, _ = LAPACK.syconv!(getfield(B, :uplo), copy(getfield(B, :LD)),
                    getfield(B, :ipiv))
            end
        else
            LUD, _ = generic_syconv(B)
        end
        if d === :D
            if getfield(B, :uplo) == 'L'
                odl = od[1:n - 1]
                return Tridiagonal(odl, md, getfield(B, :symmetric) ? odl : conj.(odl))
            else # 'U'
                odu = od[2:n]
                return Tridiagonal(getfield(B, :symmetric) ? odu : conj.(odu), md, odu)
            end
        elseif d === :L
            if getfield(B, :uplo) == 'L'
                return UnitLowerTriangular(LUD)
            else
                throw(ArgumentError("factorization is U*D*U' but you requested L"))
            end
        else # :U
            if B.uplo == 'U'
                return UnitUpperTriangular(LUD)
            else
                throw(ArgumentError("factorization is L*D*L' but you requested U"))
            end
        end
    else
        getfield(B, d)
    end
end

Base.propertynames(B::BunchKaufman, private::Bool=false) =
    (:p, :P, :L, :U, :D, (private ? fieldnames(typeof(B)) : ())...)

function getproperties!(B::BunchKaufman{T,<:StridedMatrix}) where {T<:BlasFloat}
    # NOTE: Unlike in the 'getproperty' function, in this function L/U and D are computed in place.
    if B.rook
        LUD, od = LAPACK.syconvf_rook!(B.uplo, 'C', B.LD, B.ipiv)
    else
        LUD, od = LAPACK.syconv!(B.uplo, B.LD, B.ipiv)
    end
    if B.uplo == 'U'
        M = UnitUpperTriangular(LUD)
        du = od[2:end]
        # Avoid aliasing dl and du.
        dl = B.symmetric ? du : conj.(du)
    else
        M = UnitLowerTriangular(LUD)
        dl = od[1:end-1]
        # Avoid aliasing dl and du.
        du = B.symmetric ? dl : conj.(dl)
    end
    return (M, Tridiagonal(dl, diag(LUD), du), B.p)
end

issuccess(B::BunchKaufman) = B.info == 0

function adjoint(B::BunchKaufman)
    if ishermitian(B)
        return B
    else
        throw(ArgumentError("adjoint not implemented for complex symmetric matrices"))
    end
end

function Base.show(io::IO, mime::MIME{Symbol("text/plain")}, B::BunchKaufman)
    if issuccess(B)
        summary(io, B); println(io)
        println(io, "D factor:")
        show(io, mime, B.D)
        println(io, "\n$(B.uplo) factor:")
        show(io, mime, B.uplo == 'L' ? B.L : B.U)
        println(io, "\npermutation:")
        show(io, mime, B.p)
    else
        print(io, "Failed factorization of type $(typeof(B))")
    end
end

function inv(B::BunchKaufman{<:BlasReal,<:StridedMatrix})
    if B.rook
        copytri!(LAPACK.sytri_rook!(B.uplo, copy(B.LD), B.ipiv), B.uplo, true)
    else
        copytri!(LAPACK.sytri!(B.uplo, copy(B.LD), B.ipiv), B.uplo, true)
    end
end

function inv(B::BunchKaufman{<:BlasComplex,<:StridedMatrix})
    if issymmetric(B)
        if B.rook
            copytri!(LAPACK.sytri_rook!(B.uplo, copy(B.LD), B.ipiv), B.uplo)
        else
            copytri!(LAPACK.sytri!(B.uplo, copy(B.LD), B.ipiv), B.uplo)
        end
    else
        if B.rook
            copytri!(LAPACK.hetri_rook!(B.uplo, copy(B.LD), B.ipiv), B.uplo, true)
        else
            copytri!(LAPACK.hetri!(B.uplo, copy(B.LD), B.ipiv), B.uplo, true)
        end
    end
end

function ldiv!(B::BunchKaufman{T,<:StridedMatrix}, R::StridedVecOrMat{T}) where {T<:BlasReal}
    if B.rook
        LAPACK.sytrs_rook!(B.uplo, B.LD, B.ipiv, R)
    else
        LAPACK.sytrs!(B.uplo, B.LD, B.ipiv, R)
    end
end
function ldiv!(B::BunchKaufman{T,<:StridedMatrix}, R::StridedVecOrMat{T}) where {T<:BlasComplex}
    if B.rook
        if issymmetric(B)
            LAPACK.sytrs_rook!(B.uplo, B.LD, B.ipiv, R)
        else
            LAPACK.hetrs_rook!(B.uplo, B.LD, B.ipiv, R)
        end
    else
        if issymmetric(B)
            LAPACK.sytrs!(B.uplo, B.LD, B.ipiv, R)
        else
            LAPACK.hetrs!(B.uplo, B.LD, B.ipiv, R)
        end
    end
end

function logabsdet(F::BunchKaufman)
    M = F.LD
    p = F.ipiv
    n = size(F.LD, 1)

    if !issuccess(F)
        return eltype(F)(-Inf), zero(eltype(F))
    end
    s = one(real(eltype(F)))
    i = 1
    abs_det = zero(real(eltype(F)))
    while i <= n
        if p[i] > 0
            elm = M[i,i]
            s *= sign(elm)
            abs_det += log(abs(elm))
            i += 1
        else
            # 2x2 pivot case. Make sure not to square before the subtraction by scaling
            # with the off-diagonal element. This is safe because the off diagonal is
            # always large for 2x2 pivots.
            if F.uplo == 'U'
                elm = M[i, i + 1]*(M[i,i]/M[i, i + 1]*M[i + 1, i + 1] -
                    (issymmetric(F) ? M[i, i + 1] : conj(M[i, i + 1])))
                s *= sign(elm)
                abs_det += log(abs(elm))
            else
                elm = M[i + 1,i]*(M[i, i]/M[i + 1, i]*M[i + 1, i + 1] -
                    (issymmetric(F) ? M[i + 1, i] : conj(M[i + 1, i])))
                s *= sign(elm)
                abs_det += log(abs(elm))
            end
            i += 2
        end
    end
    return abs_det, s
end

## reconstruct the original matrix
## TODO: understand the procedure described at
## https://www.nag.com/numeric/FL/nagdoc_fl22/pdf/F07/f07mdf.pdf


##--------------------------------------------------------------------------
##------------- Start of generic Bunch-Kaufman Implementation --------------
##--------------------------------------------------------------------------

export inertia

function arg_illegal(fun_name::AbstractString,
    info::Integer,
    waer::AbstractChar)
    if waer == 'W'
        @warn " ** On entry to '$(fun_name)' parameter number " *
            "$(info) had an illegal value"
    else
        error(" ** On entry to '$(fun_name)' parameter number " *
            "$(info) had an illegal value")
    end
end


function cabs1(z::T) where T <: Complex
    return abs(real(z)) + abs(imag(z))
end


function cabsr(z::T) where T <: Complex
    return abs(real(z))
end


"""
generic_adr1!(uplo, alpha, x, y, A, syhe) -> nothing

`generic_adr1!` performs the following adjoint (symmetric or Hermitian)
rank 1 operation

`A[1:K,1:L] = alpha*x*y' + A[1:K,1:L]`

in-place, where `alpha` is a scalar, `x` is a K element vector, `y`
is an L element vector and `A` is an `NxM` matrix. Note that `y'` can
denote either the transpose, i.e. `transpose(y)` or the conjugate
transpose , i.e. `adjoint(y)`.

`uplo` is a character, either `'U'`, `'L'` or `'F'`, indicating whether
the matrix is stored in the upper triangular part (`uplo=='U'`), the
lower triangular part (`uplo=='L'`), or the full storage space is used
(`uplo=='F'`). If `uplo!='F'` then only the corresponding triangular
part is updated. The values `'U'` or `'L'` can only be used when A is
square (`N==M`).

`syhe` is a character, either `'S'` or `'H'`, indicating whether the
symmetric adjoint (`syhe=='S'`, and `y'==transpose(y)`) or the hermitian
adjoint (`syhe=='H'`, and `y'==adjoint(y)`) must be used.
"""
function generic_adr1!(uplo::AbstractChar,
    alpha::ClosedScalar{TR},
    x::AbstractVector{TS},
    y::AbstractVector{TS},
    A::AbstractMatrix{TS},
    syhe::AbstractChar
    ) where TS <: ClosedScalar{TR} where TR <: ClosedReal

    # Inputs must be 1-indexed; bounds may not be checked.
    Base.require_one_based_indexing(x, A)

    # Check argument validity
    K = length(x)
    L = length(y)
    N, M = size(A)
    info = 0::BlasInt
    if (uplo != 'U' && uplo != 'L' && uplo != 'F') || (uplo != 'F' && N != M)
        info = (-1)::BlasInt
    elseif K > N
        info = (-3)::BlasInt
    elseif L > M
        info = (-4)::BlasInt
    elseif syhe != 'S' && syhe != 'H'
        info = (-6)::BlasInt
    end
    if info < 0
        arg_illegal("generic_sadr1!", -info, 'E')
    end

    # Load the requested adjoining operator
    adj_op = syhe == 'S' ? identity : conj

    # Define loop range function according to the type of storage
    # TODO: can we adjust the range without anonymous functions,
    # but without having to write the same code thrice?
    i_range = uplo == 'F' ? _ -> (1:K) : uplo == 'U' ? j -> (1:min(j,K)) : j -> (j:K)

    # Compute rank update of A
    for j in 1:L; @inbounds begin
        if y[j] != 0
            temp = alpha * adj_op(y[j])
            for i in i_range(j)
                A[i,j] += x[i] * temp
            end
        end
    end; end
    return
end


"""
generic_mvpv!(trans, alpha, A, x, beta, y) -> nothing

`generic_mvpv!` performs the following matrix-vector operation:

`y[1:K] = alpha*A'*x[1:L] + beta*y[1:K]`

in-place, where `alpha` and `beta` are scalars, `x` is a vector with at
least L elements, `y` is a vector with at least K elements, and `A` is
an `NxM` matrix. `A'` can denote the transpose, i.e. `transpose(A)` or
the conjugate transpose, i.e. `adjoint(A)`, and then `M==K && N==L`.
`A'` can also denote no adjoining at all, i.e. `A'==A`, and then
`N==K && M==L`.

`trans` is a character, either `'T'`, `'C'` or `'N'`, indicating whether
`A'=transpose(A)`, `A'=adjoint(A)` or `A'=A`, respectively.
"""
function generic_mvpv!(trans::AbstractChar,
    alpha::ClosedScalar{TR},
    A::AbstractMatrix{TS},
    x::AbstractVector{TS},
    beta::ClosedScalar{TR},
    y::AbstractVector{TS},
    ) where TS <: ClosedScalar{TR} where TR <: ClosedReal

    # Inputs must be 1-indexed; bounds may not be checked.
    Base.require_one_based_indexing(A, x, y)

    # Check argument validity
    M, N = size(A)
    K = trans == 'N' ? M : N
    L = trans == 'N' ? N : M
    info = 0::BlasInt
    if trans != 'T' && trans != 'C' && trans != 'N'
        info = (-1)::BlasInt
    elseif length(y) < K
        info = (-3)::BlasInt
    elseif length(x) < L
        info = (-4)::BlasInt
    end
    if info < 0
        arg_illegal("generic_sadr1!", -info, 'E')
    end

    #   Quick return if possible.
    if K == 0 || (alpha == 0 && beta == 1); return; end

    #   Start the operations. In this version the elements of A are
    #   accessed sequentially with one pass through A.
    #   First form  y := beta*y.
    @inbounds begin
        if beta != 1
            if beta == 0
                # Way less allocations and way faster for BigFloat.
                # For Float64 there is some (acceptable IMO) performance loss.
                y[1:K] .= 0
            else
                for i in 1:K; y[i] *= beta; end
            end
        end
        if alpha == 0 || L == 0; return; end

        if trans == 'N'
            #   Form  y := alpha*A*x + y.
            for j in 1:L
                # Faster than a loop
                axpy!(alpha*x[j], view(A, 1:K, j), view(y, 1:K))
            end
        else
            #   Form  y := alpha*A**T*x + y  or  y := alpha*A**H*x + y.
            noconj = (trans == 'T')
            for i = 1:K
                temp = 0
                if noconj
                    for j = 1:L
                        temp = temp + A[j,i]*x[j]
                    end
                else
                    for j = 1:L
                        temp = temp + conj(A[j,i])*x[j]
                    end
                end
                y[i] += alpha*temp
            end
        end
    end
    return
end


"""
bk_rowcol_swap!(A, k, kp, kstep, upper, herm) -> did_swap::Bool

Performs the row and column interchange of the Bunch-Kaufman factorization.
If `upper==true` then the rows and columns `kp` of `A[1:k,1:k]` are
interchanged with either rows and columns `k` or `k-1` of `A[1:k,1:k]`,
depending on whether `kstep==1` or `kstep==2`, respectively. If
`upper==false` then the rows and columns `kp-k+1` of `A[k:N,k:N]` are
interchanged with either rows and columns `1` or `2` of `A[k:N,k:N]`,
depending on whether `kstep==1` or `kstep==2`, respectively. `herm=true`
then it is assumed that `A` is Hermitian, and conjugation is applied to
the appropriate entries of the interchanged rows and columns. If
`herm=false` no conjugation is performed.

This is an internal helper function for the main Bunch-Kaufman
factorization function, `generic_bunchkaufman!`. As such, validity of the
input values is not verified.
"""
function bk_rowcol_swap!(
    A::AbstractMatrix{TS},
    k::Integer,
    kp::Integer,
    kstep::Integer,
    upper::Bool,
    herm::Bool
    ) where TS <: ClosedScalar{TR} where TR <: ClosedReal
    kk = upper ? k - kstep + 1 : k + kstep - 1
    if kp != kk
        if kp > 1
            thisview = upper ? view(A, 1:(kp-1), :) : view(A, (kp+1):size(A,1), :)
            Base.swapcols!(thisview, kp, kk)
        end
        thisrange = upper ? ((kp+1):(kk-1)) : ((kk+1):(kp-1))
        if !herm
            # Real/complex symmetric case
            for j in thisrange
                A[j,kk], A[kp,j] = A[kp,j], A[j,kk]
            end
            A[kk,kk], A[kp,kp] = A[kp,kp], A[kk,kk]
        else
            # Hermitian case
            for j in thisrange
                A[j,kk], A[kp,j] = conj(A[kp,j]), conj(A[j,kk])
            end
            A[kp,kk] = conj(A[kp,kk])
            A[kk,kk], A[kp,kp] = real(A[kp,kp]), real(A[kk,kk])
        end
        if kstep == 2
            if herm
                # Force diagonal entry to be purely real
                A[k,k] = real(A[k,k])
            end
            if upper
                A[k-1,k], A[kp,k] = A[kp,k], A[k-1,k]
            else
                A[k+1,k], A[kp,k] = A[kp,k], A[k+1,k]
            end
        end
        return true
    else
        return false
    end
end


"""
generic_bunchkaufman!(uplo, A, syhe, rook::Bool=false) ->
LD<:AbstractMatrix, ipiv<:AbstractVector{Integer}, info::BlasInt

Computes the Bunch-Kaufman factorization of a symmetric or Hermitian
matrix `A` of size `NxN` as `P'*U*D*U'*P` or `P'*L*D*L'*P`, depending on
which triangle is stored in `A`. Note that if `A` is complex symmetric
then `U'` and `L'` denote the unconjugated transposes, i.e.
`transpose(U)` and `transpose(L)`. The resulting `U` or `L` and D are
stored in-place in `A`, LAPACK style. `LD` is just a reference to `A`
(that is, `LD===A`). `ipiv` stores the permutation information of the
algorithm in LAPACK format. `info` indicates whether the factorization
was successful and non-singular when `info==0`, or else `info` takes a
different value. The outputs `LD`, `ipiv`, `info` follow the format of
the LAPACK functions of the Bunch-Kaufman factorization (`dsytrf`,
`csytrf`, `chetrf`, etc.), so this function can (ideally) be used
interchangeably with its LAPACK counterparts `LAPACK.sytrf!`,
`LAPACK.sytrf_rook!`, etc.

`uplo` is a character, either `'U'` or `'L'`, indicating whether the
matrix is stored in the upper triangular part (`uplo=='U'`) or in the
lower triangular part (`uplo=='L'`).

`syhe` is a character, either `'S'` or `'H'`, indicating whether the
matrix is real/complex symmetric (`syhe=='S'`, and the symmetric
Bunch-Kaufman factorization is performed) or complex hermitian
(`syhe=='H'`, and the hermitian Bunch-Kaufman factorization is
performed).

If `rook` is `true`, rook pivoting is used (also called bounded
Bunch-Kaufman factorization). If `rook` is `false`, rook pivoting is
not used (standard Bunch-Kaufman factorization). Rook pivoting can
require up to `~N^3/6` extra comparisons in addition to the `~N^3/3`
additions and `~N^3/3` multiplications of the standard Bunch-Kaufman
factorization. However, rook pivoting guarantees that the entries of
`U` or `L` are bounded.

This function implements the factorization algorithm entirely in
native Julia, so it supports any number type representing real or
complex numbers.
"""
function generic_bunchkaufman!(
    uplo::AbstractChar,
    A::AbstractMatrix{TS},
    syhe::AbstractChar,
    rook::Bool=false
    ) where TS <: ClosedScalar{TR} where TR <: ClosedReal

    # Inputs must be 1-indexed; bounds may not be checked.
    Base.require_one_based_indexing(A)

    # Initialize info integer as 0
    info = 0::BlasInt
    # Get size of matrix
    N, N2 = size(A)
    # Initialize permutation vector
    ipiv = Vector{BlasInt}(undef, N)

    # Check input correctness
    if uplo != 'U' && uplo != 'L'
        info = (-1)::BlasInt
    elseif N != N2
        info = (-2)::BlasInt
    elseif syhe != 'S' && syhe != 'H'
        info = (-3)::BlasInt
    end
    if info < 0
        arg_illegal("generic_bunchkaufman!", -info, 'W')
        return A, ipiv, info
    end
    # if rook
    #     error("Rook pivoting not implemented yet.")
    # end

    # Initialize `alpha` for use in choosing pivot block size.
    # The exact value is
    # (1 + sqrt(17)) / 8 ~= 0.6404
    # For rational matrices we a the small denominator approximation:
    # 16/25 = 0.64 ~= (1 + sqrt(17)) / 8
    # in order to not increase the denominator size too much in computations.
    # The error of this approximation is ≤0.1%, and it still guarantees that a
    # 2x2 block in the D factor has a positive-negative eigenvalue pair, as long
    # as the approximation lies in (0,1).
    alpha = TR <: AbstractFloat ? (1 + sqrt(TR(17))) / 8 : TR(16//25)
    # Use complex 1-norm for pivot selection, as in LAPACK
    abs1_fun = TS <: Real ? abs : cabs1

    # Check if the matrix is symmetric of hermitian
    if syhe == 'S' || (syhe == 'H' && TS <: Real)
        # Use symmetric variant if matrix is real, regardless of 'syhe' value
        syhe = 'S'
        diag_abs_fun = abs1_fun
    else
        diag_abs_fun = cabsr
    end

    # Compute machine safe minimum when working with floating point numbers.
    # LAPACK doesn't use this for diagonal pivoting though...
    if rook
        if TR <: AbstractFloat
            # eps(0) gives the smallest subnormal number, and eps(1) gives the floating
            # point type epsilon. eps(0)/eps(1) gives the smallest normal number, plus
            # possibly some rounding error.
            sfmin = nextfloat(eps(TR(0)) / eps(TR(1)), 2)
            small = 1 / prevfloat(typemax(TR), 2)
            if small >= sfmin
                # 1/sfmin may overflow, so use 'small' plus a bit as the safe minimum
                sfmin = nextfloat(small * (1 + eps(TR(1))), 2)
            end
        else
            # We're working with rationals in this case, so the all results are exact.
            sfmin = TR(0)
        end
    end

    # Run factorization depending on where the data is stored
    upper = (uplo == 'U')
    herm = (syhe == 'H')
    # TODO: Is this gonna inline properly?
    @inline k_cond = upper ? k -> k >= 1 : k -> k <= N
    @inline irange = upper ? j -> (j:-1:1) : j -> (j:N)
    @inline conj_op = herm ? conj : identity
    @inline diagreal_op = herm ? (j -> A[j,j] = TS(real(A[j,j]))) : _ -> ()
    k = upper ? N : 1
    # Main loop, comments refer to the upper triangular version of the factorization.
    # The lower triangular version is analogous.
    while k_cond(k); @inbounds begin
        kstep = 1
        knext = upper ? k - 1 : k + 1
        p = k
        #   Determine rows and columns to be interchanged and whether
        #   a 1-by-1 or 2-by-2 pivot block will be used
        absakk = diag_abs_fun(A[k,k])
        #   IMAX is the row-index of the largest off-diagonal element in
        #    column K, and COLMAX is its absolute value.
        #    Determine both COLMAX and IMAX.
        if upper && k > 1
            colmax, imax = findmax(abs1_fun, view(A, 1:(k-1), k))
        elseif (!upper) && k < N
            colmax, imax = findmax(abs1_fun, view(A, (k+1):N, k))
            imax += k
        else
            colmax = 0
        end
        if (max(absakk, colmax) == 0) || isnan(absakk)
            #   Column K is zero or underflow, or contains a NaN:
            #   set INFO and continue
            if info == 0
                info = k::BlasInt
            end
            kp = k
            if herm
                # Force diagonal entry to be purely real
                A[k,k] = real(A[k,k])
            end
        else
            if absakk >= alpha*colmax
            #   no interchange, use 1-by-1 pivot block
                kp = k
            elseif rook
                #   Loop until pivot found
                while true
                    #   Begin pivot search loop body
                    #   JMAX is the column-index of the largest off-diagonal
                    #   element in row IMAX, and ROWMAX is its absolute value.
                    #   Determine both ROWMAX and JMAX.
                    if imax != k
                        thisview = upper ? view(A, imax, (imax+1):k) :
                            view(A, imax, k:(imax-1))
                        rowmax, jmax = findmax(abs1_fun, thisview)
                        jmax += upper ? imax : k - 1
                    else
                        # LAPACK makes rowmax=0 in this case, but I believe it's
                        # better to make rowmax=-1, so that we guarantee that jmax
                        # will be define in the next if-block.
                        # TODO: is this correct/safe?
                        rowmax = 0
                    end
                    if (upper && imax > 1) || ((!upper) && imax < N)
                        # Remember that we only have the upper triangular part
                        # of the matrix. We inspect the part of the row in the
                        # lower triangular part by traversing the corresponding
                        # part of the transpose column.
                        if upper
                            stemp, itemp = findmax(abs1_fun, view(A, 1:(imax-1), imax))
                        else
                            stemp, itemp = findmax(abs1_fun, view(A, (imax+1):N, imax))
                            itemp += imax
                        end
                        if stemp > rowmax
                            rowmax = stemp
                            jmax = itemp
                        end
                    end
                    #   Equivalent to testing for (used to handle NaN and Inf)
                    #   CABS1( A( IMAX, IMAX ) ).GE.ALPHA*ROWMAX
                    if !(diag_abs_fun(A[imax,imax]) < alpha*rowmax)
                    #   interchange rows and columns K and IMAX,
                    #   use 1-by-1 pivot block
                        kp = imax
                        break
                    #   Equivalent to testing for ROWMAX .EQ. COLMAX,
                    #   used to handle NaN and Inf
                    elseif (p == jmax || rowmax <= colmax)
                    #   interchange rows and columns K+1 and IMAX,
                    #   use 2-by-2 pivot block
                        kp = imax
                        kstep = 2
                        break
                    else
                    #   Pivot NOT found, set variables and repeat
                        p = imax
                        colmax = rowmax
                        imax = jmax
                    end
                    #   End pivot search loop body
                end
            else
                #   JMAX is the column-index of the largest off-diagonal
                #   element in row IMAX, and ROWMAX is its absolute value
                # We don't really need JMAX, se we don't store it
                thisview = upper ? view(A, imax, (imax+1):k) : view(A, imax, k:(imax-1))
                rowmax = findmax(abs1_fun, thisview)[1]
                if (upper && imax > 1) || ((!upper) && imax < N)
                    # Remember that we only have the upper triangular part
                    # of the matrix. We inspect the part of the row in the
                    # lower triangular part by traversing the corresponding
                    # part of the transpose column.
                    thisview = upper ? view(A, 1:(imax-1), imax) :
                        view(A, (imax+1):N, imax)
                    rowmax = max(rowmax, findmax(abs1_fun, thisview)[1])
                end
                if absakk >= alpha * colmax * (colmax/rowmax)
                    #   no interchange, use 1-by-1 pivot block
                    kp = k
                elseif diag_abs_fun(A[imax,imax]) >= alpha * rowmax
                    #   interchange rows and columns K and IMAX, use 1-by-1
                    #   pivot block
                    kp = imax
                else
                    #   interchange rows and columns K-1 and IMAX, use 2-by-2
                    #   pivot block
                    kp = imax
                    p = imax
                    kstep = 2
                end
            end
            #   Swap TWO rows and TWO columns
            #   First swap
            # The first swap only needs to be done when using rook pivoting
            if rook && kstep == 2
                #   Interchange rows and column K and P in the leading
                #   submatrix A(1:k,1:k) if we have a 2-by-2 pivot
                bk_rowcol_swap!(A, k, p, 1, upper, herm)
            end
            #   Second swap
            did_swap = bk_rowcol_swap!(A, k, kp, kstep, upper, herm)
            if herm && (!did_swap)
                # Force diagonal entries to be purely real
                A[k,k] = real(A[k,k])
                if kstep == 2
                    A[knext,knext] = real(A[knext,knext])
                end
            end
            if kstep == 1
                #   1-by-1 pivot block D(k): column k now holds
                #   W(k) = U(k)*D(k)
                #   where U(k) is the k-th column of U
                #   When rook=false, sfmin is not defined, but the short-circuit
                #   evaluation of the conditional avoids an error.
                if (!rook) || absakk >= sfmin
                    #   Perform a rank-1 update of A(1:k-1,1:k-1) as
                    #   A := A - U(k)*D(k)*U(k)' = A - W(k)*1/D(k)*W(k)'
                    # Compute 1/D(k)
                    r1 = !herm ? 1 / A[k,k] : 1 / real(A[k,k])
                    # Perform rank-1 update to store the Schur complement
                    # in a submatrix of A
                    x = upper ? view(A, 1:(k-1), k) : view(A, (k+1):N, k)
                    # if 'upper' this should assign by reference
                    thisview = upper ? A : view(A, (k+1):N, (k+1):N)
                    generic_adr1!(uplo, -r1, x, x, thisview, syhe)
                    #   Store U(k) in column k
                    thisrange = upper ? (1:(k-1)) : ((k+1):N)
                    for i in thisrange
                        A[i,k] *= r1
                    end
                else
                    # Compute D(k)
                    r1 = !herm ? A[k,k] : real(A[k,k])
                    #   Store U(k) in column k
                    thisrange = upper ? (1:(k-1)) : ((k+1):N)
                    for i in thisrange
                        A[i,k] /= r1
                    end
                    #   Perform a rank-1 update of A(k+1:n,k+1:n) as
                    #   A := A - U(k)*D(k)*U(k)**T
                    #      = A - W(k)*(1/D(k))*W(k)**T
                    #      = A - (W(k)/D(k))*(D(k))*(W(k)/D(K))**T
                    # Perform rank-1 update to store the Schur complement
                    # in a submatrix of A
                    x = upper ? view(A, 1:(k-1), k) : view(A, (k+1):N, k)
                    # if 'upper' this should assign by reference
                    thisview = upper ? A : view(A, (k+1):N, (k+1):N)
                    generic_adr1!(uplo, -r1, x, x, thisview, syhe)
                end
            elseif (upper && k > 2) || ((!upper) && k < N - 1)
                #   2-by-2 pivot block D(k): columns k and k-1 now hold
                #   ( W(k-1) W(k) ) = ( U(k-1) U(k) )*D(k)
                #   where U(k) and U(k-1) are the k-th and (k-1)-th columns
                #   of U
                #   Perform a rank-2 update of A(1:k-2,1:k-2) as
                #   A := A - ( U(k-1) U(k) )*D(k)*( U(k-1) U(k) )'
                #   = A - ( W(k-1) W(k) )*inv(D(k))*( W(k-1) W(k) )'
                thisrange = upper ? ((k-2):-1:1) : ((k+2):N)
                if !herm
                    # Real/complex symmetric case
                    #TODO: is this way to compute the inverse backward stable?
                    # (it probably is as it comes from LAPACK)
                    dxk = A[knext,k]
                    dxx = A[knext,knext] / dxk
                    dkk = A[k,k] / dxk
                    t = 1 / (dkk * dxx - 1)
                    dxk = t / dxk
                    dkx = dxk
                else
                    # Hermitian case
                    # TODO: is this way to compute the inverse backward stable?
                    # (it probably is as it is a small modification of LAPACK's
                    # method)
                    dxk = A[knext,k]
                    dxx = real(A[knext,knext]) / dxk
                    dkk = real(A[k,k]) / conj(dxk)
                    t = 1 / (real(dkk * dxx) - 1)
                    dkx = t / conj(dxk)
                    dxk = t / dxk
                end
                for j in thisrange
                    wknext = dxk * (dkk*A[j,knext] - A[j,k])
                    wk = dkx * (dxx*A[j,k] - A[j,knext])
                    for i in irange(j)
                        A[i,j] -= (A[i,k]*conj_op(wk) + A[i,knext]*conj_op(wknext))
                    end
                    A[j,k] = wk
                    A[j,knext] = wknext
                    # Force diagonal entry to be purely real, but still of
                    # complex type TS (I don't know why in LAPACK this
                    # case, unlike the rest, enforces a complex type
                    # explicitly).
                    diagreal_op(j)
                end
            end
        end
        #   Store details of the interchanges in IPIV
        if kstep == 1
            ipiv[k] = kp
        else
            ipiv[k] = -p
            ipiv[knext] = -kp
        end
        #   Decrease K and return to the start of the main loop
        # k -= upper ? kstep : -kstep
        if upper; k -= kstep; else; k += kstep; end
    end; end
    return A, ipiv, info
end


"""
generic_syconv(F, gettri::Bool=true) ->
(TLU<:Union{AbstractMatrix,Nothing}, e<:AbstractVector,
    d<:Union{AbstractVector,Nothing})

`generic_syconv` takes the Bunch-Kaufman object `F` and returns the
block-diagonal factor `D`, and the triangular factor `L` (or `U`) if
requested. If the `L` or `U` factor is requested then both `L` (or `U`) and
the main diagonal of `D` will be stored in `TLU`, following LAPACK format,
and `d` will be set to `nothing`. `e` contains the first subdiagonal of
`D`. If the triangular factor is not requested, then `TLU` will not be set
to `nothing`, and the main diagonal of `D` will be stored in `d`.

`gettri` is a `Bool`, indicating whether the `L` (or `U`) triangular factor
should be computed (`gettri==true`) or not (`gettri==false`). If the
triangular factor is required, a copy of `A.LD` will be created, and the
triangular factor will be computed in-place in said copy.
"""
function generic_syconv(
    F::BunchKaufman{TS},
    gettri::Bool=true
    ) where TS <: ClosedScalar{TR} where TR <: ClosedReal

    # Inputs must be 1-indexed; bounds may not be checked.
    Base.require_one_based_indexing(F.LD, F.ipiv)

    # Extract necessary variables
    A, ipiv, rook = gettri ? deepcopy(F.LD) : F.LD, F.ipiv, F.rook

    # Get size of matrix
    N = size(A)[1]

    # Initialize off-diagonal and diagonal vector
    e = Vector{TS}(undef, N)
    d = gettri ? nothing : diag(A, 0)

    #   Quick return if possible
    if N == 0; return gettri ? A : nothing, e, d; end

    # Main loops
    upper = (F.uplo == 'U')
    @inline icond_d = upper ? i -> i > 1 : i -> i < N
    @inline icond_T = upper ? i -> i >= 1 : i -> i <= N
    @inline inext = upper ? i -> i - 1 : i -> i + 1
    #   Convert VALUE
    i = upper ? N : 1
    e[N+1-i] = 0
    while icond_d(i); @inbounds begin
        if ipiv[i] < 0
            ix = inext(i)
            e[i] = A[ix,i]
            e[ix] = 0
            if gettri; A[ix,i] = 0; end
            if upper; i -= 1; else; i += 1; end
        else
            e[i] = 0
        end
        if upper; i -= 1; else; i += 1; end
    end; end
    #   Convert PERMUTATIONS
    if gettri
        i = upper ? N : 1
        while icond_T(i); @inbounds begin
            thisview = upper ? view(A, :, (i+1):N) : view(A, :, 1:(i-1))
            ip = ipiv[i]
            if ip > 0 || rook
                Base.swaprows!(thisview, abs(ip), i)
            end
            if ip <= 0
                ix = inext(i)
                Base.swaprows!(thisview, -ipiv[ix], ix)
                if upper; i -= 1; else; i += 1; end
            end
            if upper; i -= 1; else; i += 1; end
        end; end
    end
    return gettri ? A : nothing, e, d
end


"""
generic_bksolve!(F, B) -> X<:AbstractVecOrMat

`generic_bksolve!` solves a system of linear equations `A*X = B` where
the Bunch-Kaufman factorization of `A` is provided by `F`.
"""
function generic_bksolve!(
    F::BunchKaufman{TS},
    B0::AbstractVecOrMat{TS},
    ) where TS <: ClosedScalar{TR} where TR <: ClosedReal

    # Inputs must be 1-indexed; bounds may not be checked.
    Base.require_one_based_indexing(F.LD, F.ipiv, B0)

    # Get size of matrices
    N = size(F.LD)[1]
    if typeof(B0) <: AbstractVector
        N3 = size(B0)[1]
        M = 1
        B = view(B0, :, :)
    else
        N3, M = size(B0)
        B = B0
    end

    # Initialize info integer as 0
    info = 0::BlasInt

    # Check input correctness
    if N3 != N
        info = (-2)::BlasInt
    end
    if info < 0
        arg_illegal("generic_bksolve!", -info, 'E')
    end

    #   Quick return if possible
    if N == 0 || M == 0; return B; end

    # Extract necessary variables
    A, ipiv, symm, rook = F.LD, F.ipiv, issymmetric(F), F.rook

    # Load the requested adjoining operator
    adj_op = symm ? identity : conj

    R1 = TR(1)
    upper = (F.uplo == 'U')
    @inline kcond1 = upper ? k -> k >= 1 : k -> k <= N
    @inline kcond2 = upper ? k -> k <= N : k -> k >= 1
    @inline knext = upper ? k -> k - 1 : k -> k + 1
    @inline knext2 = upper ? k -> k + 1 : k -> k - 1
    k = upper ? N : 1
    while kcond1(k); @inbounds begin
        kp = ipiv[k]
        if kp > 0
            #   1 x 1 diagonal block
            #   Interchange rows K and IPIV(K).
            Base.swaprows!(B, k, kp)
            #   Multiply by inv(U(K)), where U(K) is the transformation
            #   stored in column K of A.
            Aview = upper ? view(A, 1:(k-1), k) : view(A, (k+1):N, k)
            Bview = upper ? B : view(B, (k+1):N, :)
            generic_adr1!('F', -R1, Aview, view(B, k, :), Bview, 'S')
            #   Multiply by the inverse of the diagonal block.
            s = symm ? 1 / A[k,k] : 1 / real(A[k,k])
            for j in 1:M; B[k,j] *= s; end
            if upper; k -= 1; else; k += 1; end
        else
            #   2 x 2 diagonal block
            #   Interchange rows K and -IPIV(K) THEN K-1 and -IPIV(K-1)
            # The first interchange is only needed when rook pivoting is used
            if rook; Base.swaprows!(B, k, -kp); end
            kx = knext(k)
            Base.swaprows!(B, kx, -ipiv[kx])
            #   Multiply by inv(U(K)), where U(K) is the transformation
            #   stored in columns K-1 and K of A.
            Aview = upper ? view(A, 1:(k-2), k) : view(A, (k+2):N, k)
            Bview = upper ? B : view(B, (k+2):N, :)
            generic_adr1!('F', -R1, Aview, view(B, k, :), Bview, 'S')
            Aview = upper ? view(A, 1:(k-2), kx) : view(A, (k+2):N, kx)
            generic_adr1!('F', -R1, Aview, view(B, kx, :), Bview, 'S')
            #   Multiply by the inverse of the diagonal block.
            axk = A[kx,k]
            axx = A[kx,kx] / axk
            akk = A[k,k] / adj_op(axk)
            denom = axx*akk - 1
            for j in 1:M
                bx = B[kx,j] / axk
                bk = B[k,j] / adj_op(axk)
                B[kx,j] = (akk*bx - bk) / denom
                B[k,j] = (axx*bk - bx) / denom
            end
            if upper; k -= 2; else; k += 2; end
        end
    end; end
    #   Next solve U'*X = B, overwriting B with X.
    #   K is the main loop index, increasing from 1 to N in steps of
    #   1 or 2, depending on the size of the diagonal blocks.
    k = upper ? 1 : N
    while kcond2(k); @inbounds begin
        Aview = upper ? view(A, 1:(k-1), k) : view(A, (k+1):N, k)
        Bview = upper ? view(B, 1:(k-1), :) : view(B, (k+1):N, :)
        B_row = view(B, k, :)
        kp = ipiv[k]
        if kp > 0
            #   1 x 1 diagonal block
            #   Multiply by inv(U**T(K)), where U(K) is the transformation
            #   stored in column K of A.
            if symm
                generic_mvpv!('T', -R1, Bview, Aview, R1, B_row)
            else
                conj!(B_row)
                generic_mvpv!('C', -R1, Bview, Aview, R1, B_row)
                conj!(B_row)
            end
            #   Interchange rows K and IPIV(K).
            Base.swaprows!(B, k, kp)
            if upper; k += 1; else; k -= 1; end
        else
            #   2 x 2 diagonal block
            #   Multiply by inv(U**T(K+1)), where U(K+1) is the transformation
            #   stored in columns K and K+1 of A.
            kx = knext2(k)
            if symm
                generic_mvpv!('T', -R1, Bview, Aview, R1, B_row)
                Aview = upper ? view(A, 1:(k-1), kx) : view(A, (k+1):N, kx)
                B_row = view(B, kx, :)
                generic_mvpv!('T', -R1, Bview, Aview, R1, B_row)
            elseif k > 1
                conj!(B_row)
                generic_mvpv!('C', -R1, Bview, Aview, R1, B_row)
                conj!(B_row)
                Aview = upper ? view(A, 1:(k-1), kx) : view(A, (k+1):N, kx)
                B_row = view(B, kx, :)
                conj!(B_row)
                generic_mvpv!('C', -R1, Bview, Aview, R1, B_row)
                conj!(B_row)
            end
            #   Interchange rows K and -IPIV(K) THEN K+1 and -IPIV(K+1).
            # The second interchange is only needed when rook pivoting is used
            Base.swaprows!(B, k, -kp)
            if rook; Base.swaprows!(B, kx, -ipiv[kx]); end
            if upper; k += 2; else; k -= 2; end
        end
    end; end
    return B
end


"""
inertia(B::BunchKaufman; atol::Real=0, rtol::Real=atol>0 ? 0 : n*ϵ) ->
    np::Union{Nothing,Integer}, nn::Union{Nothing,Integer}, nz::Integer

`inertia` computes the numerical inertia (the number of positive,
negative and zero eigenvalues, given by `np`, `nn` and `nz`,
respectively) of a real symmetric of Hermitian matrix `B` that has been
factored using the Bunch-Kaufman algorithm. For complex symmetric
matrices the inertia is not defined. in that case `np` and `nn` are set
to `nothing`, but the function still returns the number of zero
eigenvalues. The inertia is computed by counting the eigenvalues signs
of `B.D`. The number of zero eigenvalues is computed as the number of
estimated eigenvalues with complex 1-norm (defined as `|re(.)|+|im(.)|`)
less or equal than `max(atol, rtol*s₁)`, where `s₁` is an upper bound of
the largest singular value of `B.D`, `σ₁` (more specifically,
`0.5*s₁ <= σ₁ <= s₁` for real matrices and `0.35*s₁ <= σ₁ <= s₁` for
complex matrices). `atol` and `rtol` are the absolute and relative
tolerances, respectively. The default relative tolerance is `n*ϵ`, where
`n` is the size of  of `A`, and `ϵ` is the [`eps`](@ref) of the number
type of `A`, if this type is a subtype of `AbstractFloat`. In any other
case (if the number type of `A` is `Rational`, for example) `ϵ` is set
to `0`.

!!! note
    Numerical inertia can be a sensitive and imprecise characterization of
    ill-conditioned matrices with eigenvalues that are close in magnitude to the
    threshold tolerance `max(atol, rtol*s₁)`. In such cases, slight perturbations
    to the Bunch-Kaufman computation or to the matrix can change the result of
    `rank` by pushing one or more eigenvalues across the threshold. These
    variations can even occur due to changes in floating-point errors between
    different Julia versions, architectures, compilers, or operating systems.
    In particular, the size of the entries of the tringular factor directly
    influende the scale of the eigenvalues of the diagonal factor, so it is
    strongly recommended to use rook pivoting is the inertia is going to be
    computed.
    On the other hand, if the matrix has rational entries, the inertia
    computation is guaranteed is to be exact, as long as there is no
    under/overflow in the underlying integer type (and in such cases Julia itself
    throws an error), or a positive tolerance (absolute or relative) is
    specified.
"""
function inertia(B::BunchKaufman{TS};
    atol::TR = TR(0),
    rtol::TR = TR(0)
    ) where TS <: ClosedScalar{TR} where TR <: ClosedReal

    # Check if matrix is complex symmetric
    get_inertia = !(issymmetric(B) && TS <: Complex)

    # Initialize outputs
    np, nn, nz = get_inertia ? (0, 0, 0) : (nothing, nothing, 0)

    # Compute matrix size
    N = size(B, 1)

    # Quick return if possible
    if N == 0; return np, nn, nz; end

    # Compute default relative tolerance
    if rtol <= 0 && atol <= 0
        rtol = TR <: AbstractFloat ? (N * eps(TR)) : TR(0)
    end

    # We use the complex 1-norm for complex matrices
    real_matrix = (TS <: Real)
    abs1_fun = real_matrix ? abs : cabs1
    real_fun = real_matrix ? identity : real

    # Check if we must track the largest singular value
    get_s1 = (rtol > 0)

    # Constant for lower bound estimation of the smallest eigenvalue in 2x2 blocks.
    # The best (largest) value for complex matrices is 1/sqrt(2), but for rational
    # matrices we use the small denominator approximation 12/17, in order to not
    # increase the denominator size too much in computations. The error of this
    # approximation is ≤0.2%, and we still get a valid lower bound.
    c = real_matrix ? TR(1) : (TR <: AbstractFloat ? 1/sqrt(TR(2)) : TR(12//17))

    # First pass, estimate largest singular value and group together size-1 blocks
    D = B.D
    s1 = TR(0)
    i = 1
    while i <= N; @inbounds begin
        if i < N && D[i,i+1] != 0
            # 2x2 block
            # The largest singular value of a 2x2 matrix is between [1, 2] times
            # its complex max-norm, which is between [c, 1] times the largest
            # complex 1-norm among the entries of the 2x2 matrix. See "Roger
            # Horn and Charles Johnson. Matrix Analysis, 2nd Edition, 5.6.P23".
            abs_Dii = abs1_fun(D[i,i])
            abs_Dxx = abs1_fun(D[i+1,i+1])
            s1_block = 2 * max(abs_Dii, abs1_fun(D[i,i+1]), abs_Dxx)
            if get_s1; s1 = max(s1, s1_block); end
            # Lower bound on the smallest eigenvalue complex 2-norm is
            # abs(λ₂) ≥ abs(det(block)) / s1_block
            # so the bound in terms of the complex 1-norm becomes
            # abs1_fun(λ₂) ≥ c * abs1_fun(det(block)) / s1_block
            # For rational matrices, if λ₂=0 then det(block)=0 and then the bound
            # becomes zero too. If λ₁=0 too then the block has all zero entries
            # and 's1_block'=0, but 'D[i,i+1]' != 0 and so 's1_block' > 0. However, we
            # may still have that 'smin_block'≈0, then the value of 'smin_block' may not
            # be accurate. In that case the counting routine will detect that both
            # eigenvalues are zero without using 'smin_block', so it doesn't matter.
            # TODO: is this the most numerically stable way to compute the determinant?
            # TODO: is this the best way to avoid under/overflow?
            if abs_Dii >= abs_Dxx
                smin_block = c * abs1_fun((D[i,i]/s1_block)*D[i+1,i+1] -
                    (D[i,i+1]/s1_block)*D[i+1,i])
            else
                smin_block = c * abs1_fun(D[i,i]*(D[i+1,i+1]/s1_block) -
                    (D[i,i+1]/s1_block)*D[i+1,i])
            end
            # Store lower bound in-place in the lower off-diagonal and upper bound
            # in-place in the upper off-diagonal. The trace is stored in the first
            # diagonal entry block, but only if the full inertia is needed.
            D[i,i+1] = s1_block
            D[i+1,i] = smin_block
            if get_inertia; D[i,i] += D[i+1,i+1]; end
            i += 2
        else
            # 1x1 block
            if get_s1; s1 = max(s1, abs1_fun(D[i,i])); end
            i += 1
        end
    end; end

    # Second pass, count eigenvalue signs
    tol = max(atol, rtol * s1)
    i = 1
    while i <= N; @inbounds begin
        if i < N && D[i,i+1] != 0
            # 2x2 block. For the counting of zero eigenvalues we use the lower bound on the
            # eigenvalues' magnitude. This way, if an eigenvalue is deemed non-zero, then
            # it is guaranteed that its magnitude is greater than the tolerance.
            s1_block = real_fun(D[i,i+1])
            if (c / 2) * s1_block <= tol
                # Lower bound of largest eigenvalue is smaller than the tolerance,
                # we consider the both eigenvalues of this block to be zero.
                nz += 2
                i += 2
                continue
            end
            # Reaching this part of the lopp implies that 's1_block' != 0.
            smin_block = real_fun(D[i+1,i])
            trace_block = real_fun(D[i,i])
            if smin_block > tol || trace_block == 0
                # If first condition holds then the lower bound of the smallest eigenvalue
                # is larger than the tolerance. If the second condition holds then the trace
                # is exactly zero, so both eigenvalues have the same magnitude, and we
                # already know that the largest one is non-zero. In any case we conclude
                # that both eigenvalues are non-zero.
                if get_inertia
                    # The eigenvalues of a 2x2 block are guaranteed to be a
                    # positive-negative pair.
                    np += 1
                    nn += 1
                end
            else
                # The lower bound of smallest eigenvalue is smaller than the tolerance and
                # the trace is non-zero, so we consider the smallest eigenvalues of this
                # block to be zero.
                nz += 1
                if get_inertia
                    # The trace is non-zero, and its sign is the same of the largest
                    # eigenvalue.
                    if trace_block >= 0
                        np += 1
                    else
                        nn += 1
                    end
                end
            end
            i += 2
        else
            # 1x1 block
            if get_inertia
                eig = real_fun(D[i,i])
                if eig > tol
                    np += 1
                elseif eig < -tol
                    nn += 1
                else
                    nz += 1
                end
            elseif abs1_fun(D[i,i]) <= tol
                nz += 1
            end
            i += 1
        end
    end; end

    return np, nn, nz
end


"""
    bunchkaufman_native!(A, rook::Bool=false; check = true) -> BunchKaufman

`bunchkaufman_native!` is the same as [`bunchkaufman!`](@ref), but it performs
the factorization in native Julia code instead of calling LAPACK.
"""
function bunchkaufman_native!(A::AbstractMatrix{TS},
    rook::Bool = false;
    check::Bool = true,
    ) where TS <: ClosedScalar{TR} where TR <: ClosedReal
    if A isa RealHermSymComplexSym{TR}
        syhe = 'S'
    elseif ishermitian(A)
        syhe = 'H'
    elseif issymmetric(A)
        syhe = 'S'
    else
        throw(ArgumentError("Bunch-Kaufman decomposition is only valid for " *
            "symmetric or Hermitian matrices"))
    end
    if A isa HermOrSym
        Adata = A.data
        uplo = A.uplo
    else
        Adata = A
        uplo = 'U'
    end
    LD, ipiv, info = generic_bunchkaufman!(uplo, Adata, syhe, rook)
    check && checknonsingular(info)
    return BunchKaufman(LD, ipiv, uplo, syhe == 'S', rook, info)
end


"""
Overload 'bunchkaufman.jl' methods through multiple dispatch
"""

function bunchkaufman!(A::AbstractMatrix{TS},
    rook::Bool = false;
    check::Bool = true
    ) where TS <: ClosedScalar{TR} where TR <: ClosedReal
    return bunchkaufman_native!(A, rook; check)
end

function bunchkaufman(A::AbstractMatrix{TS},
    rook::Bool = false;
    check::Bool = true
    ) where TS <: ClosedScalar{TR} where TR <: ClosedReal
    return bunchkaufman!(eigencopy_oftype(A, TS), rook; check)
end

function bunchkaufman(A::AbstractMatrix{TS},
    rook::Bool = false;
    check::Bool = true
    ) where TS <:Union{TI, Complex{TI}} where TI <: Integer

    # Identity whether matrix is symmetric or Hermitian or none
    if A isa Symmetric
        TA = Symmetric
    elseif A isa Hermitian
        TA = Hermitian
    else
        TA = Nothing
    end

    # Create a rational copy of input integer matrix, as the Bunch-Kaufman
    # algorithm is closed over the rationals but not over the integers.
    # We promote input to BigInt to avoid overflow problems
    if TA == Nothing
        if TS <: Integer
            M = Rational{BigInt}.(eigencopy_oftype(A, TS))
        else
            M = Complex{Rational{BigInt}}.(eigencopy_oftype(A, TS))
        end
    else
        if TS <: Integer
            M = TA(Rational{BigInt}.(eigencopy_oftype(A, TS)), Symbol(A.uplo))
        else
            M = TA(Complex{Rational{BigInt}}.(eigencopy_oftype(A, TS)),
                Symbol(A.uplo))
        end
    end

    return bunchkaufman_native!(M, rook; check)
end

function ldiv!(B::BunchKaufman{TS},
    R::AbstractVecOrMat{TS}
    ) where TS <: ClosedScalar{TR} where TR <: ClosedReal
    return generic_bksolve!(B, R)
end

function inv(B::BunchKaufman{TS}) where TS <: ClosedScalar{TR} where TR <: ClosedReal
    # I don't think there's value in implementing tha LAPACK in-place inverse
    # functions `dsytri`, `chetri`, etc., unless of course an efficient
    # in-place inverse function `inv!` is needed.
    # TODO: reduce the operation count of the inverse by not computing the
    # lower/upper triangular part.
    if issymmetric(B)
        return copytri!(B \ I, B.uplo)
    else
        return copytri!(B \ I, B.uplo, true)
    end
end
